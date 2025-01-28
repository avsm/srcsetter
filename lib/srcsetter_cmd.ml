open Eio

type ('a, 'b) config = {
  dummy : bool;
  preserve : bool;
  proc_mgr : 'a Eio.Process.mgr;
  src_dir : 'b Path.t;
  dst_dir : 'b Path.t;
  img_widths : int list;
  img_exts : string list;
  idx_file : string;
  max_fibers : int;
}

let rec file_seq ~filter path =
  Path.with_open_dir path Path.read_dir
  |> List.fold_left
       (fun (dirs, files) f ->
         let fp = Path.(path / f) in
         match Path.kind ~follow:false fp with
         | `Regular_file when filter f -> (dirs, fp :: files)
         | `Directory -> (f :: dirs, files)
         | _ -> (dirs, files))
       ([], [])
  |> fun (dirs, files) ->
  Seq.append (List.to_seq files)
    (Seq.flat_map
       (fun f -> file_seq ~filter Path.(path / f))
       (List.to_seq dirs))

let iter_seq_p ?max_fibers fn seq =
  Eio.Switch.run ~name:"iter_seq_p" @@ fun sw ->
  match max_fibers with
  | None -> Seq.iter (fun v -> Fiber.fork ~sw @@ fun () -> fn v) seq
  | Some mf when mf <= 0 -> invalid_arg "iter_seq_p max_fibers"
  | Some mf ->
      let s = Semaphore.make mf in
      Seq.iter
        (fun v ->
          Semaphore.acquire s;
          Fiber.fork ~sw @@ fun () ->
          Fun.protect ~finally:(fun () -> Semaphore.release s) @@ fun () -> fn v)
        seq

let relativize_path dir path =
  let dir = Path.native_exn dir in
  let path = Path.native_exn path in
  match Fpath.(rem_prefix (v dir) (v path)) with
  | None -> failwith "bad path prefix"
  | Some v -> Fpath.to_string v

let dims { proc_mgr; _ } fl =
  let fl = Path.native_exn fl in
  let args = [ "identify"; "-ping"; "-format"; "%w %h"; fl ] in
  let l = Process.parse_out proc_mgr Buf_read.take_all args in
  Scanf.sscanf l "%d %d" (fun w h -> (w, h))

let run { dummy; proc_mgr; _ } args =
  if not dummy then Process.run proc_mgr args

let convert ({ src_dir; dst_dir; dummy; _ } as cfg) (src, dst, size) =
  if dummy then () (* TODO log skip *)
  else
    let dir =
      if Filename.dirname dst = "." then dst_dir
      else Path.(dst_dir / Filename.dirname dst)
    in
    Path.(mkdirs ~exists_ok:true ~perm:0o755 dir);
    let src = Path.(native_exn (src_dir / src)) in
    let dst = Path.(native_exn (dst_dir / dst)) in
    let sz = Printf.sprintf "%dx" size in
    let args =
      [
        "magick";
        src;
        "-auto-orient";
        "-thumbnail";
        sz;
        "-quality";
        "100";
        "-gravity";
        "center";
        "-extent";
        sz;
        dst;
      ]
    in
    run cfg args

let convert_pdf cfg ~size ~dst ~src =
  let src = Path.native_exn src in
  let dst = Path.native_exn dst in
  let sz = Printf.sprintf "%sx" size in
  let args =
    [
      "magick";
      "-density";
      "300";
      "-quality";
      "100";
      src ^ "[0]";
      "-gravity";
      "North";
      "-crop";
      "100%x50%+0+0";
      "-resize";
      sz;
      dst;
    ]
  in
  run cfg args

let needed_sizes ~img_widths ~w = List.filter (fun tw -> tw <= w) img_widths

let translate { src_dir; dst_dir; preserve; _ } ?w src =
  let src_file = relativize_path src_dir src in
  let dst_file =
    Printf.sprintf "%s%s.webp"
      (Filename.chop_extension src_file)
      (match w with None -> "" | Some w -> "." ^ string_of_int w)
  in
  let dst = Path.(dst_dir / dst_file) in
  match (preserve, Path.is_file dst) with
  | true, true -> (src_file, dst_file, w, false)
  | _, false -> (src_file, dst_file, w, true)
  | false, true -> (src_file, dst_file, w, true)

let calc_needed { src_dir; dst_dir; preserve; _ } ~img_widths ~w src =
  let ent_of_dst fname tw =
    let dst = Path.(dst_dir / fname) in
    let ent = (src, dst, tw) in
    match (preserve, Path.is_file dst) with
    | true, true -> `Exists ent
    | _, false -> `Todo ent
    | false, true -> `Todo ent
  in
  let file = relativize_path src_dir src in
  let base =
    let fname = Printf.sprintf "%s.webp" (Filename.chop_extension file) in
    ent_of_dst fname w
  in
  let variants =
    List.filter_map
      (fun tw ->
        if tw <= w then
          let fname =
            Printf.sprintf "%s.%d.webp" (Filename.chop_extension file) tw
          in
          Some (ent_of_dst fname tw)
        else None)
      img_widths
  in
  (base, variants)

let main_bar total =
  let style =
    let open Progress.Line.Bar_style in
    let open Progress.Color in
    let bars = ("|", "|") in
    v ~delims:bars ~color:(hex "#FFBA08") [ "█"; "▓"; "▒"; "░"; " " ]
  in
  let open Progress.Line in
  list [ bar ~style:(`Custom style) total; ticker_to total ]

let main_bar_heading head total =
  let open Progress.Multi in
  line (Progress.Line.const head) ++ line (main_bar total) ++ blank

let one_bar total =
  let style =
    let open Progress.Line.Bar_style in
    let open Progress.Color in
    v ~delims:("{", "}") ~color:(ansi `blue) [ "="; ">"; " " ]
  in
  let open Progress.Line in
  let a =
    list
      [
        spinner ();
        bar ~style:(`Custom style) ~width:(`Fixed 12) total;
        const " ";
      ]
  in
  let b = string in
  pair a b

let process_file cfg (display, main_rep) src =
  let w, h = dims cfg src in
  let needed_w = needed_sizes ~img_widths:cfg.img_widths ~w in
  let ((base_src, base_dst, _, _) as base) = translate cfg src in
  let needed = List.map (fun w -> translate cfg ~w src) needed_w in
  let variants =
    List.map (fun (_, dst, _, _) -> (dst, (0, 0))) needed
    |> Srcsetter.MS.of_list
  in
  let slug = Filename.basename base_dst |> Filename.chop_extension in
  (* TODO avsm check for clashing slugs *)
  let ent = Srcsetter.v base_dst slug base_src variants (w, h) in
  let todo =
    List.filter_map
      (fun (src, dst, sz, n) ->
        let sz = match sz with None -> w | Some w -> w in
        if n then Some (src, dst, sz) else None)
      (base :: needed)
  in
  if List.length todo > 3 then (
    let l = one_bar (List.length todo) in
    let r = Progress.Display.add_line display l in
    let fin = ref [] in
    let rep sz =
      if sz > 0 then fin := sz :: !fin;
      let la = String.concat "," @@ List.map string_of_int !fin in
      let flb =
        Filename.basename (Path.native_exn src) |> Filename.chop_extension
      in
      let trim_string str max_length =
        if String.length str <= max_length then str
        else if max_length <= 3 then String.sub "..." 0 max_length
        else
          let trimmed_length = max_length - 3 in
          let prefix = String.sub str 0 trimmed_length in
          prefix ^ "..."
      in
      let label = Printf.sprintf "%25s -> %s" (trim_string flb 25) la in
      Progress.Reporter.report r (1, label)
    in
    rep 0;
    List.iter
      (fun ((_, _, sz) as a) ->
        rep sz;
        convert cfg a)
      todo;
    main_rep 1;
    Progress.Display.remove_line display r)
  else (
    List.iter (fun a -> convert cfg a) todo;
    main_rep 1);
  ent
