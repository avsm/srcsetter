(* Copyright (c) 2024, Anil Madhavapeddy <anil@recoil.org>

  Permission to use, copy, modify, and/or distribute this software for
  any purpose with or without fee is hereby granted, provided that the
  above copyright notice and this permission notice appear in all
  copies.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
  WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
  AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
  DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
  OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
  TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
  PERFORMANCE OF THIS SOFTWARE.
  *)

module SC = Srcsetter_cmd

let min_interval = Some (Mtime.Span.of_uint64_ns 1000L)

let stage1 { SC.img_exts; src_dir; _ } =
  let filter f = List.exists (Filename.check_suffix ("." ^ f)) img_exts in
  let fs = SC.file_seq ~filter src_dir in
  let total = Seq.length fs in
  Format.printf "[1/3] Scanned %d images from %a.\n%!" total Eio.Path.pp src_dir;
  fs

let stage2 ({ SC.max_fibers; dst_dir; _ } as cfg) fs =
  let display =
    Progress.Display.start
      ~config:(Progress.Config.v ~persistent:false ~min_interval ())
      (SC.main_bar_heading "[2/3] Processing images..." (Seq.length fs))
  in
  let [ _; main_rep ] = Progress.Display.reporters display in
  let ents = ref [] in
  SC.iter_seq_p ~max_fibers
    (fun src ->
      let ent = SC.process_file cfg (display, main_rep) src in
      ents := ent :: !ents)
    fs;
  Progress.Display.finalise display;
  Format.printf "[2/3] Processed %d images to %a.\n%!" (List.length !ents)
    Eio.Path.pp dst_dir;
  !ents

let stage3 ({ SC.dst_dir; max_fibers; _ } as cfg) ents =
  let ents_seq = List.to_seq ents in
  let oents = ref [] in
  let display =
    Progress.Display.start
      ~config:(Progress.Config.v ~persistent:false ~min_interval ())
      (SC.main_bar_heading "[3/3] Verifying images..." (List.length ents))
  in
  let [ _; rep ] = Progress.Display.reporters display in
  SC.iter_seq_p ~max_fibers
    (fun ent ->
      let w, h = SC.dims cfg Eio.Path.(dst_dir / Srcsetter.name ent) in
      let variants =
        Srcsetter.MS.bindings ent.variants
        |> List.map (fun (k, _) -> (k, SC.dims cfg Eio.Path.(dst_dir / k)))
        |> Srcsetter.MS.of_list
      in
      rep 1;
      oents := { ent with Srcsetter.dims = (w, h); variants } :: !oents)
    ents_seq;
  Progress.Display.finalise display;
  Printf.printf "[3/3] Verified %d generated image sizes.\n%!"
    (List.length ents);
  !oents

let _ =
  (* TODO cmdliner *)
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun _ ->
  let path_env p =
    if String.starts_with ~prefix:"/" p then Eio.(Path.(Stdenv.fs env / p))
    else Eio.(Path.(Stdenv.cwd env / p))
  in
  let src_dir = path_env "bushel/images" in
  let dst_dir = path_env "site/images" in
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let idx_file = "index.json" in
  let img_widths =
    [ 320; 480; 640; 768; 1024; 1280; 1440; 1600; 1920; 2560; 3840 ]
  in
  let img_exts = [ "png"; "webp"; "jpeg"; "jpg"; "bmp"; "heic"; "gif" ] in
  let img_widths = List.sort (fun a b -> compare b a) img_widths in
  let max_fibers = 8 in
  let cfg =
    {
      Srcsetter_cmd.dummy = false;
      preserve = true;
      proc_mgr;
      src_dir;
      dst_dir;
      idx_file;
      img_widths;
      img_exts;
      max_fibers;
    }
  in
  let fs = stage1 cfg in
  let ents = stage2 cfg fs in
  let oents = stage3 cfg ents in
  let j = Srcsetter.list_to_json oents |> Result.get_ok in
  let idx = Eio.Path.(dst_dir / idx_file) in
  Eio.Path.save ~append:false ~create:(`Or_truncate 0o644) idx j
