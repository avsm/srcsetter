(lang dune 3.17)
(name srcsetter)

(generate_opam_files true)

(source (github avsm/srcsetter))
(license ISC)
(authors "Anil Madhavapeddy")
(maintainers "anil@recoil.org")

(package
 (name srcsetter)
 (description "Srcsetter is a library that allows for handling a directory of responsive images suitable for embedding as `<img srcset` tags in a website. It uses the ImageMagick CLI tool to handle the actual processing of images, and the `srcsetter-cmd` package to generate the input to this library.")
 (synopsis "Image srcset library for webp images")
 (depends jsont bytesrw pro))

(package
 (name srcsetter-cmd)
 (synopsis "Image processing tool to generate responsive images")
 (description "Srcsetter is a CLI tool that processes a directory of images and outputs a directory of responsive images suitable for embedding as `<img srcset` tags in a website. It uses the ImageMagick CLI tool to handle the actual processing of images.")
 (depends srcsetter fpath progress eio))
