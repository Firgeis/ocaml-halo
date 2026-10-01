(lang dune 3.15)
(name ocaml-halo)
(generate_opam_files true)

(package
 (name ocaml-halo)
 (authors "Ezequiel Lewin <firgeis@gmail.com>")
 (maintainers "Ezequiel Lewin <firgeis@gmail.com>")
 (license MIT)
 (synopsis "Fast OCaml HTTP 1.1 server")
 (description "Fast Ocaml HTTP 1.1 server with C runtime for performance")
 (homepage "https://github.com/firge/ocaml-halo")
 (bug_reports "https://github.com/firge/ocaml-halo/issues")
 (source (github firge/ocaml-halo))
 (depends
  (conf-libssl :build)
  (conf-zlib :build)
  (conf-pkg-config :build)
  (conf-cmake :build)
  eio
  uri
  (eio_main :with-test)
  (alcotest :with-test)
  (yojson :with-test)))
