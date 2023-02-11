(lang dune 2.7)
(name prr)
(version 0.1.1)

(license ISC)
(maintainers "Haochen Xie <hx@kxc.inc>" "KXC Members <hello@kxc.inc>")
(authors "The brr programmers" "KXC Members and Contributors")

(source
 (github kxcdev/prr))
(bug_reports "https://github.com/kxcdev/prr/issues")

(generate_opam_files true)
(package
 (name prr)
 (synopsis "A fork of brr, sans browser-only APIs")
 (description "A fork of brr, sans browser-only APIs; see https://github.com/kxcdev/prr and https://github.com/dbuenzli/brr for details")
 (depends
  (ocaml (>= 4.08.0))
  (js_of_ocaml (>= 4.0.0))
  (js_of_ocaml-compiler (>= 4.0.0))))
