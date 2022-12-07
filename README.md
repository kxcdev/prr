prr — (almost-)brr-compatible JavaScript FFI library sans browser-only APIs
-------------------------------------------------------------------------------

Prr is a fork of [Brr][brr], which provides a subset of its APIs:

* `Jstr`: JavaScript strings.
* `Jv`: JavaScript values.
* `Fut`: Future values.
* `Brr.*`: Various utils on JavaScript globals, with the following omissions when compared to brr:
  - DOM related globals their utils (e.g. `Ev`, `At`, `El` modules)
  - `Abort` module
  - `Window` module
  - `Navigator` module
  - `Performance` module
  - animation timing related utils

Prr focuses on providing Brr's JavaScript FFI APIs only, and intended to run
both in the browser and in node.js.

Prr is distributed under the ISC license. It depends on the [`js_of_ocaml`][jsoo]
compiler and runtime – but not on its libraries or syntax extension.

[brr]:  https://erratique.ch/software/brr
[jsoo]: https://ocsigen.org/js_of_ocaml

Please also see Brr's official homepage: https://erratique.ch/software/brr

## Installation

Add this repository as a git submodule, then use the
[vendoring](https://dune.readthedocs.io/en/stable/dune-files.html#vendored-dirs-since-1-11)
feature of `dune`.

Prr may also be available on `opam` in future:

    opam install prr # not for now

## Documentation

Please see Brr's [official documentation][doc].

[doc]: https://erratique.ch/software/brr/doc
