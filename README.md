prr — (almost-)brr-compatible JavaScript FFI library sans browser-only APIs
-------------------------------------------------------------------------------

[![GitHub CI][action-badge]][action-link]

Prr is a fork of [Brr][brr], which provides a subset of its APIs:

* `Jstr`: JavaScript strings.
* `Jv`: JavaScript values.
* `Fut`: Future values.
* `Brr.*`: Various utils on JavaScript globals, but with some omissions when compared to brr:
  | Included? | Module(s) | Comments |
  |--|--|--|
  | ❌ | `Ev`, `At`, `El` (DOM related) | |
  | ❌ | `Abort` | |
  | ❌ | `Window` | |
  | ❌ | `Navigator` | |
  | ❌ | `Performance` | |
  | ❌ | `File` | |
  | 🟡 | `G` | most are included, except globals regarding the above omitted modules and utils related to animated timing |
  | ✅ | Tarray | |
  | ✅ | Blob | |
  | ✅ | Base64 | |
  | ✅ | Json | |
  | ✅ | Uri | |
  | ✅ | Console | |

Prr focuses on providing Brr's JavaScript FFI facilities and selected utils modules;
and is intended to be used in Web / Node.js / React Native development.

Prr is distributed under the ISC license. It depends on the [`js_of_ocaml`][jsoo]
compiler and runtime – but not on its libraries or syntax extension.

[brr]:  https://erratique.ch/software/brr
[jsoo]: https://ocsigen.org/js_of_ocaml

Please also see Brr's official homepage: https://erratique.ch/software/brr

[action-badge]: https://github.com/kxcdev/prr/workflows/CI/badge.svg?branch=main
[action-link]: https://github.com/kxcdev/prr/actions?query=workflow:"CI"

## Installation

### With OPAM

Prr is available on opam: https://opam.ocaml.org/packages/prr/

To install prr with opam, simply run `opam install prr`.

You can also install a specific branch or commit with `opam pin add prr https://github.com/kxcdev/prr.git#GIT_REF`.
See https://opam.ocaml.org/doc/Usage.html#opam-pin for more about this feature of opam.

### Vendor with dune

Add this repository as a git submodule, then use the
[vendoring](https://dune.readthedocs.io/en/stable/dune-files.html#vendored-dirs-since-1-11)
feature of `dune`.

## Documentation

Please see Brr's [official documentation][doc].

[doc]: https://erratique.ch/software/brr/doc
