## Next version of prr
- catch-up with upstream at
  https://github.com/dbuenzli/brr/commit/f3813f02272901e23cd0af364934b192700c3d84
  - the only relevant parts are docstr updates in src/brr.ml
    (see [the diff](https://github.com/dbuenzli/brr/commit/f3813f02272901e23cd0af364934b192700c3d84#diff-70a6e64ad8ca24f310db82f833a2259027adbfeae0e5678f183bb437b08fe27f))

## Version 0.1.1 of prr (tag: [0.1.1](https://github.com/kxcdev/prr/releases/tag/0.1.1))
- attempt to fix opam CI reported issue
  - change to (lang dune 2.7) in dune-project

## Version 0.1.0 of prr (tag: [0.1.0](https://github.com/kxcdev/prr/releases/tag/0.1.0))
- first release of this [fork](https://github.com/kxcdev/prr)
- this release uses (lang dune 2.0) in dune-project

## The forking
- switch to dune (~3.0~ 2.0)
- naming the fork `prr`, as a "lighter" version of `brr`
- keeping only the following from the original brr:
  - `Jstr`: JavaScript strings.
  - `Jv`: JavaScript values.
  - `Fut`: Future values.
  - `Brr.*`: Various utils on JavaScript globals, but with some omissions when compared to brr:
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

## Before the forking (from [dbuenzli/brr](https://github.com/dbuenzli/brr))
See [	&#60;fork-point&#62;/CHANGES.md](https://github.com/kxcdev/prr/blob/c64b00bbe043526e602906e9b9ce12fd4a40da20/CHANGES.md).

