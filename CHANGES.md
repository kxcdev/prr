## prr v0.1.0 -- (planned) first release of prr
- first release of the [fork](https://github.com/kxcdev/prr)

## The forking
- switch to dune (3.0)
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

## Before the fork (from brr)
See [<fork-point>/CHANGES.md](https://github.com/kxcdev/prr/blob/c64b00bbe043526e602906e9b9ce12fd4a40da20/CHANGES.md).

