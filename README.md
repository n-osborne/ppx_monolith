<div align="center">
<h1>Ppx_monolith</h1>
<strong>A ppx-deriver for Monolith</strong>
</div>

<div align="center">
<br />

[![license](https://img.shields.io/github/license/n-osborne/ppx_monolith.svg?style=flat-square)](LICENSE)
[![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Fn-osborne%2Fppx_monolith%2Fmain&logo=ocaml&style=flat-square)](https://ci.ocamllabs.io/github/n-osborne/ppx_monolith)

</div>

`ppx_monolith` is a [ppx_deriving](https://github.com/ocaml-ppx/ppx_deriving) plugin
for [monolith](https://gitlab.inria.fr/fpottier/monolith) model based test framework.

## Example

```
type t = A of int | B of string [@@deriving monolith]
```

will automatically generate the `(t, t) Monolith.spec` type necessary to write a 
`monolith` test suite involving the type `t`.
