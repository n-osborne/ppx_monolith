open Ppxlib
open Ast_builder.Default

type kind = Spec | Gen | Printer

let kind_to_string = function
  | Spec -> "_spec_"
  | Gen -> "_gen_"
  | Printer -> "_printer_"

let symbol name kind = "__monolith_" ^ kind_to_string kind ^ name
let pat ~loc name kind = pvar ~loc (symbol name kind)
let var ~loc name kind = evar ~loc (symbol name kind)

let lident ~loc lid kind =
  let rec aux = function
    | Lident name -> Lident (symbol name kind)
    | Ldot (path, name) -> Ldot (path, symbol name kind)
    | Lapply (lid0, lid1) -> Lapply (lid0, aux lid1)
  in
  pexp_ident ~loc { txt = aux lid; loc }
