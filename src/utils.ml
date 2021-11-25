open Ppxlib
open Ast_builder.Default

let dummy ~loc str =
  pexp_constant ~loc (Pconst_string (str, Location.none, None))

type kind = Spec | Gen | Printer

let kind_to_string = function
  | Spec -> "_spec_"
  | Gen -> "_gen_"
  | Printer -> "_printer_"

let symbol name kind = "__monolith_" ^ name ^ kind_to_string kind
let pat ~loc name kind = pvar ~loc (symbol name kind)
let var ~loc name kind = evar ~loc (symbol name kind)
