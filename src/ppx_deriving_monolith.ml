open Ppxlib.Deriving
open Specs

let ppx_deriving_monolith =
  let str_type_decl_spec = Generator.make_noarg monolith_spec in
  add ~str_type_decl:str_type_decl_spec "monolith"
