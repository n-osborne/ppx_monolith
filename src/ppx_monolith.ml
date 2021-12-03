open Ppxlib
open Ppxlib.Deriving
open Utils

let derive ~loc ~path:_ (_rec_flag, type_decls) =
  (* For each type declaration -- simple or mutually recursive --
     we build the printers, the generators and then the monolith `spec`.
     If Monolith does not provide an already defined `spec` (e.g. for user
     defined types), we build one using the printers and generators we've
     just defined. *)
  let open Ast_builder.Default in
  let bind kind td =
    (* The name of the new function is defined according to its kind
       (printer, generator or spec) and the name of the type.
       We rely a lot on this naming convention *)
    let pat = pat ~loc td.ptype_name.txt kind in

    let expr =
      (* We call the appropriate builder *)
      match kind with
      | Spec ->
          let name = td.ptype_name.txt in
          Specs.derive ~loc ~name td
      | Gen -> Generators.derive ~loc td
      | Printer -> Printers.derive ~loc td
    in
    value_binding ~loc ~pat ~expr
  in

  let flag = if List.length type_decls > 1 then Recursive else Nonrecursive in

  [
    pstr_value ~loc flag (List.map (bind Printer) type_decls);
    pstr_value ~loc flag (List.map (bind Gen) type_decls);
    (* No matter if the definition is mutually recursive, Monolith `spec` does
       not need to be. *)
    pstr_value ~loc Nonrecursive (List.map (bind Spec) type_decls);
  ]

let ppx_deriving_monolith =
  let str_type_decl_spec = Generator.make_noarg derive in
  add ~str_type_decl:str_type_decl_spec "monolith"
