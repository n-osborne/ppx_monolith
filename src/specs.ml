open Ppxlib
open Generators
open Printers
open Utils

let spec ~loc ~name =
  (* A monolith `spec` is either constructlible (which needs a generator and a printer)
     or deconstructible (which only needs a printer)
     In order to build a `spec`, we rely on the already defined generators and printers
     and on the naming convention. *)
  let gen = var ~loc name Gen in
  let printer = var ~loc name Printer in
  [%expr
    Monolith.ifpol
      (Monolith.easily_constructible [%e gen] [%e printer])
      (Monolith.deconstructible [%e printer])]

let spec_kind ~loc ~name (tk : type_kind) =
  match tk with
  | Ptype_variant _ -> spec ~loc ~name
  | Ptype_record _ -> spec ~loc ~name
  | Ptype_open -> Raise.Unsupported.typekind ~loc "Ptype_open"
  | Ptype_abstract -> Raise.Unsupported.typekind ~loc "Ptype_abstract"

let rec spec_core_type ~loc (ct : core_type) =
  match ct.ptyp_desc with
  | Ptyp_any -> Raise.Unsupported.coretype ~loc "wildcard"
  | Ptyp_var _ -> [%expr Monolith.sequential ()]
  | Ptyp_arrow (_, _, _) -> Raise.Unsupported.coretype ~loc "arrow"
  | Ptyp_tuple tys -> spec_tuple ~loc tys
  | Ptyp_constr ({ txt; _ }, args) -> spec_longident ~loc txt args
  | Ptyp_object (_, _) -> Raise.Unsupported.coretype ~loc "object"
  | Ptyp_class (_, _) -> Raise.Unsupported.coretype ~loc "class"
  | Ptyp_alias (_, _) -> Raise.Unsupported.coretype ~loc "alias"
  | Ptyp_variant (_, _, _) -> Raise.Unsupported.coretype ~loc "variant"
  | Ptyp_poly (_, _) -> Raise.Unsupported.coretype ~loc "poly"
  | Ptyp_package _ -> Raise.Unsupported.coretype ~loc "package"
  | Ptyp_extension _ -> Raise.Unsupported.coretype ~loc "extension"

and spec_longident ~loc txt args =
  match txt with
  | Lident "bool" -> [%expr Monolith.bool]
  | Lident "char" -> [%expr Monolith.char]
  | Lident "unit" -> [%expr Monolith.unit]
  | Lident "int" -> [%expr Monolith.closed_interval Int.min_int Int.max_int]
  | Lident "string" as lid ->
      let gen = gen_longident ~loc lid args in
      let printer = [%expr Monolith.Print.string] in
      [%expr
        Monolith.ifpol
          (Monolith.easily_constructible [%e gen] [%e printer])
          (Monolith.deconstructible [%e printer])]
  | Lident "array" as lid ->
      assert (List.length args = 1);
      let gen = gen_longident ~loc lid args in
      let printer = printer_longident ~loc lid args in
      [%expr
        Monolith.ifpol
          (Monolith.easily_constructible [%e gen] [%e printer])
          (Monolith.deconstructible [%e printer])]
  | Lident "list" ->
      assert (List.length args = 1);
      let param = spec_core_type ~loc (List.hd args) in
      [%expr Monolith.list [%e param]]
  | Lident "option" ->
      assert (List.length args = 1);
      let param = spec_core_type ~loc (List.hd args) in
      [%expr Monolith.option [%e param]]
  | Lident "result" ->
      assert (List.length args = 2);
      let ok = spec_core_type ~loc (List.hd args) in
      let err = spec_core_type ~loc (List.nth args 1) in
      [%expr Monolith.result [%e ok] [%e err]]
  | Lident id -> var ~loc id Spec
  | Ldot (_, _) -> Raise.Unsupported.longident ~loc "Ldot"
  | Lapply (_, _) -> Raise.Unsupported.longident ~loc "Lapply"

and spec_tuple ~loc tys =
  let gen = gen_tuple ~loc tys in
  let printer = printer_tuple ~loc tys in
  [%expr
    Monolith.ifpol
      (Monolith.easily_constructible [%e gen] [%e printer])
      (Monolith.deconstructible [%e printer])]

let spec_expr ~loc ~name (type_decl : type_declaration) =
  (* If a type declaration has a manifest, then this is a core type.
     Otherwise, will inspect its kind. *)
  match type_decl.ptype_manifest with
  | None -> spec_kind ~loc ~name type_decl.ptype_kind
  | Some m -> spec_core_type ~loc m

let monolith_spec ~loc ~path:_ (_rec_flag, type_decls) =
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
          spec_expr ~loc ~name td
      | Gen -> gen_expr ~loc td
      | Printer -> printer_expr ~loc td
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
