open Ppxlib
open Generators
open Printers
open Utils

let spec ~loc gen printer =
  [%expr
    Monolith.ifpol
      (Monolith.easily_constructible [%e gen] [%e printer])
      (Monolith.deconstructible [%e printer])]

let spec_variant ~loc ~name =
  let gen = var ~loc name Gen in
  let printer = var ~loc name Printer in
  spec ~loc gen printer

let spec_record ~loc ~name =
  let gen = var ~loc name Gen in
  let printer = var ~loc name Printer in
  spec ~loc gen printer

let spec_kind ~loc ~name (tk : type_kind) =
  match tk with
  | Ptype_variant _ -> spec_variant ~loc ~name
  | Ptype_record _ -> spec_record ~loc ~name
  | Ptype_open -> Raise.Unsupported.typekind ~loc "Ptype_open"
  | Ptype_abstract -> Raise.Unsupported.typekind ~loc "Ptype_abstract"

let rec spec_core_type ~loc (ct : core_type) =
  match ct.ptyp_desc with
  | Ptyp_any -> Raise.Unsupported.coretype ~loc "wildcard"
  | Ptyp_var _ -> Raise.Unsupported.coretype ~loc "alpha"
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
      spec ~loc gen printer
  | Lident "array" as lid ->
      assert (List.length args = 1);
      let gen = gen_longident ~loc lid args in
      let printer = printer_longident ~loc lid args in
      spec ~loc gen printer
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
  spec ~loc gen printer

let spec_expr ~loc ~name (type_decl : type_declaration) =
  match type_decl.ptype_manifest with
  | None -> spec_kind ~loc ~name type_decl.ptype_kind
  | Some m -> spec_core_type ~loc m

let monolith_spec ~loc ~path:_ (_rec_flag, type_decls) =
  match type_decls with
  | [] -> assert false
  | [ td ] ->
      let make kind f td =
        [%stri let [%p pat ~loc td.ptype_name.txt kind] = [%e f ~loc td]]
      in
      let printer = make Printer printer_expr in
      let gen = make Gen gen_expr in
      let spec =
        make Spec (fun ~loc x -> spec_expr ~loc ~name:x.ptype_name.txt x)
      in
      [ printer td; gen td; spec td ]
  | tds ->
      let open Ast_builder.Default in
      let bind kind f td =
        value_binding ~loc
          ~pat:(pat ~loc td.ptype_name.txt kind)
          ~expr:(f ~loc td)
      in
      let printer = bind Printer printer_expr in
      let gen = bind Gen gen_expr in
      let spec =
        bind Spec (fun ~loc x -> spec_expr ~loc ~name:x.ptype_name.txt x)
      in
      [
        pstr_value ~loc Recursive (List.map printer tds);
        pstr_value ~loc Recursive (List.map gen tds);
        (* Monolith `spec` doesn't need to be mutually recursive,
           only generators and printers *)
        pstr_value ~loc Nonrecursive (List.map spec tds);
      ]
