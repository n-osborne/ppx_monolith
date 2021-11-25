open Ppxlib
open Generators
open Printers
open Utils

let spec ~loc gen printer =
  [%expr
    let gen = [%e gen] in
    let printer = [%e printer] in
    Monolith.ifpol
      (Monolith.easily_constructible gen printer)
      (Monolith.deconstructible printer)]

let spec_variant ~loc cds =
  let gen = gen_variant ~loc cds in
  let printer = printer_variant ~loc cds in
  spec ~loc gen printer

let spec_record ~loc ldl =
  let gen = gen_record ~loc ldl in
  let printer = printer_record ~loc ldl in
  spec ~loc gen printer

let spec_kind ~loc (tk : type_kind) =
  match tk with
  | Ptype_variant cds -> spec_variant ~loc cds
  | Ptype_record ldl -> spec_record ~loc ldl
  | Ptype_open -> dummy ~loc "spec_kind Ptype_open"
  | Ptype_abstract -> dummy ~loc "spec_kind Ptype_abstract"

let rec spec_core_type ~loc (ct : core_type) =
  match ct.ptyp_desc with
  | Ptyp_any -> dummy ~loc "spec_core_type Ptyp_any"
  | Ptyp_var _ -> dummy ~loc "spec_core_type Ptyp_var"
  | Ptyp_arrow (_, _, _) -> dummy ~loc "spec_core_type Ptyp_arrow"
  | Ptyp_tuple tys -> spec_tuple ~loc tys
  | Ptyp_constr ({ txt; _ }, args) -> spec_longident ~loc txt args
  | Ptyp_object (_, _) -> dummy ~loc "spec_core_type Ptyp_object"
  | Ptyp_class (_, _) -> dummy ~loc "spec_core_type Ptyp_class"
  | Ptyp_alias (_, _) -> dummy ~loc "spec_core_type Ptyp_alias"
  | Ptyp_variant (_, _, _) -> dummy ~loc "spec_core_type Ptyp_variant"
  | Ptyp_poly (_, _) -> dummy ~loc "spec_core_type Ptyp_poly"
  | Ptyp_package _ -> dummy ~loc "spec_core_type Ptpy_package"
  | Ptyp_extension _ -> dummy ~loc "spec_core_type Ptyp_extension"

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
  | Ldot (_, _) -> dummy ~loc "spec_longident Ldot"
  | Lapply (_, _) -> dummy ~loc "spec_longident Lapply"

and spec_tuple ~loc tys =
  let gen = gen_tuple ~loc tys in
  let printer = printer_tuple ~loc tys in
  spec ~loc gen printer

let spec_expr ~loc (type_decl : type_declaration) =
  Option.fold
    ~none:(spec_kind ~loc type_decl.ptype_kind)
    ~some:(spec_core_type ~loc) type_decl.ptype_manifest

let monolith_spec ~loc ~path:_ (_rec_flag, type_decls) =
  let type_decl td =
    let pat = pat ~loc td.ptype_name.txt in
    [
      [%stri let [%p pat Printer] = [%e printer_expr ~loc td]];
      [%stri let [%p pat Gen] = [%e gen_expr ~loc td]];
      [%stri let [%p pat Spec] = [%e spec_expr ~loc td]];
    ]
  in
  match type_decls with
  | [] -> assert false
  | [ td ] -> type_decl td
  | _tds ->
      (* don't know what to do yet *)
      assert false
