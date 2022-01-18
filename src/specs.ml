open Ppxlib

module Core_type = struct
  let rec derive ~loc (ct : core_type) =
    match ct.ptyp_desc with
    | Ptyp_any -> Raise.Unsupported.coretype ~loc "wildcard"
    | Ptyp_var _ -> [%expr Monolith.sequential ()]
    | Ptyp_arrow (_, _, _) -> Raise.Unsupported.coretype ~loc "arrow"
    | Ptyp_tuple tys -> spec_tuple ~loc tys
    | Ptyp_constr ({ txt; _ }, args) -> longident ~loc txt args
    | Ptyp_object (_, _) -> Raise.Unsupported.coretype ~loc "object"
    | Ptyp_class (_, _) -> Raise.Unsupported.coretype ~loc "class"
    | Ptyp_alias (_, _) -> Raise.Unsupported.coretype ~loc "alias"
    | Ptyp_variant (_, _, _) -> Raise.Unsupported.coretype ~loc "variant"
    | Ptyp_poly (_, _) -> Raise.Unsupported.coretype ~loc "poly"
    | Ptyp_package _ -> Raise.Unsupported.coretype ~loc "package"
    | Ptyp_extension _ -> Raise.Unsupported.coretype ~loc "extension"

  and longident ~loc txt args =
    match txt with
    | Lident "bool" -> [%expr Monolith.bool]
    | Lident "char" -> [%expr Monolith.char]
    | Lident "unit" -> [%expr Monolith.unit]
    | Lident "int" -> [%expr Monolith.closed_interval Int.min_int Int.max_int]
    | Lident "string" as lid ->
        let gen = Generators.Core_type.longident ~loc lid args in
        let printer = [%expr Monolith.Print.string] in
        [%expr
          Monolith.ifpol
            (Monolith.easily_constructible [%e gen] [%e printer])
            (Monolith.deconstructible [%e printer])]
    | Lident "array" as lid ->
        assert (List.length args = 1);
        let gen = Generators.Core_type.longident ~loc lid args in
        let printer = Printers.Core_type.longident ~loc lid args in
        [%expr
          Monolith.ifpol
            (Monolith.easily_constructible [%e gen] [%e printer])
            (Monolith.deconstructible [%e printer])]
    | Lident "list" ->
        assert (List.length args = 1);
        let param = derive ~loc (List.hd args) in
        [%expr Monolith.list [%e param]]
    | Lident "option" ->
        assert (List.length args = 1);
        let param = derive ~loc (List.hd args) in
        [%expr Monolith.option [%e param]]
    | Lident "result" ->
        assert (List.length args = 2);
        let ok = derive ~loc (List.hd args) in
        let err = derive ~loc (List.nth args 1) in
        [%expr Monolith.result [%e ok] [%e err]]
    (* In the following cases (Lident _, Ldot (_,_) and Lapply (_,_)),
       we rely on the fact that the `spec` is already defined *)
    | lid -> Utils.lident ~loc lid Spec

  and spec_tuple ~loc tys =
    let gen = Generators.Core_type.tuple ~loc tys in
    let printer = Printers.Core_type.tuple ~loc tys in
    [%expr
      Monolith.ifpol
        (Monolith.easily_constructible [%e gen] [%e printer])
        (Monolith.deconstructible [%e printer])]
end

let ifpol ~loc ~name =
  (* A monolith `spec` is either constructlible (which needs a generator and a printer)
     or deconstructible (which only needs a printer)
     In order to build a `spec`, we rely on the already defined generators and printers
     and on the naming convention. *)
  let gen = Utils.var ~loc name Gen in
  let printer = Utils.var ~loc name Printer in
  [%expr
    Monolith.ifpol
      (Monolith.easily_constructible [%e gen] [%e printer])
      (Monolith.deconstructible [%e printer])]

let derive ~loc ~name (type_decl : type_declaration) =
  (* If a type declaration has a manifest, then this is a core type.
     Otherwise, will inspect its kind. *)
  match type_decl.ptype_manifest with
  | None -> (
      match type_decl.ptype_kind with
      (* For both variants and records, printers and generators has already be defined *)
      | Ptype_variant _ -> ifpol ~loc ~name
      | Ptype_record _ -> ifpol ~loc ~name
      | Ptype_open -> Raise.Unsupported.typekind ~loc "Ptype_open"
      | Ptype_abstract -> Raise.Unsupported.typekind ~loc "Ptype_abstract")
  | Some m -> Core_type.derive ~loc m
