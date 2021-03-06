open Ppxlib
open Ast_builder.Default

module Core_type = struct
  let rec derive ~loc (ct : core_type) =
    match ct.ptyp_desc with
    | Ptyp_any -> Raise.Unsupported.coretype ~loc "wildcard"
    | Ptyp_var _ -> [%expr Monolith.Gen.sequential]
    | Ptyp_arrow (_, _, _) -> Raise.Unsupported.coretype ~loc "arrow"
    | Ptyp_tuple tys -> tuple ~loc tys
    | Ptyp_constr ({ txt; _ }, args) -> longident ~loc txt args
    | Ptyp_object (_, _) -> Raise.Unsupported.coretype ~loc "object"
    | Ptyp_class (_, _) -> Raise.Unsupported.coretype ~loc "class"
    | Ptyp_alias (_, _) -> Raise.Unsupported.coretype ~loc "alias"
    | Ptyp_variant (_, _, _) -> Raise.Unsupported.coretype ~loc "variant"
    | Ptyp_poly (_, _) -> Raise.Unsupported.coretype ~loc "poly"
    | Ptyp_package _ -> Raise.Unsupported.coretype ~loc "package"
    | Ptyp_extension _ -> Raise.Unsupported.coretype ~loc "extension"

  and tuple ~loc tys =
    let gen =
      pexp_tuple ~loc
        (List.map (fun t -> Utils.trigger ~loc (derive ~loc t)) tys)
    in
    Utils.suspend ~loc gen

  and longident ~loc txt args =
    match txt with
    | Lident "bool" -> [%expr Monolith.Gen.bool]
    | Lident "char" -> [%expr Monolith.Gen.char]
    | Lident "int" ->
        [%expr Monolith.Gen.closed_interval Int.min_int Int.max_int]
    | Lident "string" ->
        [%expr
          Monolith.Gen.string
            (Monolith.Gen.int Sys.max_string_length)
            Monolith.Gen.char]
    | Lident "array" ->
        assert (List.length args = 1);
        let param = derive ~loc (List.hd args) in
        [%expr
          Monolith.Gen.array (Monolith.Gen.int Sys.max_array_length) [%e param]]
    | Lident "list" ->
        assert (List.length args = 1);
        let param = derive ~loc (List.hd args) in
        [%expr Monolith.Gen.list (Monolith.Gen.int 42000) [%e param]]
    | Lident "option" ->
        assert (List.length args = 1);
        let param = derive ~loc (List.hd args) in
        [%expr Monolith.Gen.option [%e param]]
    | Lident "result" ->
        assert (List.length args = 2);
        let ok = derive ~loc (List.hd args) in
        let err = derive ~loc (List.nth args 1) in
        [%expr Monolith.Gen.result [%e ok] [%e err]]
    (* In the following cases (Lident _, Ldot (_,_) and Lapply (_,_)),
       we rely on the fact that the `printer` is already defined *)
    | lid -> Utils.(lident ~loc lid Gen)
end

module Variant = struct
  (* a function that build the arguments of the constructor *)
  let cstr_arg ~loc = function
    | Pcstr_tuple cts ->
        List.map
          (fun ct ->
            let t = Core_type.derive ~loc ct in
            Utils.trigger ~loc t)
          cts
        |> pexp_tuple_opt ~loc
    | Pcstr_record ldl ->
        let fields =
          List.map
            (fun ld ->
              ( Utils.ld_to_longident ~loc ld,
                Utils.trigger ~loc (Core_type.derive ~loc ld.pld_type) ))
            ldl
        in
        Some (pexp_record ~loc fields None)

  (* a function that derives a generator of a constructor and its arguments *)
  let variant ~loc cd =
    let cstr = { txt = Ppxlib.lident cd.pcd_name.txt; loc } in
    let args = cstr_arg ~loc cd.pcd_args in
    Utils.suspend ~loc (pexp_construct ~loc cstr args)

  let derive ~loc cds =
    (* an array of the different constructors with their arguments *)
    let variants = List.map (variant ~loc) cds |> pexp_array ~loc in
    (* the generator choose one of the constructor *)
    let choice =
      [%expr
        let v = [%e variants] in
        v.(Monolith.Gen.int (Array.length v) ()) ()]
    in
    (* Wait for the user to trigger it *)
    Utils.suspend ~loc choice
end

module Record = struct
  let derive ~loc cds =
    let field fd =
      ( { txt = Lident fd.pld_name.txt; loc },
        Utils.trigger ~loc (Core_type.derive ~loc fd.pld_type) )
    in
    let record = pexp_record ~loc (List.map field cds) None in
    Utils.suspend ~loc record
end

let derive ~loc (type_decl : type_declaration) =
  match type_decl.ptype_manifest with
  | None -> (
      match type_decl.ptype_kind with
      | Ptype_abstract -> Raise.Unsupported.typekind ~loc "Ptype_abstract"
      | Ptype_variant cds -> Variant.derive ~loc cds
      | Ptype_record ldl -> Record.derive ~loc ldl
      | Ptype_open -> Raise.Unsupported.typekind ~loc "Ptype_open")
  | Some ty -> Core_type.derive ~loc ty
