open Ppxlib
open Ast_builder.Default

module Core_type = struct
  let rec derive ~loc ct =
    match ct.ptyp_desc with
    | Ptyp_any -> Raise.Unsupported.coretype ~loc "wildcard"
    | Ptyp_var _ -> [%expr Monolith.Print.int]
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
    let printers = List.map (derive ~loc) tys in
    let elts = List.mapi (fun i _ -> "elt__" ^ Int.to_string i) tys in
    let vars = List.map (evar ~loc) elts in
    let pats = ppat_tuple ~loc (List.map (pvar ~loc) elts) in
    let tuple =
      elist ~loc (List.map2 (fun p v -> eapply ~loc p [ v ]) printers vars)
    in
    [%expr
      fun x ->
        let [%p pats] = x in
        PPrint.OCaml.tuple [%e tuple]]

  and longident ~loc txt args =
    match txt with
    | Lident "bool" -> [%expr Monolith.Print.bool]
    | Lident "char" -> [%expr Monolith.Print.char]
    | Lident "int" -> [%expr Monolith.Print.int]
    | Lident "string" -> [%expr Monolith.Print.string]
    | Lident "array" ->
        assert (List.length args = 1);
        let param = derive ~loc (List.hd args) in
        [%expr Monolith.Print.array [%e param]]
    | Lident "list" ->
        assert (List.length args = 1);
        let param = derive ~loc (List.hd args) in
        [%expr Monolith.Print.list [%e param]]
    | Lident "option" ->
        assert (List.length args = 1);
        let param = derive ~loc (List.hd args) in
        [%expr Monolith.Print.option [%e param]]
    | Lident "result" ->
        assert (List.length args = 2);
        let ok = derive ~loc (List.hd args) in
        let err = derive ~loc (List.nth args 1) in
        [%expr Monolith.Print.result [%e ok] [%e err]]
    (* In the following cases (Lident _, Ldot (_,_) and Lapply (_,_)),
       we rely on the fact that the `printer` is already defined *)
    | lid -> Utils.lident ~loc lid Printer

  let representation ~loc var ty =
    let printer = derive ~loc ty in
    eapply ~loc printer [ evar ~loc var ]
end

module Record = struct
  let representation ~loc ldl =
    let pp_field pld =
      pexp_tuple ~loc
        [
          estring ~loc pld.pld_name.txt;
          eapply ~loc
            (Core_type.derive ~loc pld.pld_type)
            [ evar ~loc pld.pld_name.txt ];
        ]
    in
    let fields = List.map pp_field ldl |> elist ~loc in
    [%expr PPrint.OCaml.record "" [%e fields]]

  let derive ~loc ldl =
    let pat pld =
      ({ txt = lident pld.pld_name.txt; loc }, pvar ~loc pld.pld_name.txt)
    in
    let rec_pat = ppat_record ~loc (List.map pat ldl) Closed in
    let fields = representation ~loc ldl in
    [%expr fun [%p rec_pat] -> [%e fields]]
end

module Variant = struct
  (* a function to treat one constructor at a time *)
  let variant ~loc cd =
    (* a longident for the constructor *)
    let cident = { txt = Lident cd.pcd_name.txt; loc } in
    (* Only lhs and pp_args are different depending on the case of cd.pcd_args*)
    let lhs, representations =
      match cd.pcd_args with
      | Pcstr_tuple args ->
          (* A list of name for the elements of the tuple *)
          let xs = List.mapi (fun i _ -> "pp_arg" ^ Int.to_string i) args in
          (* An OCaml expression for the list of the representations of the elements of the tuple *)
          let representations =
            List.map2 (Core_type.representation ~loc) xs args |> elist ~loc
          in
          let parg = ppat_tuple_opt ~loc (Utils.pvars ~loc xs) in
          (ppat_construct ~loc cident parg, representations)
      | Pcstr_record ldl ->
          let record_pat = Some (Utils.record_pattern ~loc ldl) in
          ( ppat_construct ~loc cident record_pat,
            [%expr [ [%e Record.representation ~loc ldl] ]] )
    in
    (* In the right hand side, we print the variant *)
    let rhs =
      [%expr
        PPrint.OCaml.variant ""
          [%e estring ~loc cd.pcd_name.txt]
          0 [%e representations]]
    in
    case ~guard:None ~lhs ~rhs

  let derive ~loc ldl =
    let cases = List.map (variant ~loc) ldl in
    pexp_function ~loc cases
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
