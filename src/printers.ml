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
        PPrintOCaml.tuple [%e tuple]]

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
    [%expr PPrintOCaml.record "" [%e fields]]

  let derive ~loc ldl =
    let pat pld =
      ({ txt = Lident pld.pld_name.txt; loc }, pvar ~loc pld.pld_name.txt)
    in
    let rec_pat = ppat_record ~loc (List.map pat ldl) Closed in
    let fields = representation ~loc ldl in
    [%expr fun [%p rec_pat] -> [%e fields]]
end

module Variant = struct
  let derive ~loc ldl =
    (* a function to treat one constructor at a time *)
    let variant cd =
      (* a longident for the constructor *)
      let cident = { txt = Lident cd.pcd_name.txt; loc } in
      match cd.pcd_args with
      | Pcstr_tuple args ->
          (* a list of names for the arguments *)
          let xs = List.mapi (fun i _ -> "pp_arg" ^ Int.to_string i) args in
          (* bundle the arguments in an optional tuple of pattern variables*)
          let parg = ppat_tuple_opt ~loc (List.map (pvar ~loc) xs) in
          (* left hand side is C (a0, a1,...) *)
          let lhs = ppat_construct ~loc cident parg in
          (* let's call the respective printers on the arguments of the constructor *)
          let pp_args =
            List.map2
              (fun x ty ->
                let printer = Core_type.derive ~loc ty in
                eapply ~loc printer [ evar ~loc x ])
              xs args
            |> elist ~loc
          in
          (* right hand side print the variant *)
          let rhs =
            [%expr
              PPrintOCaml.variant ""
                [%e estring ~loc cd.pcd_name.txt]
                0 [%e pp_args]]
          in
          case ~guard:None ~lhs ~rhs
      | Pcstr_record ldl ->
          let lhs =
            ppat_construct ~loc cident
              (Some
                 (ppat_record ~loc
                    (List.map
                       (fun ld ->
                         ( { txt = Ppxlib.lident ld.pld_name.txt; loc },
                           pvar ~loc ld.pld_name.txt ))
                       ldl)
                    Closed))
          in
          let pp_args = Record.representation ~loc ldl in
          let rhs =
            [%expr
              PPrintOCaml.variant ""
                [%e estring ~loc cd.pcd_name.txt]
                0 [ [%e pp_args] ]]
          in
          case ~guard:None ~lhs ~rhs
    in
    let cases = List.map variant ldl in
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
