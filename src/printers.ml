open Ppxlib
open Ast_builder.Default
open Utils

let rec printer_core_type ~loc ct =
  match ct.ptyp_desc with
  | Ptyp_any -> Raise.Unsupported.coretype ~loc "wildcard"
  | Ptyp_var _ -> [%expr Monolith.Print.int]
  | Ptyp_arrow (_, _, _) -> Raise.Unsupported.coretype ~loc "arrow"
  | Ptyp_tuple tys -> printer_tuple ~loc tys
  | Ptyp_constr ({ txt; _ }, args) -> printer_longident ~loc txt args
  | Ptyp_object (_, _) -> Raise.Unsupported.coretype ~loc "object"
  | Ptyp_class (_, _) -> Raise.Unsupported.coretype ~loc "class"
  | Ptyp_alias (_, _) -> Raise.Unsupported.coretype ~loc "alias"
  | Ptyp_variant (_, _, _) -> Raise.Unsupported.coretype ~loc "variant"
  | Ptyp_poly (_, _) -> Raise.Unsupported.coretype ~loc "poly"
  | Ptyp_package _ -> Raise.Unsupported.coretype ~loc "package"
  | Ptyp_extension _ -> Raise.Unsupported.coretype ~loc "extension"

and printer_tuple ~loc tys =
  let printers = List.map (printer_core_type ~loc) tys in
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

and printer_longident ~loc txt args =
  match txt with
  | Lident "bool" -> [%expr Monolith.Print.bool]
  | Lident "char" -> [%expr Monolith.Print.char]
  | Lident "int" -> [%expr Monolith.Print.int]
  | Lident "string" -> [%expr Monolith.Print.string]
  | Lident "array" ->
      assert (List.length args = 1);
      let param = printer_core_type ~loc (List.hd args) in
      [%expr Monolith.Print.array [%e param]]
  | Lident "list" ->
      assert (List.length args = 1);
      let param = printer_core_type ~loc (List.hd args) in
      [%expr Monolith.Print.list [%e param]]
  | Lident "option" ->
      assert (List.length args = 1);
      let param = printer_core_type ~loc (List.hd args) in
      [%expr Monolith.Print.option [%e param]]
  | Lident "result" ->
      assert (List.length args = 2);
      let ok = printer_core_type ~loc (List.hd args) in
      let err = printer_core_type ~loc (List.nth args 1) in
      [%expr Monolith.Print.result [%e ok] [%e err]]
  | Lident id -> var ~loc id Printer
  | Ldot (_, _) -> Raise.Unsupported.longident ~loc "Ldot"
  | Lapply (_, _) -> Raise.Unsupported.longident ~loc "Lapply"

let printer_variant ~loc ldl =
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
              let printer = printer_core_type ~loc ty in
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
    | _ -> assert false
  in
  let cases = List.map variant ldl in
  pexp_function ~loc cases

let printer_record ~loc ldl =
  let pat pld =
    ({ txt = Lident pld.pld_name.txt; loc }, pvar ~loc pld.pld_name.txt)
  in
  let rec_pat = ppat_record ~loc (List.map pat ldl) Closed in
  let pp_field pld =
    pexp_tuple ~loc
      [
        estring ~loc pld.pld_name.txt;
        eapply ~loc
          (printer_core_type ~loc pld.pld_type)
          [ evar ~loc pld.pld_name.txt ];
      ]
  in
  let fields = List.map pp_field ldl |> elist ~loc in
  [%expr fun [%p rec_pat] -> PPrintOCaml.record "" [%e fields]]

let printer_kind ~loc (tk : type_kind) =
  match tk with
  | Ptype_abstract -> Raise.Unsupported.typekind ~loc "Ptype_abstract"
  | Ptype_variant cds -> printer_variant ~loc cds
  | Ptype_record ldl -> printer_record ~loc ldl
  | Ptype_open -> Raise.Unsupported.typekind ~loc "Ptype_open"

let printer_expr ~loc (type_decl : type_declaration) =
  match type_decl.ptype_manifest with
  | None -> printer_kind ~loc type_decl.ptype_kind
  | Some ty -> printer_core_type ~loc ty
