open Ppxlib

let dummy ~loc str =
  Ast_builder.Default.pexp_constant ~loc
    (Pconst_string (str, Location.none, None))

let rec printer_core_type ~loc ct =
  match ct.ptyp_desc with
  | Ptyp_any -> dummy ~loc "printer_core_type Ptyp_any"
  | Ptyp_var _ -> dummy ~loc "printer_core_type Ptyp_var"
  | Ptyp_arrow (_, _, _) -> dummy ~loc "printer_core_type Ptyp_arrow"
  | Ptyp_tuple tys -> printer_tuple ~loc tys
  | Ptyp_constr ({ txt; _ }, args) -> printer_longident ~loc txt args
  | Ptyp_object (_, _) -> dummy ~loc "printer_core_type Ptyp_object"
  | Ptyp_class (_, _) -> dummy ~loc "printer_core_type Ptyp_class"
  | Ptyp_alias (_, _) -> dummy ~loc "printer_core_type Ptyp_alias"
  | Ptyp_variant (_, _, _) -> dummy ~loc "printer_core_type Ptyp_variant"
  | Ptyp_poly (_, _) -> dummy ~loc "printer_core_type Ptyp_poly"
  | Ptyp_package _ -> dummy ~loc "printer_core_type Ptpy_package"
  | Ptyp_extension _ -> dummy ~loc "printer_core_type Ptyp_extension"

and printer_tuple ~loc tys =
  let printers = List.map (printer_core_type ~loc) tys in
  let elts = List.mapi (fun i _ -> "elt__" ^ Int.to_string i) tys in
  let vars = List.map (Ast_builder.Default.evar ~loc) elts in
  let pats =
    Ast_builder.Default.ppat_tuple ~loc
      (List.map (Ast_builder.Default.pvar ~loc) elts)
  in
  let tuple =
    Ast_builder.Default.elist ~loc
      (List.map2
         (fun p v -> Ast_builder.Default.eapply ~loc p [ v ])
         printers vars)
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
  | _ -> dummy ~loc "printer_longident catch all"

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
        let parg =
          Ast_builder.Default.ppat_tuple_opt ~loc
            (List.map (Ast_builder.Default.pvar ~loc) xs)
        in
        (* left hand side is C (a0, a1,...) *)
        let lhs = Ast_builder.Default.ppat_construct ~loc cident parg in
        (* let's call the respective printers on the arguments of the constructor *)
        let pp_args =
          List.map2
            (fun x ty ->
              let printer = printer_core_type ~loc ty in
              Ast_builder.Default.eapply ~loc printer
                [ Ast_builder.Default.evar ~loc x ])
            xs args
          |> Ast_builder.Default.elist ~loc
        in
        (* right hand side print the variant *)
        let rhs =
          [%expr
            PPrintOCaml.variant ""
              [%e Ast_builder.Default.estring ~loc cd.pcd_name.txt]
              0 [%e pp_args]]
        in
        Ast_builder.Default.case ~guard:None ~lhs ~rhs
    | _ -> assert false
  in
  let cases = List.map variant ldl in
  Ast_builder.Default.pexp_function ~loc cases

let printer_record ~loc ldl =
  let pat pld =
    ( { txt = Lident pld.pld_name.txt; loc },
      Ast_builder.Default.pvar ~loc pld.pld_name.txt )
  in
  let rec_pat =
    Ast_builder.Default.ppat_record ~loc (List.map pat ldl) Closed
  in
  let pp_field pld =
    Ast_builder.Default.pexp_tuple ~loc
      [
        Ast_builder.Default.estring ~loc pld.pld_name.txt;
        Ast_builder.Default.eapply ~loc
          (printer_core_type ~loc pld.pld_type)
          [ Ast_builder.Default.evar ~loc pld.pld_name.txt ];
      ]
  in
  let fields = List.map pp_field ldl |> Ast_builder.Default.elist ~loc in
  [%expr fun [%p rec_pat] -> PPrintOCaml.record "" [%e fields]]
