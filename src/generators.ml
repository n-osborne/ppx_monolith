open Ppxlib
open Ast_builder.Default
open Utils

let rec gen_core_type ~loc (ct : core_type) =
  match ct.ptyp_desc with
  | Ptyp_any -> dummy ~loc "gen_core_type Ptyp_any"
  | Ptyp_var _ -> dummy ~loc "gen_core_type Ptyp_var"
  | Ptyp_arrow (_, _, _) -> dummy ~loc "gen_core_type Ptyp_arrow"
  | Ptyp_tuple tys -> gen_tuple ~loc tys
  | Ptyp_constr ({ txt; _ }, args) -> gen_longident ~loc txt args
  | Ptyp_object (_, _) -> dummy ~loc "gen_core_type Ptyp_object"
  | Ptyp_class (_, _) -> dummy ~loc "gen_core_type Ptyp_class"
  | Ptyp_alias (_, _) -> dummy ~loc "gen_core_type Ptyp_alias"
  | Ptyp_variant (_, _, _) -> dummy ~loc "gen_core_type Ptyp_variant"
  | Ptyp_poly (_, _) -> dummy ~loc "gen_core_type Ptyp_poly"
  | Ptyp_package _ -> dummy ~loc "gen_core_type Ptpy_package"
  | Ptyp_extension _ -> dummy ~loc "gen_core_type Ptyp_extension"

and gen_tuple ~loc tys =
  let gen =
    pexp_tuple ~loc
      (List.map
         (fun t -> eapply ~loc (gen_core_type ~loc t) [ eunit ~loc ])
         tys)
  in
  [%expr fun () -> [%e gen]]

and gen_longident ~loc txt args =
  match txt with
  | Lident "bool" -> [%expr Monolith.Gen.bool]
  | Lident "char" -> [%expr Monolith.Gen.char]
  | Lident "int" -> [%expr Monolith.Gen.closed_interval Int.min_int Int.max_int]
  | Lident "string" ->
      [%expr
        Monolith.Gen.string
          (Monolith.Gen.int Sys.max_string_length)
          Monolith.Gen.char]
  | Lident "array" ->
      assert (List.length args = 1);
      let param = gen_core_type ~loc (List.hd args) in
      [%expr
        Monolith.Gen.array (Monolith.Gen.int Sys.max_array_length) [%e param]]
  | Lident "list" ->
      assert (List.length args = 1);
      let param = gen_core_type ~loc (List.hd args) in
      [%expr Monolith.Gen.list (Monolith.Gen.int 42000) [%e param]]
  | Lident "option" ->
      assert (List.length args = 1);
      let param = gen_core_type ~loc (List.hd args) in
      [%expr Monolith.Gen.option [%e param]]
  | Lident "result" ->
      assert (List.length args = 2);
      let ok = gen_core_type ~loc (List.hd args) in
      let err = gen_core_type ~loc (List.nth args 1) in
      [%expr Monolith.Gen.result [%e ok] [%e err]]
  | Lident id -> var ~loc id Gen
  | _ -> dummy ~loc "gen_longident catch all"

let gen_variant ~loc cds =
  (* a function that build the arguments of the constructor *)
  let cstr_arg = function
    | Pcstr_tuple cts ->
        List.map
          (fun ct ->
            gen_core_type ~loc ct |> fun e -> eapply ~loc e [ eunit ~loc ])
          cts
        |> pexp_tuple_opt ~loc
    | Pcstr_record _ldl -> Some (dummy ~loc "variant constructor with a record")
  in
  (* a function that construct a generator of a constructor and its arguments *)
  let variant cd =
    let cstr = pexp_construct ~loc { txt = Lident cd.pcd_name.txt; loc } in
    [%expr fun () -> [%e cstr (cstr_arg cd.pcd_args)]]
  in
  (* an array of the different constructors with their arguments *)
  let variants = List.map variant cds |> pexp_array ~loc in
  (* the generator choose one of the constructor *)
  [%expr
    fun () ->
      let v = [%e variants] in
      v.(Monolith.Gen.int (Array.length v) ()) ()]

let gen_record ~loc cds =
  let field fd =
    ( { txt = Lident fd.pld_name.txt; loc },
      eapply ~loc (gen_core_type ~loc fd.pld_type) [ eunit ~loc ] )
  in
  let record = pexp_record ~loc (List.map field cds) None in
  [%expr fun () -> [%e record]]

let gen_kind ~loc (tk : type_kind) =
  match tk with
  | Ptype_abstract -> dummy ~loc "gen_kind Ptype_abstract"
  | Ptype_variant cds -> gen_variant ~loc cds
  | Ptype_record ldl -> gen_record ~loc ldl
  | Ptype_open -> dummy ~loc "gen_kind Ptype_open"

let gen_expr ~loc (type_decl : type_declaration) =
  Option.fold
    ~none:(gen_kind ~loc type_decl.ptype_kind)
    ~some:(gen_core_type ~loc) type_decl.ptype_manifest
