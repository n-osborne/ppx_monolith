type t0 = int list[@@deriving monolith]
include
  struct
    let __monolith_t0_printer_ = Monolith.Print.list Monolith.Print.int
    let __monolith_t0_gen_ =
      Monolith.Gen.list (Monolith.Gen.int 42000)
        (Monolith.Gen.closed_interval Int.min_int Int.max_int)
    let __monolith_t0_spec_ =
      Monolith.list (Monolith.closed_interval Int.min_int Int.max_int)
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t1 = int option[@@deriving monolith]
include
  struct
    let __monolith_t1_printer_ = Monolith.Print.option Monolith.Print.int
    let __monolith_t1_gen_ =
      Monolith.Gen.option
        (Monolith.Gen.closed_interval Int.min_int Int.max_int)
    let __monolith_t1_spec_ =
      Monolith.option (Monolith.closed_interval Int.min_int Int.max_int)
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t2 = int option list[@@deriving monolith]
include
  struct
    let __monolith_t2_printer_ =
      Monolith.Print.list (Monolith.Print.option Monolith.Print.int)
    let __monolith_t2_gen_ =
      Monolith.Gen.list (Monolith.Gen.int 42000)
        (Monolith.Gen.option
           (Monolith.Gen.closed_interval Int.min_int Int.max_int))
    let __monolith_t2_spec_ =
      Monolith.list
        (Monolith.option (Monolith.closed_interval Int.min_int Int.max_int))
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t3 = int array[@@deriving monolith]
include
  struct
    let __monolith_t3_printer_ = Monolith.Print.array Monolith.Print.int
    let __monolith_t3_gen_ =
      Monolith.Gen.array (Monolith.Gen.int Sys.max_array_length)
        (Monolith.Gen.closed_interval Int.min_int Int.max_int)
    let __monolith_t3_spec_ =
      Monolith.ifpol
        (Monolith.easily_constructible
           (Monolith.Gen.array (Monolith.Gen.int Sys.max_array_length)
              (Monolith.Gen.closed_interval Int.min_int Int.max_int))
           (Monolith.Print.array Monolith.Print.int))
        (Monolith.deconstructible (Monolith.Print.array Monolith.Print.int))
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t4 = string[@@deriving monolith]
include
  struct
    let __monolith_t4_printer_ = Monolith.Print.string
    let __monolith_t4_gen_ =
      Monolith.Gen.string (Monolith.Gen.int Sys.max_string_length)
        Monolith.Gen.char
    let __monolith_t4_spec_ =
      Monolith.ifpol
        (Monolith.easily_constructible
           (Monolith.Gen.string (Monolith.Gen.int Sys.max_string_length)
              Monolith.Gen.char) Monolith.Print.string)
        (Monolith.deconstructible Monolith.Print.string)
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t5 = (int, string) result[@@deriving monolith]
include
  struct
    let __monolith_t5_printer_ =
      Monolith.Print.result Monolith.Print.int Monolith.Print.string
    let __monolith_t5_gen_ =
      Monolith.Gen.result
        (Monolith.Gen.closed_interval Int.min_int Int.max_int)
        (Monolith.Gen.string (Monolith.Gen.int Sys.max_string_length)
           Monolith.Gen.char)
    let __monolith_t5_spec_ =
      Monolith.result (Monolith.closed_interval Int.min_int Int.max_int)
        (Monolith.ifpol
           (Monolith.easily_constructible
              (Monolith.Gen.string (Monolith.Gen.int Sys.max_string_length)
                 Monolith.Gen.char) Monolith.Print.string)
           (Monolith.deconstructible Monolith.Print.string))
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t6 = (int * string)[@@deriving monolith]
include
  struct
    let __monolith_t6_printer_ x =
      let (elt__0, elt__1) = x in
      PPrintOCaml.tuple
        [Monolith.Print.int elt__0; Monolith.Print.string elt__1]
    let __monolith_t6_gen_ () =
      ((Monolith.Gen.closed_interval Int.min_int Int.max_int ()),
        (Monolith.Gen.string (Monolith.Gen.int Sys.max_string_length)
           Monolith.Gen.char ()))
    let __monolith_t6_spec_ =
      Monolith.ifpol
        (Monolith.easily_constructible
           (fun () ->
              ((Monolith.Gen.closed_interval Int.min_int Int.max_int ()),
                (Monolith.Gen.string (Monolith.Gen.int Sys.max_string_length)
                   Monolith.Gen.char ())))
           (fun x ->
              let (elt__0, elt__1) = x in
              PPrintOCaml.tuple
                [Monolith.Print.int elt__0; Monolith.Print.string elt__1]))
        (Monolith.deconstructible
           (fun x ->
              let (elt__0, elt__1) = x in
              PPrintOCaml.tuple
                [Monolith.Print.int elt__0; Monolith.Print.string elt__1]))
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t7 =
  | C0 
  | C1 of int 
  | C2 of int * string 
  | C3 of (char * bool) [@@deriving monolith]
include
  struct
    let __monolith_t7_printer_ =
      function
      | C0 -> PPrintOCaml.variant "" "C0" 0 []
      | C1 pp_arg0 ->
          PPrintOCaml.variant "" "C1" 0 [Monolith.Print.int pp_arg0]
      | C2 (pp_arg0, pp_arg1) ->
          PPrintOCaml.variant "" "C2" 0
            [Monolith.Print.int pp_arg0; Monolith.Print.string pp_arg1]
      | C3 pp_arg0 ->
          PPrintOCaml.variant "" "C3" 0
            [((fun x ->
                 let (elt__0, elt__1) = x in
                 PPrintOCaml.tuple
                   [Monolith.Print.char elt__0; Monolith.Print.bool elt__1]))
               pp_arg0]
    let __monolith_t7_gen_ () =
      let v =
        [|((fun () -> C0));((fun () ->
                               C1
                                 (Monolith.Gen.closed_interval Int.min_int
                                    Int.max_int ())));((fun () ->
                                                          C2
                                                            ((Monolith.Gen.closed_interval
                                                                Int.min_int
                                                                Int.max_int
                                                                ()),
                                                              (Monolith.Gen.string
                                                                 (Monolith.Gen.int
                                                                    Sys.max_string_length)
                                                                 Monolith.Gen.char
                                                                 ()))));((
          fun () ->
            C3
              ((fun () -> ((Monolith.Gen.char ()), (Monolith.Gen.bool ())))
                 ())))|] in
      (v.(Monolith.Gen.int (Array.length v) ())) ()
    let __monolith_t7_spec_ =
      Monolith.ifpol
        (Monolith.easily_constructible __monolith_t7_gen_
           __monolith_t7_printer_)
        (Monolith.deconstructible __monolith_t7_printer_)
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t8 = {
  f0: int ;
  f1: string }[@@deriving monolith]
include
  struct
    let __monolith_t8_printer_ { f0; f1 } =
      PPrintOCaml.record ""
        [("f0", (Monolith.Print.int f0)); ("f1", (Monolith.Print.string f1))]
    let __monolith_t8_gen_ () =
      {
        f0 = (Monolith.Gen.closed_interval Int.min_int Int.max_int ());
        f1 =
          (Monolith.Gen.string (Monolith.Gen.int Sys.max_string_length)
             Monolith.Gen.char ())
      }
    let __monolith_t8_spec_ =
      Monolith.ifpol
        (Monolith.easily_constructible __monolith_t8_gen_
           __monolith_t8_printer_)
        (Monolith.deconstructible __monolith_t8_printer_)
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t9 = t0[@@deriving monolith]
include
  struct
    let __monolith_t9_printer_ = __monolith_t0_printer_
    let __monolith_t9_gen_ = __monolith_t0_gen_
    let __monolith_t9_spec_ = __monolith_t0_spec_
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t10 = {
  f2: t7 ;
  f3: t8 }[@@deriving monolith]
include
  struct
    let __monolith_t10_printer_ { f2; f3 } =
      PPrintOCaml.record ""
        [("f2", (__monolith_t7_printer_ f2));
        ("f3", (__monolith_t8_printer_ f3))]
    let __monolith_t10_gen_ () =
      { f2 = (__monolith_t7_gen_ ()); f3 = (__monolith_t8_gen_ ()) }
    let __monolith_t10_spec_ =
      Monolith.ifpol
        (Monolith.easily_constructible __monolith_t10_gen_
           __monolith_t10_printer_)
        (Monolith.deconstructible __monolith_t10_printer_)
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t11 = (t9 * t10)[@@deriving monolith]
include
  struct
    let __monolith_t11_printer_ x =
      let (elt__0, elt__1) = x in
      PPrintOCaml.tuple
        [__monolith_t9_printer_ elt__0; __monolith_t10_printer_ elt__1]
    let __monolith_t11_gen_ () =
      ((__monolith_t9_gen_ ()), (__monolith_t10_gen_ ()))
    let __monolith_t11_spec_ =
      Monolith.ifpol
        (Monolith.easily_constructible
           (fun () -> ((__monolith_t9_gen_ ()), (__monolith_t10_gen_ ())))
           (fun x ->
              let (elt__0, elt__1) = x in
              PPrintOCaml.tuple
                [__monolith_t9_printer_ elt__0;
                __monolith_t10_printer_ elt__1]))
        (Monolith.deconstructible
           (fun x ->
              let (elt__0, elt__1) = x in
              PPrintOCaml.tuple
                [__monolith_t9_printer_ elt__0;
                __monolith_t10_printer_ elt__1]))
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
