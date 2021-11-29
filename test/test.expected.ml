type t0 = int list[@@deriving monolith]
include
  struct
    let __monolith__printer_t0 = Monolith.Print.list Monolith.Print.int
    let __monolith__gen_t0 =
      Monolith.Gen.list (Monolith.Gen.int 42000)
        (Monolith.Gen.closed_interval Int.min_int Int.max_int)
    let __monolith__spec_t0 =
      Monolith.list (Monolith.closed_interval Int.min_int Int.max_int)
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t1 = int option[@@deriving monolith]
include
  struct
    let __monolith__printer_t1 = Monolith.Print.option Monolith.Print.int
    let __monolith__gen_t1 =
      Monolith.Gen.option
        (Monolith.Gen.closed_interval Int.min_int Int.max_int)
    let __monolith__spec_t1 =
      Monolith.option (Monolith.closed_interval Int.min_int Int.max_int)
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t2 = int option list[@@deriving monolith]
include
  struct
    let __monolith__printer_t2 =
      Monolith.Print.list (Monolith.Print.option Monolith.Print.int)
    let __monolith__gen_t2 =
      Monolith.Gen.list (Monolith.Gen.int 42000)
        (Monolith.Gen.option
           (Monolith.Gen.closed_interval Int.min_int Int.max_int))
    let __monolith__spec_t2 =
      Monolith.list
        (Monolith.option (Monolith.closed_interval Int.min_int Int.max_int))
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t3 = int array[@@deriving monolith]
include
  struct
    let __monolith__printer_t3 = Monolith.Print.array Monolith.Print.int
    let __monolith__gen_t3 =
      Monolith.Gen.array (Monolith.Gen.int Sys.max_array_length)
        (Monolith.Gen.closed_interval Int.min_int Int.max_int)
    let __monolith__spec_t3 =
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
    let __monolith__printer_t4 = Monolith.Print.string
    let __monolith__gen_t4 =
      Monolith.Gen.string (Monolith.Gen.int Sys.max_string_length)
        Monolith.Gen.char
    let __monolith__spec_t4 =
      Monolith.ifpol
        (Monolith.easily_constructible
           (Monolith.Gen.string (Monolith.Gen.int Sys.max_string_length)
              Monolith.Gen.char) Monolith.Print.string)
        (Monolith.deconstructible Monolith.Print.string)
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t5 = (int, string) result[@@deriving monolith]
include
  struct
    let __monolith__printer_t5 =
      Monolith.Print.result Monolith.Print.int Monolith.Print.string
    let __monolith__gen_t5 =
      Monolith.Gen.result
        (Monolith.Gen.closed_interval Int.min_int Int.max_int)
        (Monolith.Gen.string (Monolith.Gen.int Sys.max_string_length)
           Monolith.Gen.char)
    let __monolith__spec_t5 =
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
    let __monolith__printer_t6 x =
      let (elt__0, elt__1) = x in
      PPrintOCaml.tuple
        [Monolith.Print.int elt__0; Monolith.Print.string elt__1]
    let __monolith__gen_t6 () =
      ((Monolith.Gen.closed_interval Int.min_int Int.max_int ()),
        (Monolith.Gen.string (Monolith.Gen.int Sys.max_string_length)
           Monolith.Gen.char ()))
    let __monolith__spec_t6 =
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
    let __monolith__printer_t7 =
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
    let __monolith__gen_t7 () =
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
    let __monolith__spec_t7 =
      Monolith.ifpol
        (Monolith.easily_constructible __monolith__gen_t7
           __monolith__printer_t7)
        (Monolith.deconstructible __monolith__printer_t7)
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t8 = {
  f0: int ;
  f1: string }[@@deriving monolith]
include
  struct
    let __monolith__printer_t8 { f0; f1 } =
      PPrintOCaml.record ""
        [("f0", (Monolith.Print.int f0)); ("f1", (Monolith.Print.string f1))]
    let __monolith__gen_t8 () =
      {
        f0 = (Monolith.Gen.closed_interval Int.min_int Int.max_int ());
        f1 =
          (Monolith.Gen.string (Monolith.Gen.int Sys.max_string_length)
             Monolith.Gen.char ())
      }
    let __monolith__spec_t8 =
      Monolith.ifpol
        (Monolith.easily_constructible __monolith__gen_t8
           __monolith__printer_t8)
        (Monolith.deconstructible __monolith__printer_t8)
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t9 = t0[@@deriving monolith]
include
  struct
    let __monolith__printer_t9 = __monolith__printer_t0
    let __monolith__gen_t9 = __monolith__gen_t0
    let __monolith__spec_t9 = __monolith__spec_t0
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t10 = {
  f2: t7 ;
  f3: t8 }[@@deriving monolith]
include
  struct
    let __monolith__printer_t10 { f2; f3 } =
      PPrintOCaml.record ""
        [("f2", (__monolith__printer_t7 f2));
        ("f3", (__monolith__printer_t8 f3))]
    let __monolith__gen_t10 () =
      { f2 = (__monolith__gen_t7 ()); f3 = (__monolith__gen_t8 ()) }
    let __monolith__spec_t10 =
      Monolith.ifpol
        (Monolith.easily_constructible __monolith__gen_t10
           __monolith__printer_t10)
        (Monolith.deconstructible __monolith__printer_t10)
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t11 = (t9 * t10)[@@deriving monolith]
include
  struct
    let __monolith__printer_t11 x =
      let (elt__0, elt__1) = x in
      PPrintOCaml.tuple
        [__monolith__printer_t9 elt__0; __monolith__printer_t10 elt__1]
    let __monolith__gen_t11 () =
      ((__monolith__gen_t9 ()), (__monolith__gen_t10 ()))
    let __monolith__spec_t11 =
      Monolith.ifpol
        (Monolith.easily_constructible
           (fun () -> ((__monolith__gen_t9 ()), (__monolith__gen_t10 ())))
           (fun x ->
              let (elt__0, elt__1) = x in
              PPrintOCaml.tuple
                [__monolith__printer_t9 elt__0;
                __monolith__printer_t10 elt__1]))
        (Monolith.deconstructible
           (fun x ->
              let (elt__0, elt__1) = x in
              PPrintOCaml.tuple
                [__monolith__printer_t9 elt__0;
                __monolith__printer_t10 elt__1]))
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type mutual_1 =
  | C4 of mutual_2 [@@deriving monolith]
and mutual_2 =
  | C5 of mutual_1 
  | C6 of int [@@deriving monolith]
include
  struct
    let rec __monolith__printer_mutual_1 =
      function
      | C4 pp_arg0 ->
          PPrintOCaml.variant "" "C4" 0
            [__monolith__printer_mutual_2 pp_arg0]
    and __monolith__printer_mutual_2 =
      function
      | C5 pp_arg0 ->
          PPrintOCaml.variant "" "C5" 0
            [__monolith__printer_mutual_1 pp_arg0]
      | C6 pp_arg0 ->
          PPrintOCaml.variant "" "C6" 0 [Monolith.Print.int pp_arg0]
    let rec __monolith__gen_mutual_1 () =
      let v = [|((fun () -> C4 (__monolith__gen_mutual_2 ())))|] in
      (v.(Monolith.Gen.int (Array.length v) ())) ()
    and __monolith__gen_mutual_2 () =
      let v =
        [|((fun () -> C5 (__monolith__gen_mutual_1 ())));((fun () ->
                                                             C6
                                                               (Monolith.Gen.closed_interval
                                                                  Int.min_int
                                                                  Int.max_int
                                                                  ())))|] in
      (v.(Monolith.Gen.int (Array.length v) ())) ()
    let __monolith__spec_mutual_1 =
      Monolith.ifpol
        (Monolith.easily_constructible __monolith__gen_mutual_1
           __monolith__printer_mutual_1)
        (Monolith.deconstructible __monolith__printer_mutual_1)
    and __monolith__spec_mutual_2 =
      Monolith.ifpol
        (Monolith.easily_constructible __monolith__gen_mutual_2
           __monolith__printer_mutual_2)
        (Monolith.deconstructible __monolith__printer_mutual_2)
    let rec __monolith__printer_mutual_1 =
      function
      | C4 pp_arg0 ->
          PPrintOCaml.variant "" "C4" 0
            [__monolith__printer_mutual_2 pp_arg0]
    and __monolith__printer_mutual_2 =
      function
      | C5 pp_arg0 ->
          PPrintOCaml.variant "" "C5" 0
            [__monolith__printer_mutual_1 pp_arg0]
      | C6 pp_arg0 ->
          PPrintOCaml.variant "" "C6" 0 [Monolith.Print.int pp_arg0]
    let rec __monolith__gen_mutual_1 () =
      let v = [|((fun () -> C4 (__monolith__gen_mutual_2 ())))|] in
      (v.(Monolith.Gen.int (Array.length v) ())) ()
    and __monolith__gen_mutual_2 () =
      let v =
        [|((fun () -> C5 (__monolith__gen_mutual_1 ())));((fun () ->
                                                             C6
                                                               (Monolith.Gen.closed_interval
                                                                  Int.min_int
                                                                  Int.max_int
                                                                  ())))|] in
      (v.(Monolith.Gen.int (Array.length v) ())) ()
    let __monolith__spec_mutual_1 =
      Monolith.ifpol
        (Monolith.easily_constructible __monolith__gen_mutual_1
           __monolith__printer_mutual_1)
        (Monolith.deconstructible __monolith__printer_mutual_1)
    and __monolith__spec_mutual_2 =
      Monolith.ifpol
        (Monolith.easily_constructible __monolith__gen_mutual_2
           __monolith__printer_mutual_2)
        (Monolith.deconstructible __monolith__printer_mutual_2)
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type 'a t12 = 'a list[@@deriving monolith]
include
  struct
    let __monolith__printer_t12 = Monolith.Print.list Monolith.Print.int
    let __monolith__gen_t12 =
      Monolith.Gen.list (Monolith.Gen.int 42000) Monolith.Gen.sequential
    let __monolith__spec_t12 = Monolith.list (Monolith.sequential ())
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
