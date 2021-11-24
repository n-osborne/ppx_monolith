type t0 = int list[@@deriving monolith]
include
  struct
    let t0_spec =
      Monolith.list (Monolith.closed_interval Int.min_int Int.max_int)
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t1 = int option[@@deriving monolith]
include
  struct
    let t1_spec =
      Monolith.option (Monolith.closed_interval Int.min_int Int.max_int)
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t2 = int option list[@@deriving monolith]
include
  struct
    let t2_spec =
      Monolith.list
        (Monolith.option (Monolith.closed_interval Int.min_int Int.max_int))
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t3 = int array[@@deriving monolith]
include
  struct
    let t3_spec =
      let gen =
        Monolith.Gen.array (Monolith.Gen.int Sys.max_array_length)
          (Monolith.Gen.closed_interval Int.min_int Int.max_int) in
      let printer = Monolith.Print.array Monolith.Print.int in
      Monolith.ifpol (Monolith.easily_constructible gen printer)
        (Monolith.deconstructible printer)
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t4 = string[@@deriving monolith]
include
  struct
    let t4_spec =
      let gen =
        Monolith.Gen.string (Monolith.Gen.int Sys.max_string_length)
          Monolith.Gen.char in
      let printer = Monolith.Print.string in
      Monolith.ifpol (Monolith.easily_constructible gen printer)
        (Monolith.deconstructible printer)
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t5 = (int, string) result[@@deriving monolith]
include
  struct
    let t5_spec =
      Monolith.result (Monolith.closed_interval Int.min_int Int.max_int)
        (let gen =
           Monolith.Gen.string (Monolith.Gen.int Sys.max_string_length)
             Monolith.Gen.char in
         let printer = Monolith.Print.string in
         Monolith.ifpol (Monolith.easily_constructible gen printer)
           (Monolith.deconstructible printer))
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t6 = (int * string)[@@deriving monolith]
include
  struct
    let t6_spec =
      let gen () =
        ((Monolith.Gen.closed_interval Int.min_int Int.max_int ()),
          (Monolith.Gen.string (Monolith.Gen.int Sys.max_string_length)
             Monolith.Gen.char ())) in
      let printer x =
        let (elt__0, elt__1) = x in
        PPrintOCaml.tuple
          [Monolith.Print.int elt__0; Monolith.Print.string elt__1] in
      Monolith.ifpol (Monolith.easily_constructible gen printer)
        (Monolith.deconstructible printer)
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t7 =
  | C0 
  | C1 of int 
  | C2 of int * string 
  | C3 of (char * bool) [@@deriving monolith]
include
  struct
    let t7_spec =
      let gen () =
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
        (v.(Monolith.Gen.int (Array.length v) ())) () in
      let printer =
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
                 pp_arg0] in
      Monolith.ifpol (Monolith.easily_constructible gen printer)
        (Monolith.deconstructible printer)
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t8 = {
  f0: int ;
  f1: string }[@@deriving monolith]
include
  struct
    let t8_spec =
      let gen () =
        {
          f0 = (Monolith.Gen.closed_interval Int.min_int Int.max_int ());
          f1 =
            (Monolith.Gen.string (Monolith.Gen.int Sys.max_string_length)
               Monolith.Gen.char ())
        } in
      let printer { f0; f1 } =
        PPrintOCaml.record ""
          [("f0", (Monolith.Print.int f0));
          ("f1", (Monolith.Print.string f1))] in
      Monolith.ifpol (Monolith.easily_constructible gen printer)
        (Monolith.deconstructible printer)
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
