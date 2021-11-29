type t0 = int list [@@deriving monolith]
type t1 = int option [@@deriving monolith]
type t2 = int option list [@@deriving monolith]
type t3 = int array [@@deriving monolith]
type t4 = string [@@deriving monolith]
type t5 = (int, string) result [@@deriving monolith]
type t6 = int * string [@@deriving monolith]

type t7 = C0 | C1 of int | C2 of int * string | C3 of (char * bool)
[@@deriving monolith]

type t8 = { f0 : int; f1 : string } [@@deriving monolith]
type t9 = t0 [@@deriving monolith]
type t10 = { f2 : t7; f3 : t8 } [@@deriving monolith]
type t11 = t9 * t10 [@@deriving monolith]

type mutual_1 = C4 of mutual_2 [@@deriving monolith]

and mutual_2 = C5 of mutual_1 | C6 of int [@@deriving monolith]
