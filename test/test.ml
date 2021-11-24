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
