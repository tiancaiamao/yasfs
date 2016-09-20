type t =
    Int of int
  | Var of int
  | App of t * t list
  | Fun of int * t
