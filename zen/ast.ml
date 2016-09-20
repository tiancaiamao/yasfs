type t =
    Int of int
  | Var of string
  | App of t * t list
  | Fun of string list * t
