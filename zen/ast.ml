type t =
  | Int of int
  | Var of string
  | Add of t * t
  | Let of string * t * t
  | App of t * t list
  | Fun of t list * t
