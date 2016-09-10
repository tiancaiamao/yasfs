type t =
  | Int of int
  | Add of t * t
  | Let of string * t * t
  | Var of string
  | App of t * t list
  | Fun of t list * t
