type t =
    Int of int
  | Var of int
  | App of t * t list
  | Fun of int * t list
  | Fun1 of int * t list
  | Bind of t
  | Plus of t * t
  | Equal of t * t
