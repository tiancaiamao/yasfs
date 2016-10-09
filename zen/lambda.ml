type t =
    Int of int
  | Bool of bool
  | Var of int
  | Tuple of t list
  | App of t * t list
  | Fun of int * t list
  | Fun1 of int * t list
  | Bind of t
  | If of t * t * t
  | Plus of t * t
  | Mul of t * t
  | Sub of t * t
  | Equal of t * t
  | Prim of string
  | Switch of t * case list
and case = int * t
