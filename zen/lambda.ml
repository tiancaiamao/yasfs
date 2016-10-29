type t =
    Int of int
  | Bool of bool
  | String of string
  | Var of int
  | Tuple of int * t list
  | App of t * t list
  | CCall of string * t list
  | Fun of int * t list
  | Fun1 of int * t list
  | Bind of t
  | If of t * t * t
  | Plus of t * t
  | Mul of t * t
  | Sub of t * t
  | Div of t * t
  | Field of int * t
  | Equal of t * t
  | Switch of t * (int * t) list
