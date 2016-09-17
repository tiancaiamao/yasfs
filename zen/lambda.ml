type t =
    Var of int
  | App of t * t list
  | Fun of int * t
  | Let of t list * t
  | If of t * t * t
