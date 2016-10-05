type t =
    Int
  | Bool
  | Fun of t * t
  | Var of char
  | Unit
