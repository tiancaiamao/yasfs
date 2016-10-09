type t =
    Int
  | Bool
  | Fun of t * t
  | Var of char
  | Unit
  | Tuple of t list
  | Union of string * (string * t) list
