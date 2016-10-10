type name = Name of string

type field = Field of string

type t =
    Int
  | Bool
  | Fun of t * t
  | Var of char
  | Unit
  | Tuple of t list
  | Union of name * (field * t) list
