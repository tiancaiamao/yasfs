type tag = TNone | TOneof of int list | TExact of int

type t =
    Int
  | Bool
  | Unit
  | Fun of t * t
  | Tuple of tag * t list
  | Var of char
