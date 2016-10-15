type tag = TAny | TOneof of int list | TExact of int | TNone

type t =
    Int
  | Bool
  | Unit
  | Fun of t * t
  | Var of char
  | Tuple of tuple_desc
and tuple_desc = {mutable tag: tag; mutable tuple: (int * t) list}

let make_tuple_desc tag tuple = {tag=tag; tuple=tuple}
