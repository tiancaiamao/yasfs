type tag_tuple_constraint = CNone | CLeast of int | CExact of int

type t =
    Int
  | Bool
  | Unit
  | Fun of t * t
  | Var of char
  | Tuple of t list
  | TagTuple of tag_tuple_desc
and tag_tuple_desc = {tag: int;  size: tag_tuple_constraint; tuple: t list}

let make_tuple_desc x y z = {tag=x; size=y; tuple=z}

let check_size n e = match e with
  | CNone -> true
  | CExact x -> n = x
  | CLeast x -> n >= x
