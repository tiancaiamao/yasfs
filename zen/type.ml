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
