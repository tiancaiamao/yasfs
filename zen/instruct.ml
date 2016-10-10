type t =
    Const of int
  | Bool of bool
  | MakeTuple of int
  | MakeUnion of int
  | Prim of string
  | Access of int
  | Closure of t list
  | Tailapply
  | Apply
  | Pushmark
  | Copy
  | Pop
  | Grab
  | Return
  | Stop
  | Branch of t list * t list
  | Bind
  | Plus
  | Mul
  | Sub
  | Equal
