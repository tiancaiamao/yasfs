type t =
    Const of int
  | Bool of bool
  | MakeTuple of int * int
  | Prim of string
  | Access of int
  | Closure of t list
  | Tailapply
  | Apply
  | Push
  | Pushmark
  | Copy
  | Pop
  | Grab of int
  | UNENV
  | Return
  | Stop
  | Branch of t list * t list
  | Bind
  | Plus
  | Mul
  | Sub
  | Equal
  | Field of int
  | Switch of (int * t list) list
