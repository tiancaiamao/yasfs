type t =
    Const of int
  | Bool of bool
  | Access of int
  | Closure of t list
  | Tailapply
  | Apply
  | Pushmark
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
