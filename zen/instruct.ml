type t =
    Const of int
  | Access of int
  | Closure of t list
  | Tailapply
  | Apply
  | Pushmark
  | Grab
  | Return
  | Stop
  | Bind
