type t =
    Const of int
  | Bool of bool
  | MakeTuple of int * int
  | Prim of string
  | StackAccess of int
  | EnvAccess of int
  | Closure of t list
  | Apply
  | Push
  | PushRetAddr of t list
  | Grab of int
  | Return of int
  | Stop
  | Branch of t list * t list
  | Bind
  | Plus
  | Mul
  | Sub
  | Div
  | Equal
  | Field of int
  | Switch of (int * t list) list
