type t =
    Const of int
  | Bool of bool
  | String of string
  | MakeTuple of int * int
  | CCall of int
  | StackAccess of int
  | EnvAccess of int
  | Closure of t list
  | Apply
  | Push
  | PushRetAddr of t list
  | Grab of int
  | Return
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
