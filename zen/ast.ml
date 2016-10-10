type typedef =
  (* | Union of string * (string * Type.t) list *)
  | Union of string * string
  | Struct of string * (string * Type.t) list

type t =
    Int of int
  | Bool of bool
  | Var of string
  | App of t * t list
  | Fun of string list * t list
  | Fun1 of string list * t list
  | Bind of string * t
  | Tuple of t list
  | If of t * t * t
  | Plus of t * t
  | Sub of t * t
  | Mul of t * t
  | Equal of t * t
  | Construct of string * (string * t) list
  | Switch of t * string * (string * t) list

type item =
  | Type of typedef
  | Expr of t
