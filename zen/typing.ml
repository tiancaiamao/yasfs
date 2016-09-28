module Ast = struct
  type t =
      Int of int
    | Bool of bool
    | Var of string
    | App of t * t
    | Fun of string * t
    | If of t * t * t
    | Plus of t * t
    | Equal of t * t
end

module Type = struct
  type t = Int | Bool
         | Fun of t * t
         | Var of int
end

let rec subst t0 v t =
  match t0 with
  | Type.Int -> Type.Int
  | Type.Bool -> Type.Bool
  | Type.Fun (arg,ret) ->
    Type.Fun (subst arg v t, subst ret v t)
  | Type.Var s -> if t0 = v then t else t0

let rec update_type t subst =
  match t with
  | Type.Int -> Type.Int
  | Type.Bool -> Type.Bool
  | Type.Fun (arg,ret) ->
    Type.Fun (update_type arg subst, update_type ret subst)
  | Type.Var s -> try List.assoc t subst with _ -> t

let extend_subst s v t =
  let update (lhs,rhs) = lhs, (subst rhs v t) in
  (v,t)::(List.map update s)

let rec occur v t =
  match v with
  | Type.Int -> false
  | Type.Bool -> false
  | Type.Fun (arg,ret) ->
    (occur v arg) || (occur v ret)
  | Type.Var s -> v = t

(* 'a M -> ('a -> 'b M) -> 'b M *)
let (>>=) aM a2bM = match aM with
  | None -> None
  | Some a -> a2bM a

(* (unifier t1 t2) : ('a -> 'b M) *)
let rec unifier t1 t2 s =
  let ty1 = update_type t1 s in
  let ty2 = update_type t2 s in
  match (ty1,ty2) with
  | (Type.Int,Type.Int) -> Some s
  | (Type.Bool,Type.Bool) -> Some s
  | (Type.Var a, Type.Var b) when a=b -> Some s
  | (Type.Var _, _) -> if (occur ty1 ty2) then None
    else Some (extend_subst s ty1 ty2)
  | (_, Type.Var _) -> if (occur ty2 ty1) then None
    else Some (extend_subst s ty2 ty1)
  | (Type.Fun(a1,e1), Type.Fun(a2,e2)) ->
    (Some s)
    >>= (unifier a1 a2)
    >>= (unifier e1 e2)
  | _ -> None

let env_lookup env n = List.assoc n env

let env_extend env n v = (n,v)::env

(* let rec type_of exp env subst = *)
(*   match exp with *)
(*   | Ast.Bool _ -> (Type.Bool, subst) *)
(*   | Ast.Int n -> (Type.Int, subst) *)
(*   | Ast.Equal (a,b) -> *)
(*     let (ta, subst1) = (type_of a env subst) in *)
(*     let subst2 = (unifier ta Type.Int subst1 a) in *)
(*     let (tb, subst3) = (type_of a env subst2) in *)
(*     let subst4 = (unifier tb Type.Int subst3 b) in *)
(*     (Type.Bool, subst4) *)
(*   | Ast.Plus (a,b) -> *)
(*     let (ta, subst1) = (type_of a env subst) in *)
(*     let subst2 = (unifier ta Type.Int subst1 a) in *)
(*     let (tb, subst3) = (type_of b env subst2) in *)
(*     let subst4 = (unifier tb Type.Int subst3 b) in *)
(*     (Type.Int, subst4) *)
(*   | Ast.If (a,b,c) -> *)
(*     let (ta, subst1) = (type_of a env subst) in *)
(*     let subst2 = unifier ta Type.Bool subst1 a in *)
(*     let (tb, subst3) = (type_of b env subst2) in *)
(*     let (tc, subst4) = (type_of c env subst3) in *)
(*     let subst5 = unifier tb tc subst4 exp in *)
(*     (tb, subst5) *)
(*   | Ast.Var n -> ((env_lookup env n), subst) *)
(*   | Ast.Fun (var,body) -> *)
(*     let tv = Type.Var var in *)
(*     let (ty, subst1) = type_of body (env_extend env var tv) subst in *)
(*     (Type.Fun (tv,ty), subst1) *)
(*   | Ast.App (rator,rand) -> *)
(*     let result_type = Type.Var "ret" in *)
(*     let (rator_type, subst1) = type_of rator env subst in *)
(*     let (rand_type, subst2) = type_of rand env subst1 in *)
(*     let subst3 = unifier rator_type (Type.Fun (rand_type, result_type)) subst2 exp in *)
(*     (result_type, subst3) *)

(* let infer exp = let (ty, subst) = type_of exp [] [] in *)
(*   apply_subst_to_type ty subst *)

(* let output4 = type_of (Ast.Plus ((Ast.Var "x"), (Ast.Var "y"))) [("x",Type.Var "x"); ("y",Type.Var "y")] [];; *)


(* let input2 = Ast.Fun ("f", Ast.App (Ast.Var "f", Ast.Int 11)) *)
(* let output2 = infer input2 *)

(* let input1 = Ast.Fun ("f", Ast.Fun ("x", *)
(*                              Ast.Plus(Ast.App (Ast.Var "f", Ast.Int 3), *)
(*                                       Ast.App (Ast.Var "f", Ast.Var "x")))) *)
(* let output1 = infer input1 *)


(* let input3 = Ast.Fun ("x", Ast.If ((Ast.Var "x"), (Ast.Plus ((Ast.Var "x"), (Ast.Int 1))), Ast.Int 0)) *)

