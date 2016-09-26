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
         | Var of string
           (* | Any *)
           (* | Error of string *)
end

(* 把出现在ty0中的tvar都用ty替换掉，返回ty0被更新之后的类型 *)
let rec apply_one_subst ty0 tvar ty =
  match ty0 with
  | Type.Int -> Type.Int
  | Type.Bool -> Type.Bool
  | Type.Fun (arg,ret) ->
    Type.Fun (apply_one_subst arg tvar ty, apply_one_subst ret tvar ty)
  | Type.Var s -> if ty0 = tvar then ty else ty0

(* 用subst里面的(变量，类型)列表，更新ty *)
let rec apply_subst_to_type ty subst =
  match ty with
  | Type.Int -> Type.Int
  | Type.Bool -> Type.Bool
  | Type.Fun (arg,ret) ->
    Type.Fun (apply_subst_to_type arg subst, apply_subst_to_type ret subst)
  | Type.Var s -> try List.assoc ty subst with _ -> ty

let empty_subst () = []

(* 用tvar ty更新subst *)
let extend_subst subst tvar ty =
  let update (lhs,rhs) = lhs, (apply_one_subst rhs tvar ty) in
  (tvar,ty)::(List.map update subst)

let tvar_type ty1 = match ty1 with Type.Var s -> true | _ -> false

let proc_type ty1 = match ty1 with Type.Fun _ -> true | _ -> false

(* 判断tvar是在ty中出现 *)
let rec no_occurrence tvar ty =
  match tvar with
  | Type.Int -> true
  | Type.Bool -> true
  | Type.Fun (arg,ret) ->
    (no_occurrence tvar arg) && (no_occurrence tvar ret)
  | Type.Var s -> tvar <> ty

let report_no_occurrence_violation ty1 ty2 exp =
  failwith "xxx"

let proc_type_arg x = match x with Type.Fun (arg,ret) -> arg | _ -> failwith "never here"

let proc_type_ret x = match x with Type.Fun (arg,ret) -> ret | _ -> failwith "never here"

(* 解方程t1等于t2，更新加到subst里面。subst里面是已经计算出来了的替换 *)
let rec unifier t1 t2 subst exp =
  let ty1 = apply_subst_to_type t1 subst in
  let ty2 = apply_subst_to_type t2 subst in
  if ty1=ty2 then subst
  else if tvar_type t1 then
    if no_occurrence ty1 ty2
    then extend_subst subst ty1 ty2
    else report_no_occurrence_violation ty1 ty2 exp
  else if tvar_type t2 then
    if no_occurrence ty2 ty1
    then extend_subst subst ty2 ty1
    else report_no_occurrence_violation ty2 ty1 exp
  else if proc_type ty1 && proc_type ty2 then
    let subst1 = unifier (proc_type_arg ty1) (proc_type_arg ty2) subst exp in
    unifier (proc_type_ret ty1) (proc_type_ret ty2) subst1 exp
  else report_no_occurrence_violation ty1 ty2 exp

let env_lookup env n = List.assoc n env

let env_extend env n v = (n,v)::env

let rec type_of exp env subst = match exp with
  | Ast.Bool _ -> (Type.Bool, subst)
  | Ast.Int n -> (Type.Int, subst)
  | Ast.Equal (a,b) ->
    let (ta, subst1) = (type_of a env subst) in
    let subst2 = (unifier ta Type.Int subst1 a) in
    let (tb, subst3) = (type_of a env subst2) in
    let subst4 = (unifier tb Type.Int subst3 b) in
    (Type.Bool, subst4)
  | Ast.Plus (a,b) ->
    let (ta, subst1) = (type_of a env subst) in
    let subst2 = (unifier ta Type.Int subst1 a) in
    let (tb, subst3) = (type_of b env subst2) in
    let subst4 = (unifier tb Type.Int subst3 b) in
    (Type.Int, subst4)
  | Ast.If (a,b,c) ->
    let (ta, subst1) = (type_of a env subst) in
    let subst2 = unifier ta Type.Bool subst1 a in
    let (tb, subst3) = (type_of b env subst2) in
    let (tc, subst4) = (type_of c env subst3) in
    let subst5 = unifier tb tc subst4 exp in
    (tb, subst5)
  | Ast.Var n -> ((env_lookup env n), subst)
  | Ast.Fun (var,body) ->
    let tv = Type.Var var in
    let (ty, subst1) = type_of body (env_extend env var tv) subst in
    (Type.Fun (tv,ty), subst1)
  | Ast.App (rator,rand) ->
    let result_type = Type.Var "ret" in
    let (rator_type, subst1) = type_of rator env subst in
    let (rand_type, subst2) = type_of rand env subst1 in
    let subst3 = unifier rator_type (Type.Fun (rand_type, result_type)) subst2 exp in
    (result_type, subst3)

let infer exp = let (ty, subst) = type_of exp [] [] in
  apply_subst_to_type ty subst

let output4 = type_of (Ast.Plus ((Ast.Var "x"), (Ast.Var "y"))) [("x",Type.Var "x"); ("y",Type.Var "y")] [];;


let input2 = Ast.Fun ("f", Ast.App (Ast.Var "f", Ast.Int 11))
let output2 = infer input2

(* let input1 = Ast.Fun ("f", Ast.Fun ("x", *)
(*                              Ast.Plus(Ast.App (Ast.Var "f", Ast.Int 3), *)
(*                                       Ast.App (Ast.Var "f", Ast.Var "x")))) *)
(* let output1 = infer input1 *)


(* let input3 = Ast.Fun ("x", Ast.If ((Ast.Var "x"), (Ast.Plus ((Ast.Var "x"), (Ast.Int 1))), Ast.Int 0)) *)

