module Ast = struct
  type t =
      Int of int
    | Var of string
    | App of t * t list
    | Fun of string list * t
end

module Lambda = struct
  type t =
      Int of int
    | Var of int
    | App of t * t list
    | Fun of int * t
end

module Instruct = struct
  type t =
      Const of int
    | StackAccess of int
    | EnvAccess of int
    | Closure of t list
    | Grab
    | Return
    | Restart
    | Endlet
    | Let
    | Apply
    | TailApply
    | Pushretaddr of t list
    | Stop
end

let find_env e v =
  let rec find e v i =
    match e with
    | [] -> None
    | x::xs -> if x = v then Some(i) else find xs v (i+1)
  in find e v 0

let extend_env env v = v @ env

let (empty_env : string list) = []

let rec ast2lambda env ast = match ast with
  | Ast.Int v -> Lambda.Int v
  | Ast.App (t, ts) -> Lambda.App (ast2lambda env t, List.map (ast2lambda env) ts)
  | Ast.Fun (ts, t) -> Lambda.Fun (List.length ts, ast2lambda (extend_env env ts) t)
  | Ast.Var s -> match find_env env s with
    | Some i -> Lambda.Var i
    | None -> failwith "cannot handle free variable"

let rec compile exp code = match exp with
    Lambda.Int v -> (Instruct.Const v)::code
  | Lambda.Var n -> (Instruct.EnvAccess n)::code
  | Lambda.Fun (n,t) -> (Instruct.Closure (compile_tail exp n))::code
  | Lambda.App (t,ts) ->
    let body = ts |> List.rev |> List.map (fun x -> compile x []) |> List.flatten in
    (Instruct.Pushretaddr code)::(body @ (compile t [Instruct.TailApply]))
and compile_tail exp threshold = match exp with
    Lambda.Int v -> [Instruct.Const v]
  | Lambda.Var n ->
    let access = if n < threshold then Instruct.StackAccess n else Instruct.EnvAccess n in
    [access; Instruct.Return]
  | Lambda.Fun (n,t) -> Instruct.Grab::(compile_tail t n)
  | Lambda.App (t,ts) ->
    let fold_func code exp = compile exp code in
    let rev_ts = List.rev ts in
    List.fold_left fold_func (compile t [Instruct.TailApply]) rev_ts

type result =
    Value of int
  | Lambda of (Instruct.t list * result list)

let env_get (e : result list) n = List.nth e n

let env_put (e : result list) v = v :: e

let rec run code e s =
  match code with
  | [] -> failwith "should stop with STOP"
  | x::c -> match x with
      Instruct.Stop -> Stack.top s
    | Instruct.Const v -> Stack.push (Value v) s; run c e s
    | Instruct.StackAccess n -> Stack.push (env_get e n) s; run c e s
    | Instruct.Closure c1 -> Stack.push (Lambda (c1,e)) s; run c e s
    | Instruct.Apply ->
      let v = Stack.pop s in
      let (c1,e1) = match Stack.pop s with
          Value _ -> failwith "cannot apply non closure"
        | Lambda x -> x in
      Stack.push (Lambda (c,e)) s;
      run c1 (env_put e1 v) s
    | Instruct.Return ->
      let v = Stack.pop s in
      let (c1, e1) = match Stack.pop s with
          Value _ -> failwith "not a saved return address"
        | Lambda x -> x in
      Stack.push v s;
      run c1 e1 s
    | _ -> failwith "not implement"

(* let input0 = (Lambda.App ((Lambda.Fun (1,Lambda.Var 0)), [(Lambda.Int 5)]));; *)
(* let input1 = compile input0 [Instruct.Stop];; *)
(* let output = run input1 [] (Stack.create ());; *)
