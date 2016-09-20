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
    | Access of int
    | Closure of t list
    | Tailapply
    | Apply
    | Pushmark
    | Grab
    | Return
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
  | Lambda.Var n -> (Instruct.Access n)::code
  | Lambda.Fun (n,t) -> (Instruct.Closure (compile_tail exp))::code
  | Lambda.App (t,ts) ->
    let init = compile t (Instruct.Apply::code) in
    Instruct.Pushmark::(List.fold_left (fun a b -> compile b a) init ts)
and compile_tail exp = match exp with
    Lambda.Int v -> [Instruct.Const v]
  | Lambda.Var n -> [Instruct.Access n; Instruct.Return]
  | Lambda.Fun (n,t) -> (match n with
      0 -> (compile_tail t)
    | _ -> Instruct.Grab::(compile_tail (Lambda.Fun (n-1,t))))
  | Lambda.App (t,ts) ->
    (t::ts
     |> List.rev
     |> List.map (fun x -> compile x [])
     |> List.flatten) @ [Instruct.Tailapply]

type result =
    Value of int
  | Lambda of (Instruct.t list * result list)
  | Eplison

let env_get (e : result list) n = List.nth e n

let env_put (e : result list) v = v :: e

let step (c, e, s, r) op =
  match op with
    Instruct.Const v -> Stack.push (Value v) s; (c, e, s, r)
  | Instruct.Access n -> Stack.push (env_get e n) s; (c, e, s, r)
  | Instruct.Closure c1 -> Stack.push (Lambda (c1,e)) s; (c, e, s, r)
  | Instruct.Tailapply ->
    let (c1,e1) = match Stack.pop s with
        Lambda x -> x
      | _ -> failwith "cannot tailapply with non closure" in
    (c1, e1, s, r)
  | Instruct.Apply ->
    Stack.push (Lambda (c,e)) r;
    let (c1,e1) = match Stack.pop s with
        Lambda x -> x
      | _ -> failwith "cannot apply non closure" in
    (c1, e1, s, r)
  | Instruct.Pushmark -> Stack.push Eplison s; (c, e, s, r)
  | Instruct.Grab ->
    if Stack.top s = Eplison then
      (Stack.pop s |> ignore;
       Stack.push (Lambda (Instruct.Grab::c,e)) s;
       (let (c1,e1) = match Stack.pop r with
             Lambda x -> x
           | _ -> failwith "should be closure in r" in
        (c1, e1, s, r)))
    else let v = Stack.pop s in
      (c, (env_put e v), s, r)
  | Instruct.Return ->
    let v = Stack.pop s in
    (if Stack.top s = Eplison then
      (Stack.pop s |> ignore; Stack.push v s;
       let (c1,e1) = match Stack.pop r with
           Lambda x -> x
         | _ -> failwith "should be closure in return" in
       (c1, e1, s, r))
    else match v with
        Lambda (c1,e1) -> (c1, e1, s, r)
      | _ -> failwith "should be")
  | _ -> failwith "not implement"

let step1 ((op::c), e, s, r) = step (c, e, s, r) op

let state c = (c, [], (Stack.create ()), (Stack.create ()))

let run code e s r =
  let rec loop (c, e, s, r) =
    match c with
    | [] -> failwith "must stop with Instruct.Stop"
    | [Instruct.Stop] -> Stack.top s
    | op::c1 -> loop (step (c1, e, s, r) op)
  in loop (code, e, s, r)

let easy_run code = run code [] (Stack.create ()) (Stack.create ())

let input0 = Lambda.App ((Lambda.Fun (2,Lambda.Var 1)), [Lambda.Int 3; Lambda.Int 5])

let s0 = compile input0 [Instruct.Stop]

(* let input0 = Lambda.App ((Lambda.App ((Lambda.Fun (2,Lambda.Var 1)), [Lambda.Int 3])), [Lambda.Int 5]);; *)
(* let input1 = compile input0 [Instruct.Stop];; *)
(* let output = run input1 [] (Stack.create ());; *)
