let find_env e v =
  let rec find e v i =
    match e with
    | [] -> None
    | x::xs -> if x = v then Some(i) else find xs v (i+1)
  in find e v 0

let extend_env env v = (List.rev v) @ env

let rec find_in_list name lst i =
  match lst with
  | [] -> failwith "name not exist in list"
  | (n,_)::xs -> if n=name then i else find_in_list name xs (i+1)

let (empty_env : string list) = []

let rec ast2lambda env ast = match ast with
  | Ast.Bool v -> Lambda.Bool v
  | Ast.Int v -> Lambda.Int v
  | Ast.App (t, ts) -> Lambda.App (ast2lambda env t, List.map (ast2lambda env) ts)
  | Ast.Fun (ts, t) ->
    Lambda.Fun (List.length ts,
                let e = extend_env env ts in
                List.map (ast2lambda e) t)
  | Ast.Fun1 (ts, t) ->
    Lambda.Fun1 (List.length ts,
                 let e = extend_env env ts in
                 List.map (ast2lambda e) t)
  | Ast.Var s -> (match find_env env s with
      | Some i -> Lambda.Var i
      | None -> failwith "cannot handle free variable")
  | Ast.Plus (t1, t2) -> Lambda.Plus (
      (ast2lambda env t1), (ast2lambda env t2))
  | Ast.Sub (t1, t2) -> Lambda.Sub (
      (ast2lambda env t1), (ast2lambda env t2))
  | Ast.Mul (t1, t2) -> Lambda.Mul (
      (ast2lambda env t1), (ast2lambda env t2))
  | Ast.Equal (t1, t2) -> Lambda.Equal (
      (ast2lambda env t1), (ast2lambda env t2))
  | Ast.Bind (n, t) -> ast2lambda (extend_env env [n]) t
  | Ast.If (test, succ, fail) -> Lambda.If (
      (ast2lambda env test), (ast2lambda env succ), (ast2lambda env fail))
  | Ast.Tuple (tag, ts) ->
    let tag1 = (match tag with
        | Some str -> 42 (* Hashtbl.find Global.g_t_env str *)
        | None -> 0)
    in Lambda.Tuple (tag1, (List.map (ast2lambda env) ts))

(*   | Ast.Switch (n, tn, cs) -> *)
(*     let ty = Hashtbl.find Global.g_t_env tn in *)
(*     let tag = Lambda.App (Lambda.Prim "tag", [ast2lambda env n]) in *)
(*     let body = (List.map (fun (n,t) -> case2lambda (Type.Field n) t ty env) cs) in *)
(*     Lambda.Switch (tag, body) *)
(* and case2lambda field term ty env = *)
(*     let idx = field2index field ty in *)
(*     let lam = ast2lambda env term in *)
(*     (idx, lam) *)
