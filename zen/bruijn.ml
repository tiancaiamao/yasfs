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
  | Ast.String s -> Lambda.String s
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
  | Ast.Div (t1, t2) -> Lambda.Div (
      (ast2lambda env t1), (ast2lambda env t2))
  | Ast.Field (n, t) -> Lambda.Field (n, ast2lambda env t)
  | Ast.Equal (t1, t2) -> Lambda.Equal (
      (ast2lambda env t1), (ast2lambda env t2))
  | Ast.Bind (n, t) -> ast2lambda (extend_env env [n]) t
  | Ast.If (test, succ, fail) -> Lambda.If (
      (ast2lambda env test), (ast2lambda env succ), (ast2lambda env fail))
  | Ast.Tuple (name, ts) ->
    let tag = match name with
      | Some str -> Global.name2tag str
      | None -> 0
    in Lambda.Tuple (tag, (List.map (ast2lambda env) ts))
  | Ast.Switch (t, cs) ->
    let body = List.map (fun (n, v) -> (Global.name2tag n, (ast2lambda env v))) cs in
    Lambda.Switch (ast2lambda env t, body)
