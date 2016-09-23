let find_env e v =
  let rec find e v i =
    match e with
    | [] -> None
    | x::xs -> if x = v then Some(i) else find xs v (i+1)
  in find e v 0

let extend_env env v = (List.rev v) @ env

let (empty_env : string list) = []

let rec ast2lambda env ast = match ast with
  | Ast.Int v -> Lambda.Int v
  | Ast.App (t, ts) -> Lambda.App (ast2lambda env t, List.map (ast2lambda env) ts)
  | Ast.Fun (ts, t) -> Lambda.Fun (List.length ts,
        let e = extend_env env ts in
          List.map (ast2lambda e) t)
  | Ast.Var s -> (match find_env env s with
    | Some i -> Lambda.Var i
    | None -> failwith "cannot handle free variable")
  | Ast.Bind (n, t) -> ast2lambda (extend_env env [n]) t
