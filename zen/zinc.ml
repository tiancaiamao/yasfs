let rec compile exp code = match exp with
    Lambda.Int v -> (Instruct.Const v)::code
  | Lambda.Bool v -> (Instruct.Bool v)::code
  | Lambda.Tuple (tag, vs) -> let n = (List.length vs) in
    (List.flatten (List.map (fun x -> compile x []) vs)) @
    (Instruct.MakeTuple (tag,n))::code
  | Lambda.Var n -> (Instruct.Access n)::code
  | Lambda.Bind t -> compile t (Instruct.Bind::code)
  | Lambda.Fun (n,ts) -> (Instruct.Closure (compile_tail exp))::code
  | Lambda.Fun1 (n,ts) ->
    let body = [Instruct.Grab n] @ (compile_body ts) in
        (Instruct.Closure body)::code
  | Lambda.App (t,ts) ->
    let init = compile t [Instruct.Apply] in
    let f = (fun a b -> compile b (Instruct.Push::a)) in
    (Instruct.PushRetAddr code)::(List.fold_left f init ts)
  | Lambda.If (t, succ, fail) ->
    compile t [Instruct.Branch ((compile succ code), (compile fail code))]
  | Lambda.Switch (t, cases) ->
    compile t [Instruct.Switch
                 (List.map (fun (i, x) -> (i, (compile x code))) cases)]
  | Lambda.Prim s -> (Instruct.Prim s)::code
  | Lambda.Plus (a, b) ->
    compile b (Instruct.Push::(compile a (Instruct.Plus::code)))
  | Lambda.Sub (a, b) ->
    compile a (compile b (Instruct.Sub::code))
  | Lambda.Mul (a, b) ->
    compile a (compile b (Instruct.Mul::code))
  | Lambda.Field (n, b) ->
    compile b ((Instruct.Field n)::code)
  | Lambda.Equal (a, b) ->
    compile a (compile b (Instruct.Equal::code))
and compile_tail exp = match exp with
    Lambda.Int v -> [Instruct.Const v]
  | Lambda.Bool v -> [Instruct.Bool v]
  | Lambda.Prim s -> [Instruct.Prim s]
  | Lambda.Var n -> [Instruct.Access n; Instruct.Return]
  | Lambda.Bind t -> [Instruct.Bind]
  | Lambda.Switch _ -> compile exp [Instruct.Return]
  | Lambda.Tuple _ -> compile exp [Instruct.Return]
  | Lambda.Plus _ -> compile exp []
  | Lambda.Sub _ -> compile exp [Instruct.Return]
  | Lambda.Mul _ -> compile exp [Instruct.Return]
  | Lambda.Equal _ -> compile exp [Instruct.Return]
  | Lambda.Field _ -> compile exp [Instruct.Return]
  | Lambda.If _ -> compile exp [Instruct.Return]
  | Lambda.Fun (n,ts) ->
    [Instruct.Grab n] @ (compile_body ts) @ [Instruct.Return]
    (* | [t] -> (match n with *)
    (*     | 0 -> compile_tail t *)
    (*     | _ -> (Instruct.Grab n)::(compile_tail (Lambda.Fun (n-1,[t])))) *)
    (* | _ -> failwith "must be one") *)
  | Lambda.Fun1 (n,ts) -> (match ts with
    | [t] -> (match n with
        | 0 -> compile_tail t
        | _ -> (Instruct.Grab n)::(compile_tail (Lambda.Fun (n-1,[t]))))
    | _ -> failwith "must be one")
  | Lambda.App (t,ts) ->
    (t::ts
     |> List.rev
     |> List.map (fun x -> compile x [])
     |> List.flatten) @ [Instruct.Tailapply]
and compile_body ts = match ts with
  | [x] -> compile x []
  | x::xs -> compile x (compile_body xs)
  | [] -> []
