let rec compile exp code threshold = match exp with
    Lambda.Int v -> (Instruct.Const v)::code
  | Lambda.Bool v -> (Instruct.Bool v)::code
  | Lambda.String s -> (Instruct.String s)::code
  | Lambda.Tuple (tag, vs) ->
    let n = (List.length vs) in
    if n = 0 then Instruct.MakeTuple (tag, 0)::code else
      let fn a b = compile b (Instruct.Push::a) threshold in
      let init = compile (List.hd vs) ((Instruct.MakeTuple (tag,n))::code) threshold in
      List.fold_left fn init (List.tl vs)
  | Lambda.Var n ->
    if n < threshold
    then (Instruct.StackAccess n)::code
    else (Instruct.EnvAccess (n-threshold))::code
  | Lambda.Bind t -> compile t (Instruct.Bind::code) threshold
  | Lambda.Fun (n,ts) -> (Instruct.Closure (compile_tail exp n))::code
  | Lambda.Fun1 (n,ts) -> (Instruct.Closure (compile_tail exp n))::code
  | Lambda.App (t,ts) ->
    let init = compile t [Instruct.Apply] threshold in
    let f = (fun a b -> compile b (Instruct.Push::a) threshold) in
    (Instruct.PushRetAddr code)::(List.fold_left f init ts)
  | Lambda.CCall (prim,ts) ->
    let init = Instruct.String prim::(Instruct.CCall (List.length ts))::code in
    let f = (fun a b -> compile b (Instruct.Push::a) threshold) in
    List.fold_left f init ts
  | Lambda.If (t, succ, fail) ->
    compile t [Instruct.Branch ((compile succ code threshold), (compile fail code threshold))] threshold
  | Lambda.Switch (t, cases) ->
    compile t [Instruct.Switch
                 (List.map (fun (i, x) -> (i, (compile x code threshold))) cases)] threshold
  | Lambda.Plus (a, b) ->
    compile a (Instruct.Push::(compile b (Instruct.Plus::code)) threshold) threshold
  | Lambda.Sub (a, b) ->
    compile a (Instruct.Push::(compile b (Instruct.Sub::code)) threshold) threshold
  | Lambda.Mul (a, b) ->
    compile a (Instruct.Push::(compile b (Instruct.Mul::code)) threshold) threshold
  | Lambda.Div (a, b) ->
    compile a (Instruct.Push::(compile b (Instruct.Div::code)) threshold) threshold
  | Lambda.Field (n, b) ->
    compile b ((Instruct.Field n)::code) threshold
  | Lambda.Equal (a, b) ->
    compile a (Instruct.Push::(compile b (Instruct.Equal::code) threshold)) threshold
and compile_tail exp threshold = match exp with
    Lambda.Int v -> [Instruct.Const v; Instruct.Return]
  | Lambda.Bool v -> [Instruct.Bool v; Instruct.Return]
  | Lambda.String s -> [Instruct.String s; Instruct.Return]
  | Lambda.CCall _ -> compile exp [Instruct.Return] threshold
  | Lambda.Var n -> if n < threshold
    then [Instruct.StackAccess n; Instruct.Return]
    else [Instruct.EnvAccess (n-threshold); Instruct.Return]
  | Lambda.Bind t -> [Instruct.Bind; Instruct.Return]
  | Lambda.Switch _ -> compile exp [Instruct.Return] threshold
  | Lambda.Tuple _ -> compile exp [Instruct.Return] threshold
  | Lambda.Plus _ -> compile exp [Instruct.Return] threshold
  | Lambda.Sub _ -> compile exp [Instruct.Return] threshold
  | Lambda.Mul _ -> compile exp [Instruct.Return] threshold
  | Lambda.Div _ -> compile exp [Instruct.Return] threshold
  | Lambda.Equal _ -> compile exp [Instruct.Return] threshold
  | Lambda.Field _ -> compile exp [Instruct.Return] threshold
  | Lambda.If (t,succ,fail) ->
    compile t [Instruct.Branch ((compile_tail succ threshold), (compile_tail fail threshold))] threshold
  | Lambda.Fun (n,ts) ->
    (Instruct.Grab n)::(compile_body ts n)
  | Lambda.Fun1 (n,ts) ->
    [Instruct.Grab (n-1); Instruct.Push] @ (compile_body ts n) @ [Instruct.Return]
  | Lambda.App (t,ts) ->
    let init = compile t [Instruct.Apply] threshold in
    let f = (fun a b -> compile b (Instruct.Push::a) threshold) in
    (List.fold_left f init ts)
and compile_body ts threshold = match ts with
  | [x] -> compile x [Instruct.Return] threshold
  (* | [x] -> compile_tail x threshold *)
  | x::xs -> compile x (compile_body xs threshold) threshold
  | [] -> []
