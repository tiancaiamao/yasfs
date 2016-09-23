let rec compile exp code = match exp with
    Lambda.Int v -> (Instruct.Const v)::code
  | Lambda.Var n -> (Instruct.Access n)::code
  | Lambda.Bind t -> compile t (Instruct.Bind::code)
  | Lambda.Fun (n,ts) -> (Instruct.Closure (List.flatten (List.map compile_tail ts)))::code
  | Lambda.App (t,ts) ->
    let init = compile t (Instruct.Apply::code) in
    Instruct.Pushmark::(List.fold_left (fun a b -> compile b a) init ts)
and compile_tail exp = match exp with
    Lambda.Int v -> [Instruct.Const v]
  | Lambda.Var n -> [Instruct.Access n; Instruct.Return]
  | Lambda.Bind t -> [Instruct.Bind]
  | Lambda.Fun (n,ts) -> (match n with
      0 -> List.flatten (List.map compile_tail ts)
    | _ -> Instruct.Grab::(compile_tail (Lambda.Fun (n-1,ts))))
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

let run code e s r =
  let rec loop (c, e, s, r) =
    match c with
    | [] -> failwith "must stop with Instruct.Stop"
    | [Instruct.Stop] -> Stack.top s
    | op::c1 -> loop (step (c1, e, s, r) op)
  in loop (code, e, s, r)
