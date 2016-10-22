let env_get (e : result list) n = List.nth e n

let env_put (e : result list) v = v :: e


type result =
    Value of int
  | Bool of bool
  | Tuple of int * result list
  | Prim of string
  | Lambda of (Instruct.t list * result list)
  | Eplison

let step (c, e, s, r) op =
  match op with
  Instruct.Bool v -> Stack.push (Bool v) s; (c, e, s, r)
  | Instruct.Const v -> Stack.push (Value v) s; (c, e, s, r)
  | Instruct.Prim str -> Stack.push (Prim str) s; (c, e, s, r)
  | Instruct.MakeTuple (tag, n) ->
    let rec loop i n res =
      if i=n then begin Stack.push (Tuple (tag, res)) s; (c, e, s, r) end
      else loop (i+1) n ((Stack.pop s)::res)
    in loop 0 n []
  | Instruct.Field n ->
    (match (Stack.pop s) with
    | Tuple (tag, ls) -> (Stack.push (List.nth ls n) s); (c, e, s, r)
    | _ -> failwith "type infer should kill this case")
  | Instruct.Pop -> Stack.pop s |> ignore; (c, e, s, r)
  | Instruct.Copy -> Stack.push (Stack.top s) s |> ignore; (c, e, s, r)
  | Instruct.Plus -> (match Stack.pop s with
    | Value x -> (match Stack.pop s with
        | Value y -> Stack.push (Value (x+y)) s;
          (c, e, s, r)
        | _ -> failwith "can add non int")
    | _ -> failwith "can add non int")
  | Instruct.Sub -> (match Stack.pop s with
    | Value x -> (match Stack.pop s with
        | Value y -> Stack.push (Value (y-x)) s;
          (c, e, s, r)
        | _ -> failwith "can add non int")
    | _ -> failwith "can add non int")
  | Instruct.Mul -> (match Stack.pop s with
    | Value x -> (match Stack.pop s with
        | Value y -> Stack.push (Value (x*y)) s;
          (c, e, s, r)
        | _ -> failwith "can add non int")
    | _ -> failwith "an add non int")
  | Instruct.Equal -> (match Stack.pop s with
    | Value x -> (match Stack.pop s with
        | Value y -> Stack.push (Bool (if x=y then true else false)) s;
          (c, e, s, r)
        | _ -> failwith "can add non int")
    | _ -> failwith "can add non int")
  | Instruct.Branch (succ, fail) ->
    (match Stack.pop s with
     | Bool x -> if x then
         (succ, e, s, r)
       else (fail, e, s, r)
     | _ -> failwith "branch instruct expect bool type")
  | Instruct.Access n -> Stack.push (env_get e n) s; (c, e, s, r)
  | Instruct.Closure c1 -> Stack.push (Lambda (c1,e)) s; (c, e, s, r)
  | Instruct.Tailapply ->
    let (c1,e1) = match Stack.top s with
        Lambda x -> x
      | _ -> failwith "cannot tailapply with non closure" in
    (c1, e1, s, r)
  | Instruct.Apply ->
    (match Stack.top s with
     | Lambda (c1, e1) ->
       Stack.push (Lambda (c,e)) r;
       (c1, e1, s, r)
     | _ -> failwith "cannot apply non closure")
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
  | Instruct.Switch cases ->
    (match Stack.pop s with
    | Tuple (tag,_) ->
      let code = List.assq tag cases in
      (code, e, s, r)
    | _ -> failwith "switch should get tuple!")
  | _ -> failwith "not implement"

let run code e s r =
  let rec loop (c, e, s, r) =
    match c with
    | [] -> failwith "must stop with Instruct.Stop"
    | [Instruct.Stop] -> Stack.top s
    | op::c1 -> loop (step (c1, e, s, r) op)
  in loop (code, e, s, r)
