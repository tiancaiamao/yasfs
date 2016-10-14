let rec subst t0 v t =
  match t0 with
  | Type.Unit -> Type.Unit
  | Type.Int -> Type.Int
  | Type.Bool -> Type.Bool
  | Type.Var s -> if s = v then t else t0
  | Type.Tuple (tag, tuple) ->
    Type.Tuple (tag, (List.map (fun (i,x) -> (i, (subst x v t))) tuple))
  | Type.Fun (arg,ret) ->
    Type.Fun (subst arg v t, subst ret v t)

let rec update_type t subst =
  match t with
  | Type.Unit -> Type.Unit
  | Type.Int -> Type.Int
  | Type.Bool -> Type.Bool
  | Type.Fun (arg,ret) ->
    Type.Fun (update_type arg subst, update_type ret subst)
  | Type.Tuple (tag, tuple) ->
    Type.Tuple (tag, (List.map (fun (i,x) -> (i, (update_type x subst))) tuple))
  | Type.Var v -> try List.assoc v subst with _ -> t

let extend_subst s v t =
  (v,t)::(List.map (fun (v1,rhs) -> (v1, (subst rhs v t))) s)

let rec occur v t =
  match t with
  | Type.Var v1 -> v = v1
  | Type.Fun (arg,ret) ->
    (occur v arg) || (occur v ret)
  | Type.Tuple (tag, ts) ->
    List.exists (occur v) (List.map (fun (i,v) -> v) ts)
  | _ -> false

(* 'a M -> ('a -> 'b M) -> 'b M *)
let (>>=) aM a2bM = match aM with
  | None -> None
  | Some a -> a2bM a

let rec list_equal l1 l2 =
  match l1 with
  | [] -> (List.length l2) = 0
  | x::xs -> (List.mem x l2) && list_equal xs (List.filter (fun v -> v <> x) l2)

let rec list_union l1 l2 getkey =
  match l1 with
  | [] -> []
  | x::xs ->
    try let y = List.find (fun v -> (getkey v) = (getkey x)) l2 in
      (x, y) :: (list_union xs l2 getkey) with
    _ -> list_union xs l2 getkey

let tag_equal x y =
  match (x, y) with
  | (Type.TNone, Type.TNone) -> true
  | (Type.TExact a, Type.TExact b) -> a=b
  | (Type.TExact a, Type.TOneof l) -> List.mem a l
  | (Type.TOneof l, Type.TExact a) -> List.mem a l
  | (Type.TOneof l1, Type.TOneof l2) -> list_equal l1 l2
  | _ -> false

(* (unifier t1 t2) : ('a -> 'b M) *)
let rec unifier t1 t2 s =
  let ty1 = update_type t1 s in
  let ty2 = update_type t2 s in
  match (ty1,ty2) with
  | (Type.Int,Type.Int) -> Some s
  | (Type.Bool,Type.Bool) -> Some s
  | (Type.Unit,Type.Unit) -> Some s
  | (Type.Var a, Type.Var b) when a=b -> Some s
  | (Type.Var a, _) -> if (occur a ty2) then None
    else Some (extend_subst s a ty2)
  | (_, Type.Var b) -> if (occur b ty1) then None
    else Some (extend_subst s b ty1)
  | (Type.Fun(a1,e1), Type.Fun(a2,e2)) ->
    (Some s)
    >>= (unifier a1 a2)
    >>= (unifier e1 e2)
  | (Type.Tuple (tag1, t1s), Type.Tuple (tag2, t2s)) when tag_equal tag1 tag2 ->
    unifier_tuple_list t1s t2s s
  | _ -> None
and unifier_tuple_list t1s t2s s =
  let common = list_union t1s t2s (function (x,y) -> x) in
  let ls1 = List.map (function ((_,x),(_,y)) -> x) common in
  let ls2 = List.map (function ((_,x),(_,y)) -> y) common in
  unifier_list ls1 ls2 s
and unifier_list t1s t2s s =
  match (t1s,t2s) with
  | ([],[]) -> Some s
  | (x::xs, y::ys) -> (unifier x y s) >>= (unifier_list xs ys)
  | _ -> None

let env_lookup env n = List.assoc n env

let env_extend env n v = (n,v)::env

let (gen_var, reset_var) = let id = ref 96 in
  (fun () -> id := !id + 1; Type.Var (Char.chr !id)),
  (fun () -> id := 96)

let make_type_tuple i n t =
  let rec recur i cur n t l =
    if cur = n then l else
    if cur = i then recur i (cur+1) n t (t::l)
    else recur i (cur+1) n t ((gen_var ())::l)
  in List.rev (recur i 0 n t [])

let rec type_of exp env subst =
  match exp with
  | Ast.Bool _ -> (Type.Bool, subst, env)
  | Ast.Int n -> (Type.Int, subst, env)
  | Ast.Equal (a,b) ->
    let (ta, subst1, env) = (type_of a env subst) in
    let subst2 = (subst1 >>= (unifier ta Type.Int)) in
    let (tb, subst3, env) = (type_of b env subst2) in
    let subst4 = (subst3 >>= (unifier tb Type.Int)) in
    (Type.Bool, subst4, env)
  | Ast.Bind (k,v) ->
    let (t, subst1, env) = type_of v env subst in
    (Type.Unit, subst1, env_extend env k t)
  | Ast.Plus (a,b) ->
    let (ta, subst1, env) = (type_of a env subst) in
    let subst2 = subst1 >>= (unifier ta Type.Int) in
    let (tb, subst3, env) = (type_of b env subst2) in
    let subst4 = subst3 >>= (unifier tb Type.Int) in
    (Type.Int, subst4, env)
  | Ast.Sub (a,b) ->
    let (ta, subst1, env) = (type_of a env subst) in
    let subst2 = subst1 >>= (unifier ta Type.Int) in
    let (tb, subst3, env) = (type_of b env subst2) in
    let subst4 = subst3 >>= (unifier tb Type.Int) in
    (Type.Int, subst4, env)
  | Ast.Mul (a,b) ->
    let (ta, subst1, env) = (type_of a env subst) in
    let subst2 = subst1 >>= (unifier ta Type.Int) in
    let (tb, subst3, env) = (type_of b env subst2) in
    let subst4 = subst3 >>= (unifier tb Type.Int) in
    (Type.Int, subst4, env)
  | Ast.Field (i,e) ->
    let result_type = gen_var () in
    let (te, subst1, env) = (type_of e env subst) in
    let subst2 = (subst1 >>= (unifier te (Type.Tuple (Type.TNone, [(i, result_type)])))) in
    (result_type, subst2, env)
  | Ast.If (a,b,c) ->
    let (ta, subst1, env) = type_of a env subst in
    let subst2 = subst1 >>= (unifier ta Type.Bool) in
    let (tb, subst3, env) = type_of b env subst2 in
    let (tc, subst4, env) = type_of c env subst3 in
    let subst5 = subst4 >>= (unifier tb tc) in
    (tb, subst5, env)
  | Ast.Var n -> ((env_lookup env n), subst, env)
  | Ast.Fun (vars,body) -> (match vars with
      | [] -> failwith "fuck..1"
      | x::[] -> let tv = gen_var () in
        let (ty, subst1, env) = type_of_body body (env_extend env x tv) subst in
        (Type.Fun (tv, ty), subst1, env)
      | x::xs -> let tv = gen_var () in
        let (ty, subst1, env) = type_of (Ast.Fun (xs, body)) (env_extend env x tv) subst in
        (Type.Fun (tv, ty), subst1, env))
  | Ast.Tuple (tag, vs) ->
    let ts = List.mapi (fun i v -> let (t, s, e) = (type_of v env subst) in (i, t)) vs in
    (Type.Tuple (Type.TNone, ts), subst, env)

  (* | Ast.Switch (v, tn, cases) -> *)
  (*   let (tv, subst1, env) = type_of v env subst in *)
  (*   let ty = Hashtbl.find Global.g_t_env tn in *)
  (*   let subst2 = subst >>= unifier tv ty in *)
  (*   (ty, subst2, env) *)
  | Ast.Fun1 (vars, body) ->
    let (t, subst1, env) = type_of (Ast.Fun (vars, body)) env subst in
    (match t with
    | Type.Fun (self, _) -> (self, subst1, env)
    | _ -> failwith "Ast.fun1 fail")
  | Ast.App (rator,rands) -> type_of_app rator (List.rev rands) env subst
and type_of_body ls env subst =
  match ls with
  | [] -> failwith "fuck you"
  | x::[] -> type_of x env subst
  | x::xs ->
    let (tx, subst1, env) = (type_of x env subst) in
    let subst2 = subst1 >>= unifier tx Type.Unit in
    type_of_body xs env subst2
and type_of_app rator rev_rands env subst =
  (match rev_rands with
   | [] -> failwith "should never run here"
   | x::[] ->
     let result_type = gen_var () in
     let (rator_type, subst1, env) = type_of rator env subst in
     let (rand_type, subst2, env) = type_of x env subst1 in
     let subst3 = subst2 >>= (unifier rator_type (Type.Fun (rand_type, result_type))) in
     (result_type, subst3, env)
   | x::xs ->
     let result_type = gen_var () in
     let (rator_type, subst1, env) = type_of_app rator xs env subst in
     let (rand_type, subst2, env) = type_of x env subst1 in
     let subst3 = subst2 >>= (unifier rator_type (Type.Fun (rand_type, result_type))) in
     (result_type, subst3, env))

let infer exp =
  let () = reset_var () in
  let (ty, subst, env) = type_of exp [] (Some []) in
  match subst with
  | Some s -> update_type ty s
  | None -> failwith "false"

let infer_list es =
  let rec aux_infer es env subst =
    match es with
    | [] -> failwith "should not here"
    | x::[] -> let (ty, subst, env) = type_of x env subst in
      (match subst with
       | Some s -> (update_type ty s), env
       | None -> failwith "false")
    | x::xs -> let (ty, subst, env) = type_of x env subst in
      (match subst with
       | Some s -> aux_infer xs env subst
       | None -> failwith "false") in
  let (t, env) = aux_infer es [] (Some []) in
  t
