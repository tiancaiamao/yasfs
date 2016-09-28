let rec subst t0 v t =
  match t0 with
  | Type.Int -> Type.Int
  | Type.Bool -> Type.Bool
  | Type.Var s -> if s = v then t else t0
  | Type.Fun (arg,ret) ->
    Type.Fun (subst arg v t, subst ret v t)

let rec update_type t subst =
  match t with
  | Type.Int -> Type.Int
  | Type.Bool -> Type.Bool
  | Type.Fun (arg,ret) ->
    Type.Fun (update_type arg subst, update_type ret subst)
  | Type.Var v -> try List.assoc v subst with _ -> t

let extend_subst s v t =
  (v,t)::(List.map (fun (v1,rhs) -> (v1, (subst rhs v t))) s)

let rec occur v t =
  match t with
  | Type.Var v1 -> v = v1
  | Type.Fun (arg,ret) ->
    (occur v arg) || (occur v ret)
  | _ -> false

(* 'a M -> ('a -> 'b M) -> 'b M *)
let (>>=) aM a2bM = match aM with
  | None -> None
  | Some a -> a2bM a

(* (unifier t1 t2) : ('a -> 'b M) *)
let rec unifier t1 t2 s =
  let ty1 = update_type t1 s in
  let ty2 = update_type t2 s in
  match (ty1,ty2) with
  | (Type.Int,Type.Int) -> Some s
  | (Type.Bool,Type.Bool) -> Some s
  | (Type.Var a, Type.Var b) when a=b -> Some s
  | (Type.Var a, _) -> if (occur a ty2) then None
    else Some (extend_subst s a ty2)
  | (_, Type.Var b) -> if (occur b ty1) then None
    else Some (extend_subst s b ty1)
  | (Type.Fun(a1,e1), Type.Fun(a2,e2)) ->
    (Some s)
    >>= (unifier a1 a2)
    >>= (unifier e1 e2)
  | _ -> None

let env_lookup env n = List.assoc n env

let env_extend env n v = (n,v)::env

let gen_var = let id = ref 96 in
  fun () -> id := !id + 1; Type.Var (Char.chr !id)

let rec type_of exp env subst =
  match exp with
  | Ast.Bool _ -> (Type.Bool, subst)
  | Ast.Int n -> (Type.Int, subst)
  | Ast.Equal (a,b) ->
    let (ta, subst1) = (type_of a env subst) in
    let subst2 = (subst1 >>= (unifier ta Type.Int)) in
    let (tb, subst3) = (type_of b env subst2) in
    let subst4 = (subst3 >>= (unifier tb Type.Int)) in
    (Type.Bool, subst4)
  | Ast.Plus (a,b) ->
    let (ta, subst1) = (type_of a env subst) in
    let subst2 = subst1 >>= (unifier ta Type.Int) in
    let (tb, subst3) = (type_of b env subst2) in
    let subst4 = subst3 >>= (unifier tb Type.Int) in
    (Type.Int, subst4)
  | Ast.If (a,b,c) ->
    let (ta, subst1) = type_of a env subst in
    let subst2 = subst1 >>= (unifier ta Type.Bool) in
    let (tb, subst3) = type_of b env subst2 in
    let (tc, subst4) = type_of c env subst3 in
    let subst5 = subst4 >>= (unifier tb tc) in
    (tb, subst5)
  | Ast.Var n -> ((env_lookup env n), subst)
  (* | Ast.Fun (var,body) -> *)
  (*   let tv = gen_var () in *)
  (*   let (ty, subst1) = type_of body (env_extend env var tv) subst in *)
  (*   (Type.Fun (tv,ty), subst1) *)
  (* | Ast.App (rator,rand) -> *)
  (*   let result_type = gen_var () in *)
  (*   let (rator_type, subst1) = type_of rator env subst in *)
  (*   let (rand_type, subst2) = type_of rand env subst1 in *)
  (*   let subst3 = subst2 >>= (unifier rator_type (Type.Fun (rand_type, result_type))) in *)
  (*   (result_type, subst3) *)
  | _ -> failwith "not support yet"

let infer exp =
  let (ty, subst) = type_of exp [] (Some []) in
  match subst with
  | Some s -> update_type ty s
  | None -> failwith "false"
