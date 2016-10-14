let g_t_env = Hashtbl.create 33;;

let counter = ref 0;;

Hashtbl.add g_t_env "neveruse" (Type.TNone, [Type.Unit]);;

(* let add_t_env n ts = *)
(*   counter := !counter + 1; *)
(*   let size = (List.length ts) in *)
(*   let v = (Type.make_tuple_desc !counter (Type.CExact size) ts) in *)
(*   begin Hashtbl.add g_t_env n v; *)
(*   v end;; *)

let get_t_env n = try Some (Hashtbl.find g_t_env n) with _ -> None
