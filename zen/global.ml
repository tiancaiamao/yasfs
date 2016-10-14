let g_t_env = Hashtbl.create 33;;

let counter = ref 0;;

Hashtbl.add g_t_env "neveruse" !counter;;

let name2tag n = try Hashtbl.find g_t_env n with
  | _ -> (counter := !counter + 1;
    Hashtbl.add g_t_env n !counter;
    !counter)
