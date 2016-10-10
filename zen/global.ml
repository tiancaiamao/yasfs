let g_t_env = Hashtbl.create 33;;

Hashtbl.add g_t_env "bool" Type.Bool;;

Hashtbl.add g_t_env "int" Type.Int;;

Hashtbl.add g_t_env "unit" Type.Unit

let add_t_env (n, v) = Hashtbl.add g_t_env n v
