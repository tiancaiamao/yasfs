open Zinc

let test_identity () =
  compile (Lambda.Fun (1, [Lambda.Var 0])) [] =
  [Instruct.Closure [Instruct.Grab; Instruct.Access 0; Instruct.Return]]

let test_basic_apply () =
  compile (Lambda.App (Lambda.Fun (1, [Lambda.Var 42]), [Lambda.Int 42])) [] = [Instruct.Pushmark; Instruct.Const 42; Instruct.Closure [Instruct.Grab; Instruct.Access 42; Instruct.Return];
                                                                                Instruct.Apply]

let test_multi_argument () =
  compile (Lambda.Fun (2, [Lambda.Var 1])) [] = [Instruct.Closure [Instruct.Grab; Instruct.Grab; Instruct.Access 1; Instruct.Return]]

(* order of push stack *)
let test_order () =
  compile (Lambda.App (Lambda.Int 0, [Lambda.Int 1; Lambda.Int 2; Lambda.Int 3])) [] = [Instruct.Pushmark; Instruct.Const 3; Instruct.Const 2; Instruct.Const 1; Instruct.Const 0; Instruct.Apply]

let test_bind () =
  compile (Lambda.Fun (0, [Lambda.Bind (Lambda.Int 3); Lambda.Var 0])) [] =
  [Instruct.Closure [Instruct.Const 3; Instruct.Bind; Instruct.Access 0; Instruct.Return]]

let tests = [
  ("compile_identity", test_identity);
  ("compile_basic_apply", test_basic_apply);
  ("compile_multi_argument", test_multi_argument);
  ("compile_order", test_order);
  ("compile_bind", test_bind);
]

let testfn = function (a, b) -> Printf.printf "%s ... %s\n" a (if b () then "ok" else "fail");;

List.iter testfn tests
