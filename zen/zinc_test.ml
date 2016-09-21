open Zinc

let test_identity () =
  compile (Lambda.Fun (1, Lambda.Var 0)) [] =
  [Instruct.Closure [Instruct.Grab; Instruct.Access 0; Instruct.Return]]

let test_basic_apply () =
  compile (Lambda.App (Lambda.Fun (1, Lambda.Var 42), [Lambda.Int 42])) [] = [Instruct.Pushmark; Instruct.Const 42; Instruct.Closure [Instruct.Grab; Instruct.Access 42; Instruct.Return];
 Instruct.Apply]

let tests = [
  ("compile_identity", test_identity);
  ("compile_basic_apply", test_basic_apply);
]

let testfn = function (a, b) -> Printf.printf "%s ... %s\n" a (if b () then "ok" else "fail");;

List.iter testfn tests
