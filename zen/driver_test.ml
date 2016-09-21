open Driver
open Zinc

let test_identity () =
  (eval "(fn x -> x) 5") = (Value 5)

let test_multi_argument () =
  (eval "(fn x y -> y) 2 42") = (Value 42)

let test_partial_apply () =
  (eval "((fn x y -> y) 1) 42") = (Value 42)

let tests = [
  ("identity", test_identity);
  ("multi_argument", test_multi_argument);
  ("partial_apply", test_partial_apply);
]


let testfn = function (a, b) -> Printf.printf "%s ... %s\n" a (if b () then "ok" else "fail");;

List.iter testfn tests
