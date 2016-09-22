open Bruijn

(* fn x y -> x *)
let test_order () =
  ast2lambda [] (Ast.Fun (["x"; "y"], Ast.Var "x")) =
  Lambda.Fun (2, Lambda.Var 1)

let tests = [
  ("test_order", test_order)
]


let testfn = function (a, b) -> Printf.printf "%s ... %s\n" a (if b () then "ok" else "fail");;

List.iter testfn tests
