open Zinc

let step1 ((op::c), e, s, r) = step (c, e, s, r) op

let state c = (c, [], (Stack.create ()), (Stack.create ()))

let easy_run code = run code [] (Stack.create ()) (Stack.create ())

let input0 = Lambda.App ((Lambda.Fun (2,Lambda.Var 1)), [Lambda.Int 3; Lambda.Int 5])

let s0 = compile input0 [Instruct.Stop]

let eval s = let ast = Parser.exp Lexer.token (Lexing.from_string s) in
  let ir = Bruijn.ast2lambda [] ast in
  let bc = Zinc.compile ir [Instruct.Stop] in
  easy_run bc;;

(* simple test *)
eval "(fn x -> x) 1"
