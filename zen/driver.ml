open Zinc

let state c = (c, [], (Stack.create ()), (Stack.create ()))

let easy_run code = run code [] (Stack.create ()) (Stack.create ())

let input0 = Lambda.App ((Lambda.Fun (2,[Lambda.Var 1])), [Lambda.Int 3; Lambda.Int 5])

let s0 = compile input0 [Instruct.Stop]

let eval s = let ast = Parser.top Lexer.token (Lexing.from_string s) in
  let ir = Bruijn.ast2lambda [] (List.hd ast) in
  let bc = Zinc.compile ir [Instruct.Stop] in
  easy_run bc;;
