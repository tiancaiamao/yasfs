open Zinc

let step_parse str = Parser.top Lexer.token (Lexing.from_string str)

let step_infer ast = List.iter (fun x -> (Infer.infer x) |> ignore) ast

let step_bruijn ast = Bruijn.ast2lambda [] (List.hd ast)

let step_compile ir = Zinc.compile ir [Instruct.Stop]

let step_run code = run code [] (Stack.create ()) (Stack.create ())

let eval s =
  let ast = step_parse s in
  let _ = step_infer ast in
  let ir = step_bruijn ast in
  let bc = step_compile ir in
  step_run bc
