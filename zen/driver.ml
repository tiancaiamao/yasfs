open Zinc

let step_parse str = Parser.top Lexer.token (Lexing.from_string str)

let step_infer ast = Infer.infer_list ast

let step_bruijn ast = Bruijn.ast2lambda [] (List.hd ast)

let step_compile ir = Zinc.compile ir [Instruct.Stop] 0

(* let step_run code = run code [] (Stack.create ()) (Stack.create ()) *)

let step_emit code =
  let buf = Emit.new_buffer () in
  let bc = Emit.emit buf code in
  let out = open_out_bin "test.out" in
  let _ = output_bytes out bc in
  close_out out

(* let eval s = *)
(*   let ast = step_parse s in *)
(*   let _ = step_infer ast in *)
(*   let ir = step_bruijn ast in *)
(*   let bc = step_compile ir in *)
(*   step_run bc *)
