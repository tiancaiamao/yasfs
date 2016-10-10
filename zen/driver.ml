open Zinc

let get_expr = function Ast.Expr x -> Some x | _ -> None

let filter_map f ls =
  let ff ls x = match f x with
    | None -> ls
    | Some v -> v::ls in
  List.fold_left ff [] ls

let step_parse str = Parser.top Lexer.token (Lexing.from_string str)

let step_infer ast =
   ast |> (filter_map get_expr) |> Infer.infer_list

let step_bruijn ast = Bruijn.ast2lambda [] (List.hd (ast |> (filter_map get_expr)))

let step_compile ir = Zinc.compile ir [Instruct.Stop]

let step_run code = run code [] (Stack.create ()) (Stack.create ())

let eval s =
  let ast = step_parse s in
  let _ = step_infer ast in
  let ir = step_bruijn ast in
  let bc = step_compile ir in
  step_run bc
