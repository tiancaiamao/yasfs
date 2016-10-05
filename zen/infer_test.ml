open Infer

let tests = [
  ("if 3 then 88 else 99", false);
  ("fn x -> 3 x", false);
  ("fn x -> x 3", true);
  ("fn f -> fn x -> f x", true);
  ("x := 4; x 3", false);
  ("x := true; x 3", false);
  ("f := 3; fn x -> f x", false);
  ("(fn x -> x 3) 4", false);
  ("(fn x -> x-3) true", false);
  ("(fn f -> fn x -> f x) 3", false);
  ("fn f x => f (x-1)", true); (* dead loop but type safe *)
  ("fn f -> fn x -> (f 3) - (f x)", true);
  ("fn f -> f 11", true);
  ("fn x -> if x then x-1 else 0", false);
  ("fn f -> (f f) = 0", false);

  ("fn x y -> x + y", true);
  ("(fn x y -> x + y) 3 4", true);
  ("(fn x y -> x + y) true 4", false);
];;

let parse str = Parser.top Lexer.token (Lexing.from_string str);;

let infer_of str =
  try infer_list (parse str) |> fun _ -> true with
  | _ -> false

let test_one (s, r) = Printf.printf "%s ... %s\n" s (if r = infer_of s then "succ" else "fail");;

List.iter test_one tests
