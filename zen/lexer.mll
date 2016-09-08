let space = [' ' '\t' '\r']
let newline = ['\n']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
| newline
    { NEWLINE }
| space+
    { token lexbuf }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| digit+
    { INT(int_of_string (Lexing.lexeme lexbuf)) }
| '+'
    { PLUS }
| '='
    { EQUAL }
| "!="
    { NOT_EQUAL }
| "<="
    { LESS_EQUAL }
| ">="
    { GREATER_EQUAL }
| '<'
    { LESS }
| '>'
    { GREATER }
| "->"
    { ARROW }
| "=>"
    { ARROW2 }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }
| "fn"
    { FN }
| ":="
    { BIND }
| "rec"
    { REC }
| ','
    { COMMA }
| '.'
    { DOT }
| ';'
    { SEMICOLON }
| eof
    { EOF }
| lower (digit|lower|upper|'_')*
    { IDENT(Lexing.lexeme lexbuf) }
| _
    { failwith
        (Printf.sprintf "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }