{
        open Parser
}
let space = [' ' '\t' '\r']
let newline = ['\n']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
| newline
    { EOL }
| space+
    { token lexbuf }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '{'
    { LBRACE }
| '}'
    { RBRACE }
| ';'
    { SEMICOLON }
| digit+
    { INT(int_of_string (Lexing.lexeme lexbuf)) }
| '+'
    { PLUS }
| '*'
    { MUL }
| '='
    { EQUAL }
| "->"
    { ARROW }
| "=>"
    { ARROW2 }
| "fn"
    { FN }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }
| ":="
    { BIND }
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
