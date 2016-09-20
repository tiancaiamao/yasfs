%{
        open Ast
%}

%token ARROW
%token ARROW2
%token BIND
%token <int> INT
%token PLUS
%token <string> IDENT
%token FN
%token LPAREN
%token RPAREN
%token EOL
%token EOF

%left PLUS

%type <Ast.t> exp
%start exp

%%

simple_exp:
| LPAREN exp RPAREN
    { $2 }
| INT
    { Int($1) }
| IDENT
    { Var($1) }

exp:
| simple_exp
    { $1 }
| FN formal_args ARROW exp
    { Fun($2, $4) }
| exp actual_args
    { App($1, $2) }
| error
    { failwith
        (Printf.sprintf "parse error near characters %d-%d"
           (Parsing.symbol_start ())
           (Parsing.symbol_end ())) }

formal_args:
| IDENT formal_args
    { $1::$2 }
| IDENT
    { [$1] }

actual_args:
| actual_args simple_exp
    { $1 @ [$2] }
| simple_exp
    { [$1] }
