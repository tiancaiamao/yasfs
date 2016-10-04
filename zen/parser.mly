%{
        open Ast
%}

%token ARROW
%token ARROW2
%token BIND
%token <int> INT
%token <string> IDENT
%token PLUS
%token MUL
%token SUB
%token TRUE
%token FALSE
%token FN
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token IF
%token THEN
%token ELSE
%token EQUAL
%token EOF
%token SEMICOLON

%left PLUS

%type <Ast.t> exp
%type <Ast.t list> top
%start top

%%

top:
| exp
  { [$1] }
| exp_list
  { $1 }

simple_exp:
| LPAREN exp RPAREN
    { $2 }
| INT
    { Int($1) }
| IDENT
    { Var($1) }
| TRUE
    { Bool(true) }
| FALSE
    { Bool(false) }

exp:
| simple_exp
    { $1 }
| FN formal_args ARROW fn_body
    { Fun($2, $4) }
| FN formal_args ARROW2 fn_body
    { Fun1($2, $4) }
| exp actual_args
    { App($1, $2) }
| exp PLUS exp
    { Plus($1, $3) }
| exp SUB exp
    { Sub($1, $3) }
| exp MUL exp
    { Mul($1, $3) }
| exp EQUAL exp
    { Equal($1, $3) }
| IF exp THEN exp ELSE exp
    { If($2, $4, $6) }
| IDENT BIND exp
    { Bind($1, $3) }
| error
    { failwith
        (Printf.sprintf "parse error near characters %d-%d"
           (Parsing.symbol_start ())
           (Parsing.symbol_end ())) }

fn_body:
| LBRACE exp_list RBRACE
  { $2 }
| exp
  { [$1] }

exp_list:
| exp SEMICOLON exp_list
  { $1::$3 }
| exp 
  { [$1] }

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
