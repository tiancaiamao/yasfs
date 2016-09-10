%{
        open Ast
%}

%token ARROW
%token ARROW2
%token BIND
%token <int> INT
%token PLUS
%token EQUAL
%token NOT_EQUAL
%token LESS_EQUAL
%token GREATER_EQUAL
%token LESS
%token GREATER
%token IF
%token THEN
%token ELSE
%token <string> IDENT
%token LET
%token FN
%token IN
%token REC
%token COMMA
%token ARRAY_CREATE
%token DOT
%token LESS_MINUS
%token SEMICOLON
%token LPAREN
%token RPAREN
%token EOF EOL

%right prec_let
%right SEMICOLON
%right prec_if
%right LESS_MINUS
%left COMMA
%left EQUAL NOT_EQUAL LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS
%right prec_unary_minus
%left prec_app
%left DOT

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
| exp PLUS exp
    { Add($1, $3) }
| FN formal_args ARROW exp
    { Fun($2, $4) }
| IDENT BIND exp EOL exp
    { Let($1, $3, $5) }
| exp actual_args
    %prec prec_app
    { App($1, $2) }
| error
    { failwith
        (Printf.sprintf "parse error near characters %d-%d"
           (Parsing.symbol_start ())
           (Parsing.symbol_end ())) }

formal_args:
| IDENT formal_args
    { Var($1)::$2 }
| IDENT
    { [Var($1)] }

actual_args:
| actual_args simple_exp
    %prec prec_app
    { $1 @ [$2] }
| simple_exp
    %prec prec_app
    { [$1] }
