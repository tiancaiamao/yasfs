%{
open Syntax
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
%token <Id.t> IDENT
%token LET
%token FN
%token NEWLINE
%token IN
%token REC
%token COMMA
%token ARRAY_CREATE
%token DOT
%token LESS_MINUS
%token SEMICOLON
%token LPAREN
%token RPAREN
%token EOF

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

%type <Syntax.t> exp
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
| exp EQUAL exp
    { Eq($1, $3) }
| exp NOT_EQUAL exp
    { Not(Eq($1, $3)) }
| exp LESS exp
    { Not(LE($3, $1)) }
| exp GREATER exp
    { Not(LE($1, $3)) }
| exp LESS_EQUAL exp
    { LE($1, $3) }
| exp GREATER_EQUAL exp
    { LE($3, $1) }
| FN formal_args ARROW exp
    { FUN($2, $4) }
| IDENT BIND exp NEWLINE exp
    { Let($1, $3, $5) }
| exp actual_args
    %prec prec_app
    { App($1, $2) }
| error
    { failwith
        (Printf.sprintf "parse error near characters %d-%d"
           (Parsing.symbol_start ())
           (Parsing.symbol_end ())) }

fundef:
| IDENT formal_args EQUAL exp
    { { name = addtyp $1; args = $2; body = $4 } }

formal_args:
| IDENT formal_args
    { addtyp $1 :: $2 }
| IDENT
    { [addtyp $1] }

actual_args:
| actual_args simple_exp
    %prec prec_app
    { $1 @ [$2] }
| simple_exp
    %prec prec_app
    { [$1] }