%{
        open Ast
%}

%token ARROW
%token ARROW2
%token COMMA
%token UNION
%token STRUCT
%token TYPE
%token BIND
%token KwINT
%token KwBOOL
%token KwUNIT
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
%type <Ast.item list> top
%type <Type.t> type_t
%start top

%%

top:
| type_or_exp
  { [$1] }
| type_or_exp SEMICOLON top
  { $1::$3 }

field:
| IDENT
  { Type.Field $1 }

field_type_list:
| field type_t COMMA
  { [($1, $2)] }
| field type_t COMMA field_type_list
  { ($1, $2)::$4 }

type_t:
| KwBOOL
  { Type.Bool }
| KwINT
  { Type.Int }
| KwUNIT
  { Type.Unit }
| type_t ARROW type_t
  { Type.Fun ($1, $3) }

type_or_exp:
| exp
  { Expr $1 }
| TYPE IDENT UNION LBRACE field_type_list RBRACE
  { Typedef ($2, Union (Type.Name($2),$5)) }

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

field_assign_list:
| IDENT EQUAL exp
    { [($1, $3)] }
| IDENT EQUAL exp COMMA field_assign_list
    { ($1,$3)::$5 }

exp:
| simple_exp
    { $1 }
| IDENT LBRACE field_assign_list RBRACE
    { Construct($1, $3) }
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
| LPAREN tuple RPAREN
    { Tuple $2 }
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

tuple:
| exp COMMA exp
  { [$1; $3] }
| exp COMMA tuple
  { $1::$3 }

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
