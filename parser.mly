%{
  open Ast
  module StringMap = Map.Make (String)
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA ASSIGN
%token PLUS MINUS TIMES DIVIDE
%token NOT EQ NEQ LT GT LEQ GEQ AND OR
%token RETURN IF ELIF ELSE FOR WHILE
%token FUNC INT BOOL FLOAT STRING VOID
%token <int> INTLIT
%token <bool> BOOLLIT
%token <string> ID STRLIT FLOATLIT
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS NEG
%left TIMES DIVIDE
%right NOT

%start program
%type <Ast.expr> program

%%

program: stmt_list EOF { List.rev $1 }

/* statements*/
stmt_list:
  /* nothing */    { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                               { Expr($1)              } /* x = 3; */
  | typ ID init_opt SEMI                    { VDecl ($1,$2,$3)       } /* int x; int y =3;*/
  | RETURN expr_opt SEMI                    { Return($2)            } /* return; */
  | IF LPAREN expr RPAREN LBRACE stmt_list RBRACE false_branch
                                            { If($3, List.rev $6, $8) } /* If, else, elif stuff */
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9)   }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }
  | FUNC ret_typ ID LPAREN params_opt RPAREN LBRACE stmt_list RBRACE
                                            { FDecl ( Func({ param_typs = List.map (fun (ty, _) -> ty) $5; return_typ = $2 }),
                                                     $3,
                                                     Some(FExpr({ name = $3; typ = $2; params = $5; body = List.rev $8 }))
                                            ) } /* (typ list * ret_typ,ID, Function expression )*/

init_opt:
  /* nothing */ { None }
| ASSIGN expr   { Some($2) }

expr_opt:
  /* nothing */ { Noexpr }
| expr          { $1 }

params_opt:
/* nothing */ { [] }
| param_list    { List.rev $1 }

param_list:
  typ ID { [($1, $2)] }
| param_list COMMA typ ID { ($3, $4) :: $1 }

/* if stuff */
false_branch: elif { $1 } | cf_else { $1 } | %prec NOELSE { [] }

elif:
ELIF LPAREN expr RPAREN LBRACE stmt_list RBRACE false_branch
    { [If($3, List.rev $6, $8)] }

cf_else:
ELSE LBRACE stmt_list RBRACE { List.rev $3 }

expr:
  /* atomic units*/
    INTLIT           { Literal ($1) }
  | FLOATLIT	       { Fliteral($1) }
  | BOOLLIT          { BLiteral($1) }
  | STRLIT           { SLiteral($1) }
  | ID               { Id($1) }
  /* Binop */
  | expr PLUS   expr { Binop($1, Add, $3) }
  | expr MINUS  expr { Binop($1, Sub, $3) }
  | expr TIMES  expr { Binop($1, Mul, $3) }
  | expr DIVIDE expr { Binop($1, Div, $3) }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  /* Unnop */
  | MINUS expr %prec NOT { Unop(Neg, $2)      }
  | NOT expr         { Unop(Not, $2)          }
  /* Anonymous Function NotImplement
  | function_expr { FExpr($1) }*/
  /* Assignmnent */
  | ID ASSIGN expr   { Assign($1, $3)         }
  /* Accessing */
  | ID LPAREN params_opt RPAREN { Call($1, $3)  } /* fun(x) */
  /* precedence */
  | LPAREN expr RPAREN { $2 }

/* Types */
ret_typ:
    VOID { Void }
  | typ { $1 }

typ:
    INT { Int }
  | FLOAT { Float }
  | BOOL { Bool }
  | STRING { String }
