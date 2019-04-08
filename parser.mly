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
%type <Ast.program> program

%%

program: stmt_list EOF { List.rev $1 }

/* statements*/
stmt_list:
  /* nothing */    { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                               { Expr($1)              } /* x = 3; */
  /* Declarations */
  | typ ID init_opt SEMI                    { VDecl ($1,$2,$3) } /* Initialize variables. Can have default init. */
  | fun_decl                                { $1 }
  | RETURN noexpr_expr SEMI                 { Return($2) } /*  Return a value */
  /* Control Flows */
  | IF LPAREN expr RPAREN LBRACE stmt_list RBRACE false_branch
                                            { If($3, List.rev $6, $8) } /* If, else, elif stuff */
  | FOR LPAREN loop_init_opt SEMI expr_opt SEMI expr_opt RPAREN LBRACE stmt_list RBRACE
                                            { For($3, $5, $7, List.rev $10) } /* For loop; for (;;) */
  | WHILE LPAREN expr_opt RPAREN LBRACE stmt_list RBRACE
                                            { For(None, $3, None, List.rev $6) } /* While loop, can be treated as a for loop */

fun_decl: /* fun int add (int i, int j) { i = i+1-1; return i+j; } */
  FUNC ret_typ ID LPAREN params_opt RPAREN LBRACE stmt_list RBRACE
    { FDecl( Func({ param_typs = List.map (fun (ty, _) -> ty) $5; return_typ = $2 }),
      $3, FExpr({ typ = $2; name = $3; params = $5; body = List.rev $8 }))}

/* if stuff */
false_branch: elif { $1 } | cf_else { $1 } | %prec NOELSE { [] }

elif:
ELIF LPAREN expr RPAREN LBRACE stmt_list RBRACE false_branch
    { [If($3, List.rev $6, $8)] }

cf_else:
ELSE LBRACE stmt_list RBRACE { List.rev $3 }

/* Expressions */
expr:
  /* Accesors */
    accessor         { $1                     }
  /* Assignmnent */
  | expr ASSIGN expr { Assign($1, $3)         }
  /* Binop */
  | expr PLUS   expr { Binop($1, Add, $3)     }
  | expr MINUS  expr { Binop($1, Sub, $3)     }
  | expr TIMES  expr { Binop($1, Mul, $3)     }
  | expr DIVIDE expr { Binop($1, Div, $3)     }
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
  | NOT expr             { Unop(Not, $2)      }
  /* Brackets for precedence */
  | LPAREN expr RPAREN   { $2                 }

accessor:
    accessor LPAREN args_opt RPAREN { Call($1, $3)  } /* fun(x) */
  | atom { $1 }

atom:
  /* atomic units*/
    INTLIT           { IntLit ($1) }
  | FLOATLIT	       { FloatLit($1) }
  | BOOLLIT          { BoolLit($1) }
  | STRLIT           { StrLit($1) }
  | ID               { Id($1) }

/* Types */
ret_typ:
    VOID { Void }
  | typ { $1 }

typ:
    INT { Int }
  | FLOAT { Float }
  | BOOL { Bool }
  | STRING { String }

/* Helpers */

init_opt:
  /* nothing */ { None }
| ASSIGN expr   { Some($2) }

noexpr_expr:
  /* nothing */ { Noexpr }
| expr          { $1 }

expr_opt:
/* nothing */   { None }
| expr          { Some($1) }

params_opt:
  /* nothing */ { [] }
| param_list    { List.rev $1 }

param_list:
  typ ID { [($1, $2)] }
| param_list COMMA typ ID { ($3, $4) :: $1 }

loop_init_opt:
  /* nothing */{ None }
| expr { Some(Expr($1)) }
| typ ID ASSIGN expr { Some(VDecl($1, $2, Some($4))) }

args_opt:
  { [] }
| arg_list { List.rev $1 }

arg_list:
  expr { [$1] }
| arg_list COMMA expr { $3 :: $1 }
