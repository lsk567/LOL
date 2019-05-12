%{
  open Ast
  module StringMap = Map.Make (String)
%}

%token APPEND LENGTH
%token SEMI LPAREN RPAREN LBRACE RBRACE LSQBRACE RSQBRACE
%token LIST TENSOR
%token QUOTE COLON COMMA DOT
%token PLUS MINUS TIMES DIVIDE MOD OUTER POW ASSIGN
%token PLUSASN MINUSASN TIMESASN DIVIDEASN MODASN INC DEC
%token EQ NEQ LT GT LEQ GEQ AND OR NOT
%token IF ELIF ELSE FOR IN WHILE
%token FUNC RETURN
%token INT BOOL FLOAT STRING VOID
%token <int> INTLIT
%token <bool> BOOLLIT
%token <string> ID STRLIT FLOATLIT
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%left COMMA
%left COLON
%right PLUSASN MINUSASN TIMESASN DIVIDEASN MODASN ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS NEG
%left TIMES DIVIDE MOD DOT OUTER
%left POW
%right NOT
%right INC DEC
%left APPEND LENGTH

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
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  /* Declarations */
  | typ ID SEMI                             { Decl ($1,$2,Noexpr) } /* Initialize variables. Can have default init. */
  | typ ID ASSIGN expr SEMI                 { Decl ($1,$2,$4) }
  | fun_decl                                { $1 } /* Function Declaration */
  | RETURN expr_opt SEMI                    { Return($2) } /*  Return a value */
  /* Control Flows */
  | IF LPAREN expr RPAREN stmt false_branch
                                            { If($3, $5, $6) } /* If, else, elif stuff */
  | FOR LPAREN init_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9) } /* For loop; for (;;) */
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5) } /* While loop, can be treated as a for loop */

fun_decl: /* fun int add (int i, int j) { i = i+1-1; return i+j; } */
  FUNC ret_typ ID LPAREN params_opt RPAREN LBRACE stmt_list RBRACE
    { Decl( Func({param_typs = List.map (fun (ty, _) -> ty) $5;return_typ = $2 }),
    $3,
    FExpr({ typ = $2; params = $5; body = List.rev $8 }))}

/* if stuff */
false_branch:
    elif { $1 }
  | cf_else { $1 }
  | %prec NOELSE { Block([]) }

elif:
  ELIF LPAREN expr RPAREN stmt false_branch { If($3, $5, $6) }

cf_else:
  ELSE stmt { $2 }

/* Expressions */
expr:
  /* Accesors */
    accessor  {$1}
  /* Arithmeics */
  | MINUS expr %prec NOT { Unop(Neg, $2)      }
  | NOT expr             { Unop(Not, $2)      }
  | expr PLUS   expr { Binop($1, Add, $3)     }
  | expr MINUS  expr { Binop($1, Sub, $3)     }
  | expr TIMES  expr { Binop($1, Mul, $3)     }
  | expr DIVIDE expr { Binop($1, Div, $3)     }
  | expr MOD    expr   { Binop($1, Mod,   $3) }
  | expr POW    expr   { Binop($1, Pow,   $3) }
  | expr OUTER    expr { Binop($1, Outer, $3) }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }

  /* Function expression*/
  | function_expr { $1 }

  /* Assignmnent */
  | ID ASSIGN expr    { Assign(Id($1), NoOp,$3)      }
  | ID PLUSASN   expr { Assign(Id($1), Add, $3) }
  | ID MINUSASN  expr { Assign(Id($1), Sub, $3) }
  | ID TIMESASN  expr { Assign(Id($1), Mul, $3)}
  | ID DIVIDEASN expr { Assign(Id($1), Div, $3) }
  | ID MODASN    expr { Assign(Id($1), Mod, $3) }
  | ID INC            { Assign(Id($1), Add, IntLit(1)) }
  | ID DEC            { Assign(Id($1), Sub, IntLit(1)) }

  /*List*/
  | LSQBRACE opt_items RSQBRACE { ListLit($2) }
  | accessor ASSIGN expr    { Assign($1, NoOp, $3) }
  | accessor PLUSASN expr   { Assign($1, Add, $3) }
  | accessor MINUSASN expr  { Assign($1, Sub, $3) }
  | accessor TIMESASN expr  { Assign($1, Mul, $3) }
  | accessor DIVIDEASN expr { Assign($1, Div, $3) }
  | accessor MODASN expr    { Assign($1, Mod, $3) }
  /* Brackets for precedence */
  | LPAREN expr RPAREN   { $2 }

/* Accesors, helpful for recursive case */
accessor:
    accessor LPAREN args_opt RPAREN { Call($1, $3)  } /* fun(x) */
  | accessor APPEND LPAREN expr RPAREN { ListAppend($1, $4) }
  | accessor LENGTH LPAREN RPAREN { ListLength($1) }
  | accessor LSQBRACE expr RSQBRACE { ListAccess($1, $3) }
  | atom { $1 }

atom:
    INTLIT           { IntLit ($1) }
  | FLOATLIT	       { FloatLit($1) }
  | BOOLLIT          { BoolLit($1) }
  | STRLIT           { StrLit($1) }
  | ID                { Id($1) }

function_expr:
  FUNC ret_typ LPAREN params_opt RPAREN LBRACE stmt_list RBRACE {FExpr( { typ = $2; params = $4; body = List.rev $7} )}

/* Types */
ret_typ:
    VOID { Void }
  | typ { $1 }

typ:
    INT { Int }
  | FLOAT { Float }
  | BOOL { Bool }
  | STRING { String }
  | VOID   { Void }
  | FUNC LPAREN typ_opt COLON ret_typ RPAREN { Func ( { param_typs = $3; return_typ = $5 } )}
  | FUNC  { Func ( { param_typs = []; return_typ = Abstract } ) } /* Anonymous function */
  | LIST LT typ GT { List($3)}
  | TENSOR { Tensor }

/* Helpers */
typ_opt:
  { [] }
| typ_list { List.rev $1 }

typ_list:
  typ { [$1] }
| typ_list COMMA typ { $3 :: $1 }

opt_items:
  /* nothing */ { [] }
| item_list { List.rev $1 }

item_list:
  expr { [$1] }
| item_list COMMA expr {$3 :: $1}

expr_opt:
  /* nothing */ { Noexpr }
| expr          { $1 }

params_opt:
  /* nothing */ { [] }
| param_list    { List.rev $1 }

param_list:
  typ ID { [($1, $2)] }
| param_list COMMA typ ID { ($3, $4) :: $1 }


args_opt:
  /* nothing */ { [] }
| arg_list { List.rev $1 }

init_opt:
  /* nothing */                     { Nostmt }
| typ ID ASSIGN expr                { Decl ($1,$2,$4)}
| ID ASSIGN expr                    { Expr(Assign(Id($1), NoOp, $3))}

arg_list:
  expr { [$1] }
| arg_list COMMA expr { $3 :: $1 }
