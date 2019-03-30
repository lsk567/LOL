(* types *)
type typ =
    Int
  | Float
  | Bool
  | String
  | Void
  | Func of func_typ

and func_typ = {
    param_typs: typ list;
    return_typ: typ;
}

(* operations *)
type op = Add | Sub | Mul | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

(* expressions *)
type expr =
    IntLit of int
  | FloatLit of string
	| BoolLit of bool
  | StringLit of string
	| Id of string
  | Binop of expr * op * expr
	| Unop of uop * expr
  | Assign of string * expr
	| Call of expr * expr list (* !! as opposed to string * expr list*)
	| Seq of expr * expr
  | FExpr of fexpr
	| Noexpr

and fexpr = {
  	typ : typ;
  	name : string;
  	params : bind list;
  	locals : bind list;
  	body : stmt list;
  }

and bind = typ * string

(* Statements*)
and stmt =
	| Expr of expr
  | VDecl of typ * string * expr option
	| Return of expr
	| If of expr * stmt * stmt
	| For of expr * expr * expr * stmt
	| While of expr * stmt


type program = stmt list
