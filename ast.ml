
module StringMap = Map.Make(String)

(* types *)
type typ =
    Int
  | Float
  | Bool
  | String
  | Void
  | Func of func_typ
  | List of typ
  | Empty
  | Tensor

and func_typ = {
    param_typs: typ list;
    return_typ: typ;
}

(* operations *)
type op = Add | Sub | Mul | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Mod | Pow | Outer | NoOp

type uop = Neg | Not

(* expressions *)
type expr =
    IntLit of int
  | FloatLit of string
	| BoolLit of bool
  | StrLit of string
	| Id of string
  | Binop of expr * op * expr
	| Unop of uop * expr
  | Assign of expr * op * expr
	| Call of expr * expr list
  | ListLit of expr list
  | ListAccess of expr * expr
  | ListLength of expr
  | FExpr of fexpr
	| Noexpr

and fexpr = {
  	typ : typ;
  	params : bind list;
  	body : stmt list;
  }

and bind = typ * string

(* Statements*)
and stmt =
    Block of stmt list
	| Expr of expr
  | Decl of typ * string * expr option
	| Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type program = stmt list

(* pretty-printing *)

let fmt_one name v = String.concat "" [name; "("; v; ")"]
let fmt_two name v1 v2 = String.concat "" [name; "("; v1; ","; v2; ")"]
let fmt_three name v1 v2 v3 = String.concat ""
  [name; "("; v1; ","; v2; ","; v3; ")"]
let fmt_four name v1 v2 v3 v4 = String.concat ""
  [name; "("; v1; ","; v2; ","; v3; ","; v4; ")"]
let fmt_five name v1 v2 v3 v4 v5 = String.concat ""
  [name; "("; v1; ","; v2; ","; v3; ","; v4; ","; string_of_bool v5; ")"]

let fmt_list l =
  let items = String.concat ";" l in
  String.concat "" ["["; items; "]"]

let rec string_of_typ = function
    Void -> "void"
  | Func(e) -> "func(" ^ (String.concat "," (List.map string_of_typ e.param_typs))
    ^ "; " ^ (string_of_typ e.return_typ) ^ ")"
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | String -> "string"

and string_of_typ_list l =
  let typs = List.map string_of_typ l in
  fmt_list typs

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Pow -> "^"
  | Outer -> "@"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Mod -> "%"
  | Pow -> "^"
  | Outer -> "@"
  | NoOp -> ""

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let fmt_params l =
  let fmt_p = function
    (t, n) -> String.concat "" ["("; string_of_typ t; ", "; n; ")"] in
  fmt_list (List.map fmt_p l)

let rec string_of_expr = function
    IntLit(l) -> fmt_one "IntLit" (string_of_int l)
  | FloatLit(l) -> fmt_one "FloatLit" l
  | StrLit(l) -> fmt_one "StrLit"  l
  | BoolLit(l) -> fmt_one "BoolLit" (string_of_bool l)
  | Id(s) -> fmt_one "Id" s
  | Binop(e1, o, e2) -> fmt_three "Binop" (string_of_expr e1) (string_of_op o) (string_of_expr e2)
  | Unop(uo, e) -> fmt_two "Unop" (string_of_uop uo) (string_of_expr e)
  | Assign(e1, o, e2) -> fmt_three "Assign" (string_of_expr e1) (string_of_op o) (string_of_expr e2)
  | Call(_, _) -> "Function Call"
  (* below actually is parsed with {name = e.name; param = e.params;
   * typ = e.typ; body = e.body}. See test programs for examples. *)
  | FExpr(e) -> fmt_fexpr e
  | Noexpr -> ""

and fmt_fexpr e =
  fmt_three "FExpr" (fmt_params e.params) (string_of_typ e.typ) (string_of_stmt_list e.body)

and fmt_members l =
  let fmt_m = function
    (t, n, None) -> fmt_three "" (string_of_typ t) n "None"
  | (t, n, Some(e)) -> fmt_three "" (string_of_typ t) n (string_of_expr e) in
  fmt_list (List.map fmt_m l)

and fmt_init l =
   let fmt_i (n, e) = fmt_two "" n (string_of_expr e) in
   fmt_list (List.map fmt_i l)

and string_of_stmt_list l =
  let stmts = List.map string_of_stmt l in
  String.concat "\n" stmts

and string_of_stmt = function
    Block(l) -> string_of_stmt_list l
  | Expr(e) -> string_of_expr e
  | Return(e) -> fmt_one "Return" (string_of_expr e)
  | Decl (t, n, l) -> fmt_three "Decl" (string_of_typ t) n (match l with
      None -> "" | Some(e) -> string_of_expr e)
  | For (init, e2, e3, s) ->
    fmt_four "ForLoop"
    (string_of_expr init)
    (string_of_expr e2)
    (string_of_expr e3) (string_of_stmt s)
  | While (e, s) ->
    fmt_two "WhileLoop" (string_of_expr e) (string_of_stmt s)
  | If(e, tL, fL) -> fmt_three "If" (string_of_expr e) (string_of_stmt tL)
    (string_of_stmt fL)


let string_of_program ast =
  string_of_stmt_list ast
