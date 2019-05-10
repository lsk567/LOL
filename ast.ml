
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
  | Tensor
  | Abstract

and func_typ = {
  param_typs: typ list;
  return_typ: typ;
}

(* operations *)
type op = Add | Sub | Mul | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Mod | Pow | Outer | NoOp

type uop = Neg | Not

type bind = typ * string

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
  | ListAppend of expr * expr
  | ListLength of expr
  | FExpr of fexpr
	| Noexpr

and fexpr = {
  	typ : typ;
  	params : bind list;
  	body : stmt list;
  }

(* Statements*)
and stmt =
    Block of stmt list
	| Expr of expr
  | Decl of typ * string * expr
	| Return of expr
  | If of expr * stmt * stmt
  | For of stmt * expr * expr * stmt
  | While of expr * stmt
  | Nostmt

type program = stmt list

(* pretty-printing, should return the same code *)
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

(* map each element in list by function f, and join the string by s *)
let rec string_of_list_stmt l s = String.concat s (List.map string_of_stmt l)
and string_of_list_typ l s = String.concat s (List.map string_of_typ l)
and string_of_list_expr l s = String.concat s (List.map string_of_expr l)
and string_of_list_bind f l s = String.concat s (List.map f l)

and string_of_typ = function
    Void -> "void"
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | String -> "string"
  | Func f -> "func " ^ string_of_typ f.return_typ ^ " (" ^ string_of_list_typ f.param_typs ", "  ^ ")"
  | List typ -> "list <" ^ string_of_typ typ ^ ">"
  | Tensor -> "Tensor"
  | Abstract -> "abstract"

and string_of_expr = function
    IntLit(l) -> string_of_int l
  | FloatLit(l) -> l
  | StrLit(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(uo, e) -> string_of_uop uo ^ " " ^ string_of_expr e
  | Assign(e1, o, e2) -> string_of_expr e1 ^ string_of_op o ^ "= " ^ string_of_expr e2
  | Call(e, e_list) -> string_of_expr e ^ "(" ^ string_of_list_expr e_list ", "  ^ ")"
  (* below actually is parsed with {name = e.name; param = e.params;
   * typ = e.typ; body = e.body}. See test programs for examples. *)
  | FExpr(fexpr) -> string_of_fexpr fexpr
  | Noexpr -> ""
  (* List *)
  | ListLit(expr_list) -> string_of_list_expr expr_list ", "
  | ListAccess(e1,e2) -> string_of_expr e1 ^ "[" ^ (string_of_expr e2) ^ "]"
  | ListAppend(e1,e2) -> string_of_expr e1 ^ "Append[" ^ (string_of_expr e2) ^ "]"
  | ListLength(e) -> "len(" ^ string_of_expr e ^ ")"

and string_of_fexpr fexpr =
  let string_of_param param = let (typ, s) = param
    in string_of_typ typ ^ " " ^ s
  in
  "func " ^ string_of_typ fexpr.typ ^ " (" ^ string_of_list_bind string_of_param fexpr.params ", " ^")"
  ^ "{\n" ^ string_of_list_stmt fexpr.body "" ^ "}\n"

and string_of_stmt = function
    Block(stmts) -> "{\n" ^ string_of_list_stmt stmts "" ^ "}\n"
  | Expr(e) -> string_of_expr e ^ ";\n";
  | Return(e) -> "return " ^ string_of_expr e ^ ";\n"
  | Decl (t, s, e) ->
    string_of_typ t ^ " " ^ s ^ " = " ^ string_of_expr e ^ ";\n"
  | For (init, e2, e3, s) -> "for (" ^ string_of_stmt init ^ " ; " ^ string_of_expr e2 ^ " ; "
    ^ string_of_expr e3 ^ ") " ^ string_of_stmt s ^"\n"
  | While (e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | If(e, s1, fL) -> let then_part = match fL with
        Block([]) -> ""
      | If(_) as s2 -> "elif\n" ^ string_of_stmt s2
      | s2 -> "else\n" ^ string_of_stmt s2
    in
    "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 ^ then_part
  | Nostmt -> ""

and string_of_program stmts =
  string_of_list_stmt stmts "" ^ "\n"
