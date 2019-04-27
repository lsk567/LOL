
module StringMap = Map.Make(String)

(* types *)
type typ =
    Int
  | Float
  | Bool
  | String
  | Void
  | Func
  | List of typ
  (* Linalg *)
  (* | Matrix of expr * expr *)
  | Matrix
  | Tensor 

(* operations *)
and op = Add | Sub | Mul | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Mod | Pow | Outer | NoOp

and uop = Neg | Not

and bind = typ * string

(* expressions *)
and expr =
    IntLit of int
  | FloatLit of string
	| BoolLit of bool
  | StrLit of string
	| Id of string (* var name *)
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
  (* Linalg *)
  (* MatrixLit(ListLit(ListLit(FloatLit("1.0"), FloatLit("2.0")), ListLit(FloatLit("3.0"), FloatLit("4.0")))) *)
  (* Check matrix rank and row/column count in semantics *)
  (* Matrix Operations *)
  | MatrixLit of expr list  
  | MatrixSet of expr * int * int * float (* MatrixSet(Id(m), i, j, x) *)
  | MatrixGet of expr * int * int (* MatrixGet(Id(m), i, j) *)
  (* | TensorLit of expr list *)
    

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
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type program = stmt list

(* pretty-printing, should return the same code *)

let rec string_of_typ = function
    Void -> "void"
  | Func -> "func"
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | String -> "string"
  | List typ -> string_of_typ typ ^ "[]"
  (* Linalg *)
  (* | Matrix(m, n) -> "Matrix" ^ "<" ^ (string_of_expr m) ^ "," ^ (string_of_expr n) ^ ">" *)
  | Matrix -> "Matrix"
  (* | Tensor -> "Tensor" *)

and string_of_op = function
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

and string_of_uop = function
    Neg -> "-"
  | Not -> "!"


(* map each element in list by function f, and join the string by s *)
and string_of_list_stmt l s = String.concat s (List.map string_of_stmt l)
and string_of_list_expr l s = String.concat s (List.map string_of_expr l)
and string_of_list_bind f l s = String.concat s (List.map f l)

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
    (* "Assign(" ^ string_of_expr e1 ^ "," ^ string_of_op o ^ "," ^ string_of_expr e2 ^ ")" *)
  | Call(e, e_list) -> string_of_expr e ^ "(" ^ string_of_list_expr e_list ", " ^ ")"
  (* below actually is parsed with {name = e.name; param = e.params;
   * typ = e.typ; body = e.body}. See test programs for examples. *)
  | FExpr(fexpr) -> string_of_fexpr fexpr
  | Noexpr -> ""
  (* List *)
  | ListLit(expr_list) -> string_of_list_expr expr_list ", "
  | ListAccess(e1,e2) -> string_of_expr e1 ^ "[" ^ (string_of_expr e2) ^ "]"
  | ListAppend(e1,e2) -> string_of_expr e1 ^ "Append[" ^ (string_of_expr e2) ^ "]"
  | ListLength(e) -> "len(" ^ string_of_expr e ^ ")"
  (* Matrix *)
  | MatrixLit(expr_list) -> "Matrix( " ^ string_of_list_expr expr_list ", " ^ " )"
  | MatrixSet(m, i, j, x) -> "MatrixSet( " ^ string_of_expr m ^ ", " ^ string_of_int i ^ ", " ^ string_of_int j ^ ", " ^ string_of_float x ^ " )"
  | MatrixGet(m, i, j) -> "MatrixGet( " ^ string_of_expr m ^ ", " ^ string_of_int i ^ ", " ^ string_of_int j ^ " )"

and string_of_fexpr fexpr =
  "func " ^ string_of_typ fexpr.typ ^ " (" ^ string_of_list_bind string_of_param fexpr.params ", " ^")"
  ^ "{\n" ^ string_of_list_stmt fexpr.body "" ^ "}\n"

and string_of_param param = let (typ, s) = param in
  string_of_typ typ ^ " " ^ s

and string_of_stmt = function
    Block(stmts) -> "{\n" ^ string_of_list_stmt stmts "" ^ "}\n"
  | Expr(e) -> string_of_expr e ^ ";\n";
  | Return(e) -> "return " ^ string_of_expr e ^ ";\n"
  | Decl (t, s, Noexpr) -> string_of_typ t ^ " " ^  s ^ ";\n"
  | Decl (t, s, e) -> string_of_typ t ^ " " ^ s ^ " = " ^ string_of_expr e ^ ";\n"
  | For (e1, e2, e3, s) -> "for (" ^ string_of_expr e1 ^ " ; " ^ string_of_expr e2 ^ " ; "
    ^ string_of_expr e3 ^ ") " ^ string_of_stmt s ^"\n"
  | While (e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | If(e, s1, fL) -> let then_part = match fL with
        Block([]) -> ""
      | If(_) as s2 -> "elif\n" ^ string_of_stmt s2
      | s2 -> "else\n" ^ string_of_stmt s2
    in
    "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 ^ then_part

let string_of_program stmts =
  String.concat "" (List.map string_of_stmt stmts) ^ "\n"
