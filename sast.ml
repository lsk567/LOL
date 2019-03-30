open Ast

(* types *)
type styp =
    SInt
  | SFloat
  | SBool
  | SString
  | SVoid
  | SFunc of sfunc_typ

and sfunc_typ = {
    sparam_typs: styp list;
    sreturn_typ: styp;
    sbuiltin: bool;
}

(* expressions *)
type sexpr = typ * sx

and sx =
    SIntLit of int
  | SFloatLit of string
  | SBoolLit of bool
  | SStringLit of string
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of string * sexpr
  | SCall of string * sexpr list
  | SFExpr of sfexpr
  | SNoexpr


and sfexpr = {
    styp : styp;
    sname : string;
    sparams: sbind list;
    slocals : sbind list;
    sbody : sstmt list
}

and sbind = styp * string

(* statements *)
and sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SVDecl of styp * string * sexpr option
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt

(* program *)
type sprogram = sstmt list
