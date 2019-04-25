
open Ast

(* types *)
type styp =
    SInt
  | SFloat
  | SBool
  | SString
  | SVoid
  | SFunc of sfunc_typ
  | SList of styp
  | SEmpty
  | STensor
  | SABSTRACT
  (* Types only used for builtin fillers*)
  | SAny
  | SListElement of styp

and sfunc_typ = {
  sparam_typs: styp list;
  sreturn_typ: styp;
}

(* No need for op *)

type sbind = styp * string

(* expressions *)
type sexpr = styp * sx

and sx =
    SIntLit of int
  | SFloatLit of string
  | SBoolLit of bool
  | SStrLit of string
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of sexpr * op * sexpr
  | SCall of sexpr * sexpr list
  | SFExpr of sfexpr
  (* List *)
  | SListLit of sexpr list
  | SListAccess of sexpr * sexpr
  | SListAppend of sexpr * sexpr
  (* Other *)
  | SClosure of sclsr
  | SNoexpr

and sfexpr = {
    styp : styp;
    sparams: sbind list;
    sbody : sstmt list
}

and sclsr = {
  ind: int;
  free_vars: sbind list;
}

(* statements *)
and sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SDecl of styp * string * sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt

type sprogram = sstmt list

(* Helper function for converting between AST and SAST *)
let rec styp_of_typ typ = match typ with
    Int -> SInt
  | Bool -> SBool
  | Float -> SFloat
  | String -> SString
  | Void -> SVoid
  | List ty -> SList (styp_of_typ ty)
  | Func -> SFunc { sreturn_typ = SVoid; sparam_typs = []} (* Default SFunc to be void and no param*)
  | _ -> raise (Failure ("styp_of_typ for " ^ string_of_typ typ ^ " not implmented"))

and typ_of_styp styp = match styp with
    SInt -> Int
  | SBool -> Bool
  | SFloat -> Float
  | SString -> String
  | SVoid -> Void
  | SList sty -> List (typ_of_styp sty)
  | SFunc sfunc_typ -> Func
  | SEmpty | SABSTRACT | SAny -> raise(Failure ("typ_of_styp of " ^ string_of_styp styp ^ " shouldn't happen"))
  | _ -> raise (Failure ("typ_of_styp for " ^ (string_of_styp styp) ^ " not implemented"))

(* Pretty printing *)
and string_of_list_sstmt l s = String.concat s (List.map string_of_sstmt l)
and string_of_list_sexpr l s = String.concat s (List.map string_of_sexpr l)
and string_of_list_sbind f l s = String.concat s (List.map f l)

(* PRETTY PRINTING based off of printer.ml *)
and string_of_styp styp = match styp with
    SInt -> "sint"
  | SFloat -> "sfloat"
  | SString -> "sstring"
  | SBool -> "sbool"
  | SVoid -> "svoid"
  | SFunc(sfunc_typ) -> "sfunc " ^ string_of_styp sfunc_typ.sreturn_typ ^ "("
    ^ (String.concat "," (List.map string_of_styp sfunc_typ.sparam_typs))
    ^ ")"
  | SList styp -> string_of_styp styp ^ "[]"
  | SEmpty -> "sempty"
  | STensor -> "tensor"
  | SABSTRACT -> "SABSTRACT"
  | SAny -> "sany"
  | SListElement styp -> "slistelement (" ^ (string_of_styp styp) ^ ")"

and string_of_sexpr (styp,sx) = "(" ^ string_of_styp styp ^ " : "
  ^ ( 
    match sx with
     SIntLit(l) -> string_of_int l
   | SFloatLit(l) -> l
   | SStrLit(l) -> l
   | SBoolLit(true) -> "true"
   | SBoolLit(false) -> "false"
   | SId(s) -> s
   | SBinop(e1, o, e2) ->
     (string_of_sexpr e1) ^ " " ^ (string_of_op o) ^ " " ^ (string_of_sexpr e2)
   | SUnop(uo, e) -> string_of_uop uo ^ string_of_sexpr e
   | SAssign(e1, o, e2) -> string_of_sexpr e1 ^ string_of_op o ^ string_of_sexpr e2
   | SCall(se, se_list) -> string_of_sexpr se ^ string_of_list_sexpr se_list ", "
   | SFExpr(sfexpr) -> string_of_sfexpr sfexpr
   | SClosure(clsr) -> "{ ind: " ^ string_of_int clsr.ind ^ ", fvs: ("
     ^ string_of_list_sbind string_of_sparam clsr.free_vars ", " ^ ") } )"
   | SNoexpr -> ""
   (* List *)
   | SListLit(sexpr_list) -> string_of_list_sexpr sexpr_list ", "
   | SListAccess(s1,s2) -> string_of_sexpr s1 ^ "[" ^ (string_of_sexpr s2) ^ "]" ^ ")"
   | SListAppend(s1,s2) -> string_of_sexpr s1 ^ "Append[" ^ (string_of_sexpr s2) ^ "]"
  ) ^ ")"

and string_of_sparam sparam = let (styp, s) = sparam in
  string_of_styp styp ^ " " ^ s

and string_of_sfexpr sfexpr =
  "sfunc " ^ string_of_styp sfexpr.styp ^ " (" ^ string_of_list_sbind string_of_sparam sfexpr.sparams ", " ^")"
  ^ "{\n" ^ string_of_list_sstmt sfexpr.sbody "" ^ "}\n"

and string_of_sstmt = function
      SBlock(sstmts) ->
        "{\n" ^ string_of_list_sstmt sstmts "" ^ "}\n"
    | SExpr(sexpr) -> string_of_sexpr sexpr ^ ";\n";
    | SReturn(sexpr) -> "sreturn " ^ string_of_sexpr sexpr ^ ";\n";
    | SIf(e, s1, fL) -> let then_part = match fL with
          SBlock([]) -> ""
        | SIf(_) as s2 -> "elif\n" ^ string_of_sstmt s2
        | s2 -> "else\n" ^ string_of_sstmt s2
      in
      "sif (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s1 ^ then_part
    | SFor(se1, se2, se3, s) ->
        "sfor (" ^ string_of_sexpr se1  ^ " ; " ^ string_of_sexpr se2 ^ " ; " ^
        string_of_sexpr se3  ^ ") " ^ string_of_sstmt s
    | SWhile(e, s) -> "swhile (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
    | SDecl(t, id, (SVoid,SNoexpr)) -> string_of_styp t ^ " " ^ id ^ ";\n" (* Uninitalized *)
    | SDecl(t, id, v) -> string_of_styp t ^ " " ^ id ^ " = " ^ string_of_sexpr v ^ ";\n"

let string_of_sprogram sast =
  String.concat "" (List.map string_of_sstmt sast) ^ "\n"
