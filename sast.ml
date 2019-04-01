
open Ast

(* types *)
type styp =
    SInt
  | SFloat
  | SBool
  | SString
  | SVoid
  | SFunc of sfunc_typ
  | SABSTRACT
  | SAny

and sfunc_typ = {
    sparam_typs: styp list;
    sreturn_typ: styp;
    sbuiltin: bool;
}

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
  | SAssign of sexpr * sexpr
  | SCall of sexpr * sexpr list
  | SFExpr of sfexpr
  | SClosure of sclsr
  | SNoexpr


and sfexpr = {
    styp : styp;
    sname : string;
    sparams: sbind list;
    sbody : sstmt list
}

and sbind = styp * string

and sclsr = {
  ind: int;
  free_vars: sbind list;
}

(* statements *)
and sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SVDecl of styp * string * sexpr option
  | SFDecl of styp * string * sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt list * sstmt list
  | SFor of (sstmt option) * (sexpr option) * (sexpr option) * sstmt list

(* program *)
type sprogram = sstmt list

(* Pretty printing *)

let string_of_sstmt = function
  | SExpr(_) -> "SExpr"
  | _ -> "Other"

(* PRETTY PRINTING based off of printer.ml *)
let rec fmt_styp = function
    SVoid -> "svoid"
  | SFunc(e) -> "sfunc(" ^
                (String.concat "," (List.map fmt_styp e.sparam_typs)) ^ "; "
                ^ (fmt_styp e.sreturn_typ) ^ ")"
  | SInt -> "sint"
  | SFloat -> "sfloat"
  | SBool -> "sbool"
  | SString -> "sstring"
  | SABSTRACT -> "SABSTRACT"
  | SAny -> "SAny"

let fmt_sparams l =
  let fmt_p = function
      (t, n) -> String.concat "" ["("; fmt_styp t; ", "; n; ")"] in
  fmt_list (List.map fmt_p l)

let rec fmt_sexpr (_,s) =
  (match s with
     SIntLit(l) -> fmt_one "IntLit" (string_of_int l)
   | SFloatLit(l) -> fmt_one "FloatLit" l
   | SStrLit(l) -> fmt_one "StrLit"  l
   | SBoolLit(l) -> fmt_one "BoolLit" (string_of_bool l)
   | SId(s) -> fmt_one "Id" s
   | SBinop(e1, o, e2) -> (fmt_sexpr e1) ^ "\n    " ^ (fmt_op o) ^ "\n    "
                          ^ (fmt_sexpr e2)
   | SUnop(uo, e) -> fmt_two "Unop" (fmt_uop uo) (fmt_sexpr e)
   | SAssign(e1, e2) -> fmt_two "Assign" (fmt_sexpr e1) (fmt_sexpr e2)
   | SCall(se, a) -> "SFCall(\n      " ^ ((fmt_sexpr se) ^ "\n") ^ ("      "
                                ^ fmt_list (List.map fmt_sexpr a) ^ "\n    )")
   (* below actually is parsed with {name = e.name; param = e.params;
    * typ = e.typ; body = e.body}. See test programs for examples. *)
   | SFExpr(s) -> fmt_three "FExpr" (fmt_sparams s.sparams)
                    (fmt_styp s.styp) (fmt_sstmt_list s.sbody)
   | SClosure(clsr) -> fmt_two "Closure" (string_of_int clsr.ind)
                        (fmt_list (List.map (fun (t, n) -> fmt_styp t ^ " " ^ n)
                        clsr.free_vars))
   | SNoexpr -> ""
  )

and fmt_smembers l =
  let fmt_m = function
      (t, n, None) -> fmt_three "" (fmt_styp t) n "None"
    | (t, n, Some(e)) -> fmt_three "" (fmt_styp t) n (fmt_sexpr e) in
  fmt_list (List.map fmt_m l)

and fmt_sinit l =
  let fmt_i (n, e) = fmt_two "" n (fmt_sexpr e) in
  fmt_list (List.map fmt_i l)

and fmt_sstmt = function
    SExpr(se) -> fmt_sexpr se
  | SReturn(e) -> "Return " ^ (fmt_sexpr e)
  | SVDecl (t, n, l) -> (fmt_styp t) ^ " " ^ n ^ " = " ^ (match l with
        None -> "" | Some(e) -> fmt_sexpr e)
  | SFDecl (t, n, l) -> (fmt_styp t) ^ " " ^ n ^ " = " ^ (fmt_sexpr l)
  | SFor (init, e2, e3, s) ->
    fmt_four "ForLoop"
      (match init with None -> "" | Some(s) -> fmt_sstmt s)
      (fmt_opt_sexpr e2)
      (fmt_opt_sexpr e3) (fmt_sstmt_list s)
  | SIf(e, tL, fL) -> fmt_three "If" (fmt_sexpr e) (fmt_sstmt_list tL)
                        (fmt_sstmt_list fL)
  | SBlock(_) -> "SVBlock"

and fmt_sstmt_list ?spacer l =
  let sstmts = List.map fmt_sstmt l in
  let s = match spacer with Some(s) -> s | _ -> "" in
  let sstmts = List.map (fun x -> s ^ x) sstmts in
  String.concat "\n" sstmts

and fmt_opt_sexpr = function
    None -> ""
  | Some(e) -> fmt_sexpr e

let string_of_sprogram sast =
  String.concat ";\n" (List.map fmt_sstmt sast)
