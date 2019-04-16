
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

and sfunc_typ = {
  sparam_typs: styp list;
  sreturn_typ: styp;
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
  | SAssign of sexpr * op * sexpr
  | SCall of sexpr * sexpr list
  | SFExpr of sfexpr
  (* List *)
  | SListLit of sexpr list
  | SListAccess of sexpr * sexpr
  (* Other *)
  | SClosure of sclsr
  | SNoexpr

and sfexpr = {
    styp : styp;
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
  | SDecl of styp * string * sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt

(* program *)
type sprogram = sstmt list

(* Pretty printing *)
let rec typ_of_styp t = match t with
    SInt -> Int
  | SBool -> Bool
  | SFloat -> Float
  | SString -> String
  | SVoid -> Void
  | SList ty -> List (typ_of_styp ty)
  | SEmpty -> Empty
  | SFunc sfunc_typ -> Func { return_typ = (typ_of_styp sfunc_typ.sreturn_typ);
                              param_typs = (List.map typ_of_styp sfunc_typ.sparam_typs) }

let rec styp_of_typ t = match t with
    Int -> SInt
  | Bool -> SBool
  | Float -> SFloat
  | String -> SString
  | Void -> SVoid
  | List ty -> SList (styp_of_typ ty)
  | Empty -> SEmpty
  | Func func_typ -> SFunc { sreturn_typ = (styp_of_typ func_typ.return_typ);
                             sparam_typs = (List.map styp_of_typ func_typ.param_typs)}

let string_of_sstmt = function
  (* ADD MORE *)
  | SExpr(_) -> "SExpr"
  | _ -> "Other"

(* PRETTY PRINTING based off of printer.ml *)
let rec string_of_styp = function
    SVoid -> "svoid"
  | SFunc(e) -> "sfunc(" ^
                (String.concat "," (List.map string_of_styp e.sparam_typs)) ^ "; "
                ^ (string_of_styp e.sreturn_typ) ^ ")"
  | SInt -> "sint"
  | SFloat -> "sfloat"
  | SBool -> "sbool"
  | SString -> "sstring"

let fmt_sparams l =
  let fmt_p = function
      (t, n) -> String.concat "" ["("; string_of_styp t; ", "; n; ")"] in
  fmt_list (List.map fmt_p l)

let rec string_of_sexpr (_,s) =
  (match s with
     SIntLit(l) -> fmt_one "IntLit" (string_of_int l)
   | SFloatLit(l) -> fmt_one "FloatLit" l
   | SStrLit(l) -> fmt_one "StrLit"  l
   | SBoolLit(l) -> fmt_one "BoolLit" (string_of_bool l)
   | SId(s) -> fmt_one "Id" s
   | SBinop(e1, o, e2) -> (string_of_sexpr e1) ^ "\n    " ^ (string_of_op o) ^ "\n    "
                          ^ (string_of_sexpr e2)
   | SUnop(uo, e) -> fmt_two "Unop" (string_of_uop uo) (string_of_sexpr e)
   | SAssign(e1, o, e2) -> fmt_three "Assign" (string_of_sexpr e1) (string_of_op o) (string_of_sexpr e2)
   | SCall(se, a) -> "SFCall(\n      " ^ ((string_of_sexpr se) ^ "\n") ^ ("      "
                                ^ fmt_list (List.map string_of_sexpr a) ^ "\n    )")
   (* below actually is parsed with {name = e.name; param = e.params;
    * typ = e.typ; body = e.body}. See test programs for examples. *)
   | SFExpr(s) -> fmt_three "FExpr" (fmt_sparams s.sparams)
                    (string_of_styp s.styp) (fmt_sstmt_list s.sbody)
   | SClosure(clsr) -> fmt_two "Closure" (string_of_int clsr.ind)
                        (fmt_list (List.map (fun (t, n) -> string_of_styp t ^ " " ^ n)
                        clsr.free_vars))
   | SNoexpr -> ""
  )

and fmt_smembers l =
  let fmt_m = function
      (t, n, None) -> fmt_three "" (string_of_styp t) n "None"
    | (t, n, Some(e)) -> fmt_three "" (string_of_styp t) n (string_of_sexpr e) in
  fmt_list (List.map fmt_m l)

and fmt_sinit l =
  let fmt_i (n, e) = fmt_two "" n (string_of_sexpr e) in
  fmt_list (List.map fmt_i l)

and fmt_sstmt = function
    SExpr(se) -> string_of_sexpr se
  | SReturn(e) -> "Return " ^ (string_of_sexpr e)
  | SDecl (t, n, l) -> (string_of_styp t) ^ " " ^ n ^ " = " ^ (string_of_sexpr l)
  | SFor (init, e2, e3, s) ->
    fmt_four "ForLoop"
      (string_of_sexpr init)
      (string_of_sexpr e2)
      (string_of_sexpr e3) (fmt_sstmt s)
  | SWhile (e,s) -> fmt_two "while" (string_of_sexpr e) (string_of_sstmt s)
  | SIf(e, tL, fL) -> fmt_three "If" (string_of_sexpr e) (fmt_sstmt tL)
                        (fmt_sstmt fL)
  | SBlock(_) -> "SVBlock"

and fmt_sstmt_list ?spacer l =
  let sstmts = List.map fmt_sstmt l in
  let s = match spacer with Some(s) -> s | _ -> "" in
  let sstmts = List.map (fun x -> s ^ x) sstmts in
  String.concat "\n" sstmts

and fmt_opt_sexpr = function
    None -> ""
  | Some(e) -> string_of_sexpr e

let string_of_sprogram sast =
  String.concat ";\n" (List.map fmt_sstmt sast)
