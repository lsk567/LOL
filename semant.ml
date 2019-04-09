(* Check semantics and convert AST to SAST if semantics is correct *)

open Ast
open Sast

module StringMap = Map.Make (String)

exception Type_mismatch of string
exception Undeclared_reference of string


(* Built-in Functions *)

let builtins_func = [

  ("gsl_test", Func({ param_typs = [String]; return_typ = Void }));

  ("printhw", Func({ param_typs = [String]; return_typ = Void })); (* test *)

  (* Printing *)
  ("println", Func({ param_typs = [String]; return_typ = Void }));
  ("print", Func({ param_typs = [String]; return_typ = Void }));
  (* Casting *)
  ("int_of_float", Func({ param_typs = [Float]; return_typ = Int }));
  ("float_of_int", Func({ param_typs = [Int]; return_typ = Float }));
  (* String *)
  ("str_of_int", Func({ param_typs = [Int]; return_typ = String }));
  ("int_of_str", Func({ param_typs = [String]; return_typ = Int }));

  ("str_of_bool", Func({ param_typs = [Bool]; return_typ = String }));
  ("str_of_float", Func({ param_typs = [Float]; return_typ = String }));

  ("string_concat", Func({ param_typs = [String; String]; return_typ = String }));
  ("string_equals", Func({ param_typs = [String; String]; return_typ = Int }));
]

let builtins = [

  ("gsl_test", SFunc({ sparam_typs = [SString]; sreturn_typ = SVoid; sbuiltin = true; }));

  ("printhw", SFunc({ sparam_typs = [SString]; sreturn_typ = SVoid; sbuiltin = true; }));



  (* Printing *)
  ("println", SFunc({ sparam_typs = [SString]; sreturn_typ = SVoid;
    sbuiltin = true; }));
  ("print", SFunc({ sparam_typs = [SString]; sreturn_typ = SVoid;
    sbuiltin = true; }));
  (* Casting *)
  ("int_of_float", SFunc({ sparam_typs = [SFloat];
    sreturn_typ = SInt; sbuiltin = true; }));
  ("float_of_int", SFunc({ sparam_typs = [SInt];
    sreturn_typ = SFloat; sbuiltin = true; }));
  (* String *)
  ("str_of_int", SFunc({ sparam_typs = [SInt]; sreturn_typ = SString;
    sbuiltin = true; }));
  ("int_of_str", SFunc({ sparam_typs = [SString]; sreturn_typ = SInt;
    sbuiltin = true; }));
  ("str_of_bool", SFunc({ sparam_typs = [SBool]; sreturn_typ = SString;
    sbuiltin = true; }));
  ("str_of_float", SFunc({ sparam_typs = [SFloat]; sreturn_typ = SString;
    sbuiltin = true; }));

  ("string_concat", SFunc({ sparam_typs = [SString; SString];
    sreturn_typ = SString; sbuiltin = true; }));
  ("string_equals", SFunc({ sparam_typs = [SString; SString];
    sreturn_typ = SInt; sbuiltin = true; }));
]

(* Helper functions *)
let rec compare_typs t1 t2 =
match t1, t2 with
    SFunc(f1), SFunc(f2) ->
      let same_ret = compare_typs f1.sreturn_typ f2.sreturn_typ in
      let same_args = List.for_all2 compare_typs f1.sparam_typs f2.sparam_typs
      in same_ret && same_args
  | _ -> t1 = t2


let check_asn lvalue_t rvalue_t =
  let found_match = compare_typs lvalue_t rvalue_t in
  if found_match
  then lvalue_t
  else
    (print_endline(Printexc.raw_backtrace_to_string(Printexc.get_callstack 100));
      raise (Type_mismatch ("type mismatch error " ^ fmt_styp lvalue_t ^ " " ^ fmt_styp rvalue_t)))

let rec styp_of_typ ctxt =
function
    Int -> SInt
  | Bool -> SBool
  | Float -> SFloat
  | String -> SString
  | Void -> SVoid
  | Func f -> SFunc({ sparam_typs = List.map (styp_of_typ ctxt) f.param_typs;
      sreturn_typ = styp_of_typ ctxt f.return_typ; sbuiltin = false; })

let sfunc_of_func ctxt func builtin=
  let (name,func) = func in
  let func = match func with Func(f) -> f | _ -> raise (Failure ("shouldn't happen")) in
  let sparam_typs = List.fold_left (fun l typ -> (styp_of_typ ctxt typ)::l) [] func.param_typs in
  (name, SFunc( {sparam_typs = sparam_typs; sreturn_typ = styp_of_typ ctxt func.return_typ; sbuiltin = builtin;}))


(*
StringMap of Built-in functions.
StringMap in the form of string: Func pair
*)
let builtin_map = List.fold_left (fun m fd -> StringMap.add (fst fd) (snd fd) m)
  StringMap.empty builtins_func

(* Context functions, which is a list of StringMap *)
let rec find_in_ctxt (v_name : string) (ctxt : styp StringMap.t list) =

(*
Returns a tuple with the type and the map and if
the variable is initalized or not. The type and
map are optional.
*)
  try
    StringMap.find v_name (List.hd ctxt)
  with Not_found -> match List.tl ctxt with
      [] -> raise (Undeclared_reference ("undeclared reference " ^ v_name))
    | tail -> find_in_ctxt v_name tail

let context_to_bindings (ctxt : styp StringMap.t list) =
  let combine_scopes s1 s2 = StringMap.union (fun _ v1 _ -> Some(v1)) s1 s2 in
  let map = List.fold_left combine_scopes StringMap.empty ctxt in
  StringMap.bindings map

(*
This function takes a tuple with the type and the map
as well as the variable name and the context map.
The map in the tuple is used for the member fields
in structs. The map is None unless you are adding a new
struct type.
*)
let add_to_ctxt (v_type : styp) (v_name : string) (ctxt : styp StringMap.t list) =
  let map = List.hd ctxt in
  try
    match (StringMap.find v_name map) with
      _ -> raise (Failure (v_name ^ " already declared"))
  with Not_found ->
    let newMap = StringMap.add v_name v_type map in
    newMap::List.tl ctxt

(* Convert built-in function from Func to SFunc and add to context list *)
let def_ctxt =
  let add_func ctxt (name, func_t) = add_to_ctxt func_t name ctxt in
  List.fold_left add_func [StringMap.empty] builtins

(* Checking *)

let rec check_bool_expr (ctxt : styp StringMap.t list) e =
  (* helper function *)
  let (t, st) = check_expr ctxt e in
      if (t <> SBool) then
        raise (Failure("Error: " ^ fmt_styp t ^ " is not a boolean type"))
      else (t, st)

(*
Checks Expression. This is the lowest level

Parameters:
- ctxt : context

Returns:
- sexpr, which is of type (Styp*Sx)
*)
and check_expr (ctxt : styp StringMap.t list) = function
    IntLit(x) -> (SInt, SIntLit x)
  | BoolLit(x) -> (SBool, SBoolLit x)
  | FloatLit(x) -> (SFloat, SFloatLit x)
  | StrLit(x) -> (SString, SStrLit x)
    (*
    Variable or Function invoking.

    Parameters:
    - v (String): name of the variable/function.
    - vv (styp): Type of the variable/function. Function can only be SFunc.

    Returns:
    - Tuple of (v,styp of v). Variables with ~ are stipped away of ~.
    *)
  | Id(v) ->
      (* Variable with ~ are created temporary variables for checking built-in functions *)
      let vv = find_in_ctxt (if String.contains v '~' then String.sub v 1
          ((String.length v) - 1) else v) ctxt in
      if (String.contains v '~') then
          (vv, SId (String.sub v 1 ((String.length v) - 1)))
      else (match vv with
        (* SFunc built-in functions only *)
        SFunc(sfunc_typ) when sfunc_typ.sbuiltin ->
          let ft = match StringMap.find v builtin_map with
                Func(func) -> func
              | _ -> raise (Failure ("shouldn't happen")) in
          check_expr ctxt (FExpr({ name = ""; typ = ft.return_typ;
            params = List.mapi (fun i t -> (t, "__p" ^ (string_of_int i))) ft.param_typs;
            body = if ft.return_typ == Void then [
              Expr(Call(Id("~" ^ v),
              List.mapi (fun i _ -> Id("__p" ^ (string_of_int i))) ft.param_typs))
            ] else [
              FDecl(ft.return_typ, "__ret", Call(Id("~" ^ v),
              List.mapi (fun i _ -> Id("__p" ^ (string_of_int i))) ft.param_typs));
              Return(Id("__ret"))
            ];}))
        (* Anything else except built-in functions*)
      | _ -> (vv, SId v))

  | Assign(e1, e2) ->
      let (t1, se1) = match e1 with
          Id(n) -> let t = find_in_ctxt n ctxt in (t, SId n)
        | _     -> check_expr ctxt e1
      in
      let (t2, se2) = check_expr ctxt e2 in
      (check_asn t1 t2, SAssign((t1, se1), (t2, se2)))
  | Binop(e1, op, e2) ->
      let (lt, se1) = check_expr ctxt e1 in
      let (rt, se2) = check_expr ctxt e2 in
      let sbinop = SBinop((lt, se1), op, (rt, se2)) in
      (match op with
          Add | Sub | Mul | Div when lt = SInt && rt = SInt
          -> (SInt, sbinop)
        | Add | Sub | Mul | Div when lt = SFloat && rt = SFloat
          -> (SFloat, sbinop)
        | Add when lt = SString && rt = SString -> (SString,sbinop) (*string concat*)
        | Equal | Neq  when lt = rt -> (SBool, sbinop)
        | Less | Leq | Greater | Geq
            when (lt = SInt && rt = SInt) || (lt = SFloat || rt = SFloat) ->
            (SBool, sbinop) (* String comparison not defined *)
        | And | Or when lt = SBool && rt = SBool -> (SBool, sbinop)
        | _ -> raise (Failure("Error: cannot use " ^ fmt_op op ^
                              " with types: "^ fmt_styp rt ^ " and " ^ fmt_styp lt )))
  | Unop(op, e) ->
      let (t, e) = check_expr ctxt e in
      let sunop = SUnop(op, (t, e)) in
      (match op with
          Neg when t = SInt -> (SInt, sunop)
        | Neg when t = SFloat -> (SFloat, sunop)
        | Not when t = SBool -> (SBool, sunop) (* Maybe use ! instead of this?*)
        | _ -> raise (Type_mismatch "Type mismatch for unary operator"))
  (*
  Checks expr results in SFunc, and then checks each argument revursively
  in the helper function.
  Helper function: Returns a list of (styp*sx)
  Parameters:
  - expr (expr): name of expr
  - args (expr list): list of (typ * sx)
  *)
  | Call(expr, args) ->
      let check_args f_type args =
        let rec helper l = function
            ([], []) -> l
          | (p_typ::pl, arg::al) -> (* pl: param_typs list, al: arg list*)
            let (styp,sx) = check_expr ctxt arg in
            if compare_typs p_typ styp
              then helper ((styp,sx)::l) (pl, al)
              else raise (Failure "argument type mismatch")
          | _ -> raise (Failure "invalid number of arguments")
        in helper [] (f_type.sparam_typs, args)
      in
      (* Check expression on left *)
      let (styp,sx) = match expr with
          Id(s) when not (String.contains s '~') -> (find_in_ctxt s ctxt, SId(s))
        | _ -> check_expr ctxt expr
      in
      (* to SAST SFUNC *)
      let (func_t, sxx) = match styp with
          SFunc(func_t) -> (func_t, check_args func_t args)
        | _ -> raise (Failure "not a function")
      in
      (func_t.sreturn_typ, SCall((styp, sx), sxx))
  (*
  Function Expression
  Create_scope: returns a Stringmap of name: styp pair from a list

  Parameters:
  fexpr(fexpr) : struct {param_typs: typ list; return_typ: typ; builtin: bool;}

  *)
  | FExpr(fexpr) ->
      let conv_params (typ, _ ) = (styp_of_typ ctxt typ) in
      let conv_params_with_both_fields (typ, str) = (styp_of_typ ctxt typ,str) in

      let sfunc_t = SFunc({
          sreturn_typ = styp_of_typ ctxt fexpr.typ;
          sparam_typs = List.map conv_params fexpr.params;
          sbuiltin = false;
      }) in
      let create_scope list =
        let rec helper m = function
            []         -> m
          | (t, n)::tl ->
                let new_m = StringMap.add n (styp_of_typ ctxt t) m in
                helper new_m tl
        in
        if fexpr.name <> ""
        then helper (StringMap.add fexpr.name sfunc_t StringMap.empty) list
        else helper StringMap.empty list
      in
      let func_scope = create_scope fexpr.params in
      let (_, return_t, sl) = check_stmt_list (func_scope::ctxt) fexpr.body in
          ignore (check_asn return_t (styp_of_typ ctxt fexpr.typ)); (* typ *)
          (sfunc_t, SFExpr({
               sname = fexpr.name;
               styp = styp_of_typ ctxt fexpr.typ;
               sparams = List.map conv_params_with_both_fields fexpr.params;
               sbody = sl;
             }))
  | Noexpr -> (SVoid, SNoexpr)
  (*
  | _ as x -> print_endline(Ast.fmt_expr x); raise (Failure "not implemented in semant")
  *)

(*
Checks statment case wise.

Parameters:
- ctxt(styp StringMap.t list): context

Returns:
- nctxt (styp StringMap.t list): updated context
- ret: return type, SVoid for everything except Return
- ss:ssl: sstmt
*)
and check_stmt (ctxt : styp StringMap.t list) = function
    Expr(expr) -> let (t, ss) = check_expr ctxt expr in (ctxt, SVoid, SExpr((t, ss)))
  | VDecl(ltyp, id, init) ->
    (* Updates context, return void *)
      let sltyp = styp_of_typ ctxt ltyp in
        (match init with
            None -> (add_to_ctxt sltyp id ctxt, sltyp, SVDecl(sltyp, id, None)) (*No initialization sets to None*)
          | Some(expr) ->
              let (t_i, s_i) = check_expr ctxt expr in
                let nctxt = add_to_ctxt sltyp id ctxt in
                (nctxt, SVoid, SVDecl((check_asn t_i sltyp), id, Some((t_i, s_i)))))
  | FDecl(ltyp, id, expr) ->
    (* Updates context, return void *)
      let sltyp = styp_of_typ ctxt ltyp in
        let (t_i, s_i) = check_expr ctxt expr in
                let nctxt = add_to_ctxt sltyp id ctxt in
                (nctxt, SVoid, SFDecl((check_asn t_i sltyp), id, (t_i, s_i)))
  | Return(e) ->
    (* Returns same context, returns what it returns*)
      let (t, ss) = check_expr ctxt e in (ctxt, t, SReturn((t, ss)))
  | For (loop_init, e2, e3, st) ->
      (* Returns same context, returns whatever the final statement in the loop returns*)
      let (ctxt1, s1') = match loop_init with (* Loop init is actually statement*)
          None -> (StringMap.empty::ctxt, None)
        | Some(s1) -> (let (nctxt, _, s1) = check_stmt (StringMap.empty::ctxt) s1 in
                       (nctxt, Some(s1)))
      in
      let (ctxt2, e2') = match e2 with
          None -> (ctxt1, None)
        | Some(e2) -> (let (t_i, si) =
                         check_bool_expr ctxt1 e2 in (ctxt1, Some((t_i, si))))
      in
      let (ctxt3, e3') = match e3 with
          None -> (ctxt2, None)
        | Some(e3) -> (let (t_i, si) =
                         check_expr ctxt2 e3 in (ctxt2, Some((t_i, si))))
      in
      let (_, ret_t, st') = check_stmt_list ctxt3 st
      in
      (ctxt, ret_t, SFor(s1', e2', e3', st'))

  | If (e, st1, st2) ->
      (* Returns same context. returns given the two statements*)
      let e' = check_bool_expr ctxt e (* (t,s) *)
      in
      let ifctxt = StringMap.empty::ctxt in
      let (_, rt1, st1') = check_stmt_list ifctxt st1 (* If statement *)
      in
      let (_, rt2, st2') = check_stmt_list ifctxt st2 (* else, elseif stuff *)
      in
      let rt = match rt1, rt2 with (* if any is void, returns void *)
          (SVoid, _) -> SVoid
        | (_, SVoid) -> SVoid
        | (t1, t2) -> check_asn t1 t2
      in
      (ctxt, rt, SIf(e', st1', st2'))

  | _ -> (ctxt, SVoid, SExpr((SVoid, SNoexpr))) (* otherwise NoExpr *)

(*
Checks list of statment by checking each statement.

Parameters:
- ctxt: context

Returns:
- nctxt (styp StringMap.t list): updated context
- ret: return type, default SVoid
- ss:ssl: stype
 *)
and check_stmt_list (ctxt : styp StringMap.t list) = function
      [] -> (ctxt, SVoid, [])
    | hd::tl ->
      let (nctxt, t, ss) = check_stmt ctxt hd in
      let (nctxt, t_rest, ssl) = check_stmt_list nctxt tl in
      let ret =
        if t = SVoid
          then t_rest
        else (if List.length tl <> 0 then raise
          (Failure "dead code after return") else (); t)
      in (nctxt, ret, ss::ssl)
(*
Semantic checking of the AST. Returns a list of SAST if successful,
throws an exception if something is wrong.

Parameters:
- prog (stmt list): program to be checked

Returns:
- List of Sast
*)
and check_program (prog : stmt list) =

if List.exists (fun x -> match x with Return(_) -> true | _ -> false) prog
  then raise (Failure "illegal return statement") (* program with only return statement is invalid *)
else
  let (_, _, ssl) = check_stmt_list def_ctxt prog in
    List.concat( List.map (fun s -> match s with
        SBlock(sl) -> sl
      | _ as x -> [x]) ssl)
