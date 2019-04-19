(* Check semantics and convert AST to SAST if semantics is correct *)

open Ast
open Sast
open Builtins

module StringMap = Map.Make (String)

(* The main function in semantics.
* Code can be broken in 5 sections: Helper functions, check expressions, check expression list,
* check stmt, and stmt list.
*)
let check statements =
  let empty_func styp = ({ sreturn_typ = styp; sparam_typs = [] }) in
  let built_in_decls = List.fold_left (fun map (name,ty) -> StringMap.add name ty map) StringMap.empty Builtins.builtins
  in
  (* Helper function for Id *)
  let type_of_id symbol_table s =
      try StringMap.find s symbol_table
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in
  let add_bind  map (ty, name) = match ty with
        Func _ -> StringMap.add name (SFunc(empty_func SVoid)) map
      | _ -> StringMap.add name (styp_of_typ ty) map
  in
  (* Helper function for empty init. Match styp to the coresponding sx*)
  let empty_sx styp = match styp with
  SInt -> SIntLit (0)
| SFloat -> SFloatLit ("0.0")
| SBool -> SBoolLit (false)
| SString -> SStrLit ("")
| SList _ -> SListLit ([])
| _ -> raise (Failure ("Empty of " ^ (string_of_styp styp) ^ "not implemented or shouldn't happen"))

in

  (* List Helper Func*)
  let rec check_list_type symbol_table = function
      [] -> SEmpty
    | [x] -> let (t,_) = check_expr symbol_table x in t
    | x :: y :: rest ->
          let (t1,_) = check_expr symbol_table x in
          let (t2,_) = check_expr symbol_table y in
          if t1 = t2 then check_list_type symbol_table (y :: rest)
          else raise (Failure(
              "cannot have elements of differing types " ^ string_of_styp t1 ^
              " and " ^ string_of_styp t2 ^ " in same list"))
  (* Helper function for control flow checking bool *)
  and check_bool_expr symbol_table e =
    let (t',e') = check_expr symbol_table e in
    match t' with
        SBool -> (t',e')
      | _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e ))

  (* Helper function for comparing left and right. If both are the same return the typ*)
  and infer_typ lt rt = match (lt,rt) with
        (_, SVoid) -> lt
      | (SFunc f1, SFunc f2) ->
        let same_ret = infer_typ f1.sreturn_typ f2.sreturn_typ in
        let same_args = List.fold_left2 (fun l f1' f2' -> (infer_typ f1' f2')::l) [] f1.sparam_typs f2.sparam_typs in
        let infer_func = { sreturn_typ = same_ret; sparam_typs = same_args } in
        SFunc (infer_func)
      (* List *)
      | (_, SEmpty) -> lt
      | (SList t1, SList t2) -> SList (infer_typ t1 t2)
      | _ -> if (lt = rt) then lt
             else raise (Failure ("illegal assignment " ^ string_of_styp lt ^ " = " ^ string_of_styp rt))

(* Check expr. Returns sexpr, a tuple of (styp * sx) *)
and check_expr symbol_table ?fname = function
    IntLit l -> (SInt, SIntLit l)
  | FloatLit l -> (SFloat, SFloatLit l)
  | BoolLit l -> (SBool, SBoolLit l)
  | StrLit l -> (SString, SStrLit l)
  | Noexpr -> (SVoid, SNoexpr)
  (* Ids and Funcs *)
  | Id s -> (type_of_id symbol_table s, SId s)
  (* Func Expressions
  * Checks parameters, the body and finally the return type
  *)
  | FExpr fn ->
    (* parameters *)
    let map = List.fold_left add_bind symbol_table fn.params in
    (* Empty function to be filled *)
    let map' = match fname with Some x -> StringMap.add x (SFunc (empty_func SVoid)) map
                                | None -> map
    in (* Check body's statments *)
    let ( istmt , _ , _ ) = List.fold_left check_stmt
        ( [] , map', Some (styp_of_typ fn.typ)) fn.body in
    (* Check return. If it is of type function, search type from statments*)
    let get_ret t = match t with
        (* Anoynmous function need to check later*)
        Func _ ->
        (* Helper function for finding the return type from statment*)
        let rec ret_search x stmt= match stmt with
          SBlock stlst -> List.fold_left ret_search x stlst
          | SReturn (t,_) -> t
          | SIf (_, b1,b2) -> ret_search (ret_search x b1) b2
          | SFor ( _, _, _, st) -> ret_search x st
          | SWhile (_,s) -> ret_search x s
          | _ -> x
        in
        List.fold_left ret_search SVoid istmt
      | _ -> (styp_of_typ t)
    in
    let sty = get_ret fn.typ
    in
    (SFunc(empty_func SVoid), SFExpr ({
      styp = sty;
      sparams = List.map (fun (xtyp, str) -> (styp_of_typ xtyp,str)) fn.params;
      sbody = List.rev istmt
    }) )
  (* Call a function. Decompose expr into SFunc *)
  | Call(expr, args) ->
    (* May need to check args*)
    let (t, se) = (match expr with
        Id(s) -> (type_of_id symbol_table s, SId s)
      | _ -> check_expr symbol_table expr)
    in (match t with
      SFunc(func_t) -> (func_t.sreturn_typ, SCall((t,se), List.map (check_expr symbol_table) args))
    | _ -> raise (Failure "not a function"))
  | Assign(e1,op,e2) ->
    let (lt, le') = check_expr symbol_table e1
    and (rt, re') = check_expr symbol_table e2 in
    (* MAY NEED TO CHECK IF LEFT = RIGHT *)
    ((infer_typ lt rt) , SAssign((lt, le'), op, (rt, re')))
  (* Arithmetics *)
  | Binop(e1, op, e2) as e->
    let (lt, le') = check_expr symbol_table e1
    and (rt, re') = check_expr symbol_table e2
    in
    let infered_typ = infer_typ lt rt in
    ( (match op with
        (* May need to add cross type op *)
          Add | Sub | Mul | Div | Mod | Pow when infered_typ = SInt -> SInt
        | Add | Sub | Mul | Div when infered_typ = SFloat -> SFloat
        | Add                       when infered_typ = SString -> SString
        | Add                       when (match infered_typ with SList _ -> true | _ -> false) -> infered_typ
        | Equal | Neq                 -> SBool
        | Less | Leq | Greater | Geq when infered_typ  = SInt || infered_typ = SFloat -> SBool
        | And | Or when infered_typ = SBool -> SBool
        | _ -> raise (Failure ("illegal binary operator " ^ string_of_styp lt ^ " " ^ string_of_op op ^ " " ^ string_of_styp rt ^ " in " ^
      string_of_expr e))), SBinop((lt, le'), op, (rt, re')) )
  | Unop(op, e) as ex ->
    let (t, e') = check_expr symbol_table e in
    ( (match op with
         Neg when t = SInt -> SInt
       | Neg when t = SFloat -> SFloat
       | Not when t = SBool -> SBool
       | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^ " " ^ string_of_styp t ^
      " in " ^ string_of_expr ex))), SUnop(op, (t, e')) )
  (* List *)
  | ListLit(l) -> (SList (check_list_type symbol_table l),  SListLit (List.map (check_expr symbol_table) l))
  | ListAccess(e1,e2) ->
    let (t1, se1) = check_expr symbol_table e1
    and (t2, se2) = check_expr symbol_table e2
    in
    let t3 = match t1 with
        SList(t) -> t
      | _ -> raise (Failure ("Not a list: " ^ string_of_expr e1))
    in match t2 with
        SInt _ -> (t3, SListAccess((t1, se1), (t2, se2)))
      | _ -> raise (Failure ("can't access list with non-integer type"))

and check_expr_list symbol_table expr_list = List.map (check_expr symbol_table) expr_list
(* Checks statement
* Keeps a list of SAST, the context (symbol_table), and the return type
*)
and check_stmt (curr_lst, symbol_table,return_typ)  = function
    Block(stmt_list) ->
      let (istmts,_,_) = List.fold_left check_stmt (curr_lst,symbol_table,return_typ) stmt_list
      in (SBlock (List.rev istmts) :: curr_lst , symbol_table,return_typ)
  | Expr(e) -> (SExpr (check_expr symbol_table e):: curr_lst, symbol_table,return_typ)
  | Decl(t,s,e) as exp -> (* THIS MAY BE WRONG *)
    (match e with
        None -> let ty = match t with
            Func _ -> raise (Failure ("Cannot declare an uninitialized function."))
          | typ -> styp_of_typ typ
        in
          (SDecl (ty, s, (SEmpty,empty_sx ty))::curr_lst, StringMap.add s ty symbol_table,return_typ)
      | Some(e) ->
      let (t',e') = match t with
          Function | Func _ -> check_expr symbol_table e ~fname:s
        | _ -> check_expr symbol_table e
      in
      if StringMap.mem s built_in_decls
      then raise (Failure ("Variable name cannot be a built-in function" ^ (string_of_stmt exp)))
      else let ty = match t with
        | Func _ -> if t' = SVoid then SFunc (empty_func SVoid) else t'
        | typ -> styp_of_typ typ
      in
        (SDecl (infer_typ ty t', s, (t',e')):: curr_lst, StringMap.add s ty symbol_table,return_typ)
    )
  | Return  e -> let (t1,e1) = check_expr symbol_table e in
    let t = match return_typ with
        Some t2 -> infer_typ t2 t1
      | None -> raise (Failure "Cannot return if not within a function")
    in (SReturn (t,e1) :: curr_lst, symbol_table,return_typ)
  | If(e,st1,st2) ->
      let (st1',_,_) = check_stmt ([],symbol_table, return_typ) st1
      and (st2',_,_) = check_stmt ([],symbol_table,return_typ) st2
      in
      (SIf ( check_bool_expr symbol_table e , List.hd st1', List.hd st2')::curr_lst, symbol_table, return_typ)
  | For(e1,e2,e3,st) ->
      let (ist,_,_) = check_stmt ([],symbol_table,return_typ) st in
      (SFor(check_expr symbol_table e1, check_bool_expr symbol_table e2, check_expr symbol_table e3, List.hd ist) :: curr_lst, symbol_table,return_typ)
  | While (p,s) -> let (is,_,_) = check_stmt ([],symbol_table,return_typ) s in
      (SWhile(check_bool_expr symbol_table p, List.hd is):: curr_lst, symbol_table,return_typ)
in

let (stmts, _,_) = List.fold_left check_stmt ([],built_in_decls,None) statements
  in SBlock (List.rev stmts) :: []
