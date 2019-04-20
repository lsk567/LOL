open Sast
open Builtins

module StringMap = Map.Make(String)

(* Some types for lifting *)
type environment = {
  variables: styp StringMap.t;
  parent: environment option;
}

(* Lifted function *)
type lfexpr = {
  lname: string;
  lfvs: sbind list; (* Free variables *)
  lreturn_typ: styp;
  lparams: sbind list;
  lbody: sstmt list;
}

(* For inbuilt functions, create empty function types *)
let built_in_decls = List.fold_left (fun map (name,ty) -> StringMap.add name ty map) StringMap.empty Builtins.builtins

(* Look up function: traverse up the tree until we encounter a symbol *)
let rec lookup (e : environment) name =
  try
    StringMap.find name e.variables
  with Not_found -> match e.parent with
      Some(parent) -> lookup parent name
    | None ->
      try StringMap.find name built_in_decls
      with Not_found -> raise (Failure ("Lift: undeclared identifier " ^ name))

let add_bind m (t, id) = StringMap.add id t m

let rec dfs_sstmt funcs env sstmt =
  (* Perform dfs of statements.
   * HANDLING SCOPE:
   * When we pass scope to a block,
   * the block can see all the variables in the current scope,
   * but its local variables are invisible to us, so we simply
   * pass in our environment and ignore the environment that
   * the block generates.
   * The only time where we actually change our environment is
   * when we encounter a declaration.
   * ---
   * HANDLING FREE VARIABLES:
   * The dfs functions return the free variables from the statement
   * or expression being explored. These free variables were not in
   * the immediate scope for them, but it might be in scope for us --
   * consider the case when there is a function expression nested within
   * a function expression. Therefore we have to check if the variables
   * are indeed not in our immediate scope.
   * ---
   * Note that in general scopes persist, the only case where we get a
   * actual new scope is when we get a function expression, where we need
   * to create a new scope for that function expression to detect free
   * variables. *)
    let (funcs', fvs', env', sstmt') =
    match sstmt with
        SBlock (stmts) ->
        let (funcs1, fvs1, _, stmts1) = dfs_sstmts funcs env stmts
        in (funcs1, fvs1, env, SBlock(stmts1))
      | SDecl(styp, id, sexpr) ->
        let (funcs1, fvs1, sexpr1) = dfs_sexpr funcs env sexpr ~fname:id in
        let (styp1, _) = sexpr1 in
        let new_typ = match (styp, styp1) with
            SFunc(_), SFunc(_) -> styp1
          | _ -> styp
        in
        let new_env = {variables = StringMap.add id new_typ env.variables;
                       parent = env.parent}
        in
        (funcs1, fvs1, new_env, SDecl(new_typ, id, sexpr1))
      | SReturn e ->
        let (funcs1, fvs1, e1) = dfs_sexpr funcs env e in
        (funcs1, fvs1, env, SReturn(e1))
      | SIf(e, s1, s2) ->
        let (funcs1, fvs1, e') = dfs_sexpr funcs env e in
        let (funcs2, fvs2, _, s1') = dfs_sstmt funcs1 env s1 in
        let (funcs3, fvs3, _, s2') = dfs_sstmt funcs2 env s2 in
        (funcs3, List.concat [fvs1; fvs2; fvs3], env, SIf(e', s1', s2'))
      | SExpr e ->
        let (funcs1, fvs1, e1) = dfs_sexpr funcs env e in
        (funcs1, fvs1, env, SExpr(e1))
      | SFor (e1, e2, e3, body) ->
        let (funcs1, fvs1, e1') = dfs_sexpr funcs env e1 in
        let (funcs2, fvs2, e2') = dfs_sexpr funcs1 env e2 in
        let (funcs3, fvs3, e3') = dfs_sexpr funcs2 env e3 in
        let (funcs4, fvs4, _, body') = dfs_sstmt funcs3 env body in
        (funcs4, List.concat [fvs1; fvs2; fvs3; fvs4], env, SFor(e1', e2', e3', body'))
      | SWhile(e, s) ->
        let (funcs1, fvs1, e') = dfs_sexpr funcs env e in
        let (funcs2, fvs2, _, s') = dfs_sstmt funcs1 env s in
        (funcs2, List.concat [fvs1; fvs2], env, SWhile(e', s'))
      | _ -> print_endline(string_of_sstmt sstmt); raise (Failure "not implemented in lifter")
    in
    let check_scope (_, fv) = not (StringMap.mem fv env.variables) in
    let fvs' = List.filter check_scope fvs' in
    (funcs', fvs', env', sstmt')

and dfs_sstmts funcs env sstmts = match sstmts with
    sstmt :: rest ->
    let (funcs1, fvs1, env1, sstmts1) = dfs_sstmt funcs env sstmt in
    let new_env = { variables = List.fold_left add_bind env1.variables fvs1;
                    parent = env1.parent}
    in
    let (funcs2, fvs2, env2, sstmts2) = dfs_sstmts funcs1 new_env rest in
    (funcs2, List.concat [fvs1; fvs2], env2, sstmts1::sstmts2)
  | [] -> (funcs, [], env, sstmts)

and dfs_sexpr ?fname funcs env (t, expr) =
  let check_scope (_, fv) = not (StringMap.mem fv env.variables) in
  let (funcs', fvs', expr') = match expr with
      SFExpr(sfexpr) ->
      (* Build a closure from the function expression
      * and return the list of free variables from
      * the function expression. Then do another check to
      * see if the free variables from the scope of the function
      * expression are still in the current scope. If not, then
      * add that variable to the free variable list of the current
      * scope *)
      let (funcs1, fvs1, (st, clsr)) = match fname with
          Some x -> build_closure funcs env sfexpr ~fname:x
        | None -> build_closure funcs env sfexpr
      in
      let fvs1' = List.filter check_scope fvs1 in
      (funcs1, fvs1', (st, SClosure(clsr)))
    | SAssign(se1, op, se2) ->
      let (funcs1, fvs1, se1') = dfs_sexpr funcs env se1 in
      let (funcs2, fvs2, se2') = dfs_sexpr funcs1 env se2 in
      (funcs2, List.concat [fvs1; fvs2], (t, SAssign(se1', op, se2')))
    | SId(sid) -> let fv =
        if StringMap.mem sid env.variables || StringMap.mem sid built_in_decls
        then []
        else [lookup env sid, sid]
      in
      (funcs, fv, (t, SId(sid)))
    | SBinop(se1, op, se2) ->
      let (funcs1, fvs1, se1') = dfs_sexpr funcs env se1 in
      let (funcs2, fvs2, se2') = dfs_sexpr funcs1 env se2 in
      (funcs2, List.concat [fvs1; fvs2], (t, SBinop(se1', op, se2')))
    | SUnop(op, se) ->
      let (funcs1, fvs1, se') = dfs_sexpr funcs env se in
      (funcs1, fvs1, (t, SUnop(op, se')))
    | SCall((lt, se), args) ->
      (match se with
         SId(s1) -> let fv' =
           if StringMap.mem s1 env.variables || StringMap.mem s1 built_in_decls
           then None
           else Some(lookup env s1, s1)
         in
         let (funcs1, fvs1, args') = dfs_sexprs funcs env (List.rev args) in
         let fvs' = match fv' with
             Some(x) -> x :: fvs1
           | _ -> fvs1
         in (funcs1, fvs', (t, SCall((lt, se), args')))
       | _ -> (* Need this for recursion. *)
         let (funcs1, fvs1, _) = dfs_sexpr funcs env (lt, se) in
         let (funcs2, fvs2, args') = dfs_sexprs funcs1 env args in
         (funcs2, fvs1@fvs2, (t, SCall((lt, se), args'))))
    | _ as x -> (funcs, [], (t, x))
  in
  let fvs' = List.filter check_scope fvs' in
  (funcs', fvs', expr')

and dfs_sexprs funcs env sexprs= match sexprs with
    sexpr :: rest ->
    let (funcs1, fvs1, sexpr1) = dfs_sexpr funcs env sexpr in
    let new_env = {
      variables = List.fold_left add_bind env.variables fvs1;
      parent = env.parent;
    } in
    let (funcs2, fvs2, rest) = dfs_sexprs funcs1 new_env rest in
    (funcs2, List.concat [fvs1; fvs2], sexpr1 :: rest)
  | [] -> (funcs, [], sexprs)

and build_closure ?fname funcs env fexpr =
  let vars = List.fold_left add_bind StringMap.empty fexpr.sparams in
  let name = match fname with Some x -> x | None -> "" in
  let vars_rec = match name with
      "" -> vars
    | _ -> StringMap.add name SABSTRACT vars in
  let new_env = { variables = vars_rec; parent = Some env } in
  let (funcs', fvs, _, body') = dfs_sstmts funcs new_env fexpr.sbody in
  let clsr = { ind = List.length funcs'; free_vars = fvs; } in
  let new_func = {
    lname = name;
    lfvs = fvs;
    lreturn_typ = fexpr.styp;
    lparams = fexpr.sparams;
    lbody = body'
  } in
  let func_t = {
    sparam_typs = List.map fst fexpr.sparams;
    sreturn_typ = fexpr.styp;
  } in
  (new_func :: funcs', fvs, (SFunc(func_t), clsr))

(* Lift takes a list of sast stmts, and converts to a list of (fname, func) *)
(* sstmt list -> (string * lfunc) list *)
let lift sstmts =
  let default_env = { variables = StringMap.empty; parent = None } in
  let (funcs, _, _, sstmts') = dfs_sstmts [] default_env sstmts in
  let main_func = {
    lname = "main";
    lfvs = [];
    lreturn_typ = SInt;
    lparams = [];
    lbody = sstmts'
  } in
  let name i func = ("f" ^ string_of_int i, func) in
  let named_funcs = List.mapi name (List.rev funcs) in
  (("main", main_func) :: named_funcs)

(* Pretty print *)
let fmt_lfunc f = String.concat "\n" [
    " -fvs: " ^ String.concat ""
      (List.map (fun (t, n) -> (string_of_styp t) ^ " " ^ n) f.lfvs);
    " -return_t: " ^ string_of_styp f.lreturn_typ;
    " -params: " ^ String.concat ""
      (List.map (fun (t, n) -> (string_of_styp t) ^ " " ^ n) f.lparams);
    " -lbody: \n" ^ string_of_list_sstmt f.lbody ", ";
  ]

let helper (name, f) = name ^ ":\n" ^ (fmt_lfunc f)

let rec string_of_lsast = function
    [] -> ""
| item :: rest -> String.concat "\n" [(helper item);(string_of_lsast rest)]
