(*
Code generation:
Translate takes a semantically checked AST and produces LLVM IR\
*)

module L = Llvm

open Ast
open Sast
open Lift
open Builtins

module StringMap = Map.Make(String)

let translate functions =
  let context = L.global_context () in (* global data container *)
  let llmem = L.MemoryBuffer.of_file "builtins.bc" in
  let llm = Llvm_bitreader.parse_bitcode context llmem in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context (* int *)
  and i1_t       = L.i1_type     context (* nool *)
  and float_t    = L.double_type context (* float *)
  and void_t     = L.void_type   context (* void *)
  and void_ptr_t = L.pointer_type (L.i8_type context)
  and str_t      = L.pointer_type (L.i8_type context) (* string *)

  and list_t     = L.pointer_type (match L.type_by_name llm "struct.List" with
      None -> raise (Failure "Missing implementation for struct List")
    | Some t -> t)
  and lst_element_t = L.pointer_type (match L.type_by_name llm "struct.List_element" with
      None -> raise (Failure "Missing implementation for struct List_element")
    | Some t -> t)
  and matrix_t   = L.pointer_type (match L.type_by_name llm "struct.gsl_matrix" with
      None -> raise (Failure "Missing implementation for struct gsl_matrix")
    | Some t -> t)

  (* Create the LLVM compilation module into which we will generate code *)
  and the_module = L.create_module context "LOL" in

  (*
  Return the LLVM type for a LOL function type
  For non-main functions, add void_ptr in front of parameters

  Parameters:
  - ret_t: return type as styp
  - param_ts: list of parameters as styp
  *)
  let rec ltype_of_func name (ret_t : styp) param_ts =
    let param_types = (List.map ltype_of_styp param_ts) in
    let param_types =
      if name = "main" then param_types
      else void_ptr_t :: param_types
    in L.function_type (ltype_of_styp ret_t) (Array.of_list param_types)

  (* Return the LLVM type for a LOL function type *)
  and ltype_of_lfexpr name (lfexpr : lfexpr) =
    ltype_of_func name lfexpr.lreturn_typ (List.map fst lfexpr.lparams)

  (* Return the LOL return and parameter types for a LLVM type *)
  and typ_of_lfexpr lfexpr = SFunc({
    sreturn_typ = lfexpr.lreturn_typ;
    sparam_typs = List.map fst lfexpr.lparams;
  })

  (* Return the LLVM type for a LOL function type *)
  and ltype_of_sfunction name (sfunc : sfunc_typ) =
    ltype_of_func name sfunc.sreturn_typ sfunc.sparam_typs

  and ltype_of_clsr name sfexpr =
    let func_t = L.pointer_type (ltype_of_lfexpr name sfexpr) in
    L.struct_type context [|func_t; void_ptr_t|]

  and ltype_of_clsr_func name (sfunc : sfunc_typ) =
    let func_t = L.pointer_type (ltype_of_sfunction name sfunc) in
    L.struct_type context [|func_t; void_ptr_t|]

  (* Return the LLVM type for a LOL type *)
  and ltype_of_styp = function
      SInt   -> i32_t
    | SBool  -> i1_t
    | SFloat -> float_t
    | SVoid  -> void_t
    | SString -> str_t
    | SFunc(ftype) -> ltype_of_clsr_func "" ftype
    | SEmpty -> void_t
    | SList _ -> list_t
    | SListElement _ -> lst_element_t
    | SMatrix(_,_) -> matrix_t
    | SAny -> void_ptr_t
    | _ -> raise (Failure "not yet implemented")

  (* Helper funciton to retrieve a function from context*)
  and get_func s lmodule =
    match L.lookup_function s lmodule with
        None -> raise( Failure (s ^ "not defined in builtin"))
      | Some f -> f

  in

  (* Creates a %name = insertvalue %agg, %val, %idx*)
  let insert_value builder agg i v = L.build_insertvalue agg v i "tmp__" builder in
  let rec generate_seq n = if n >= 0 then (n :: (generate_seq (n-1))) else [] in

  (*
  Builds Built-in functions
  Add each builtin function into Stringmap.
  Function with name length does not gets added.
  *)
  let builtins : (Sast.styp * L.llvalue) StringMap.t =
    List.fold_left (fun m (name, ty) ->
      let ftype = match ty with
          SFunc(f) -> f
        | _ -> (raise (Failure "shouldn't happen"))
      in
      let params = List.map ltype_of_styp ftype.sparam_typs in
      let ltype = L.function_type (ltype_of_styp ftype.sreturn_typ) (Array.of_list params)
      in
      StringMap.add name (ty, (L.declare_function name ltype the_module)) m
    ) StringMap.empty Builtins.builtins in

  (*
  Define each function (arguments and return type) so we can
  call it even before we've created its body

  Like bultins, adds each function into StringMap

  *)
  let function_decls : (L.llvalue * lfexpr) StringMap.t =
    let function_decl m (name, lfexpr) =
      let ftype = ltype_of_lfexpr name lfexpr in
      StringMap.add name (L.define_function name ftype the_module, lfexpr) m in
    List.fold_left function_decl StringMap.empty functions in

  (*
  Fill in the body for a given function

  Unpacking args and env vars,
  skip for main as its env is empty and has no params
  *)
  let build_function_body (name, lfexpr) =
    let (the_function, _) = StringMap.find name function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

  (*
  Construct the function's "locals": formal arguments and locally
  declared variables.  Allocate each on the stack, initialize their
  value, if appropriate, and remember their values in the "locals" map
  *)
  let local_vars =
    let add_param m (t, n) p =
      let () = L.set_value_name n p in
      let local = L.build_malloc (ltype_of_styp t) n builder in
      let _ = L.build_store p local builder in
      StringMap.add n (t, local) m
    in
    let param_list = Array.to_list (L.params the_function) in
    let params =
      if List.length param_list <= 1
      then StringMap.empty
      else List.fold_left2 add_param StringMap.empty lfexpr.lparams (List.tl param_list)
  in

  (* Create the local versions of the free variables. Unpack the environment
   * pointer and initialize the local free variables using values obtained
   * from the pointer *)
  let env =
    if param_list = []
    then L.const_null void_ptr_t
    else List.hd param_list
  in

  let () = L.set_value_name "env" env in
  let env_void_ptr = L.build_malloc void_ptr_t "env" builder in
  let _ = L.build_store env env_void_ptr builder in
  let env_p = L.build_load env_void_ptr "env_p" builder in

  let params_of_lfexpr lfexpr = match lfexpr.lfvs with
      [] -> params
    | _ ->
      let ptr_of_fv (t, _) = L.pointer_type (ltype_of_styp t) in
      let env_struct = L.struct_type context (Array.of_list
        (List.map ptr_of_fv lfexpr.lfvs)) in
      let env_ptr_t = L.pointer_type env_struct in
      let env_ptr = L.build_bitcast env_p env_ptr_t "env_p" builder in
      let env_val = L.build_load env_ptr "env_val" builder in
      let add_free_var m (t, n) idx =
        let free_var = L.build_extractvalue env_val idx "tmp_" builder in
        StringMap.add n (t, free_var) m
      in
      let fvs_count = List.length lfexpr.lfvs in
      List.fold_left2 add_free_var params lfexpr.lfvs
          (List.rev (generate_seq (fvs_count - 1)))
    in

    let params_fvs = match name with
        "main" -> params
      | _ -> params_of_lfexpr lfexpr
    in

    (* Allocate a closure of the function within itself for recursive calls *)
    let clsr_t = ltype_of_clsr name lfexpr in
    let clsr_p = L.build_malloc clsr_t lfexpr.lname builder in
    let clsr_val = List.fold_left2 (insert_value builder)
      (L.const_null clsr_t) [0;1] [the_function;env_p] in
    let _ = L.build_store clsr_val clsr_p builder in
    let func_t  = typ_of_lfexpr lfexpr in
    StringMap.add lfexpr.lname (func_t, clsr_p) params_fvs
  in

  (* Builder *)
  let rec expr builder (m : (styp * L.llvalue) StringMap.t) ((styp, e) : sexpr) =

    let lookup_both n = try StringMap.find n m with
       Not_found -> raise (Failure ("Variable not found: " ^ n)) in
    let lookup n = let (_, llval) = try StringMap.find n m with
       Not_found -> raise (Failure ("Variable not found: " ^ n)) in llval
    in

    (* Helper function for closure*)
    let build_clsr clsr =
      let fvs = List.map snd clsr.free_vars in
      let llfvs = List.map lookup fvs in
      let fvs_t = List.map ltype_of_styp (List.map fst clsr.free_vars) in
      let fvs_ptr_t = List.map L.pointer_type fvs_t in
      let env_struct_t = L.struct_type context (Array.of_list fvs_ptr_t) in
      let env_struct = L.build_malloc env_struct_t "tmp_" builder in
      let idxs = List.rev (generate_seq ((List.length fvs) - 1)) in
      let env_val = List.fold_left2 (insert_value builder)
            (L.const_null env_struct_t) idxs llfvs in
      let _ = L.build_store env_val env_struct builder in
      let env_struct_p = L.build_bitcast env_struct void_ptr_t "env_p" builder in

      (* Pack the function ptr and the env ptr into the closure struct *)
      let func_name = "f" ^ (string_of_int clsr.ind) in
      let (llfunc, sfexpr) = StringMap.find func_name function_decls in
      let llclosure_struct_t = ltype_of_clsr func_name sfexpr in
      let clsr_val = List.fold_left2 (insert_value builder)
      (L.const_null llclosure_struct_t) [0;1] [llfunc; env_struct_p] in
      clsr_val
    in

    match e with
        SIntLit x -> L.const_int i32_t x
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SStrLit s -> L.build_global_stringptr s "str" builder
      | SFloatLit x -> L.const_float_of_string float_t x
      | SId s -> L.build_load (lookup s) s builder
      | SClosure clsr -> build_clsr clsr
      | SNoexpr -> L.const_int i32_t 0
      | SAssign(e1, op, e2) ->
        let new_v = match op with
                  NoOp -> expr builder m e2
                | Add -> expr builder m (styp, SBinop(e1, Add, e2))
                | Sub -> expr builder m (styp, SBinop(e1, Sub, e2))
                | Mul -> expr builder m (styp, SBinop(e1, Mul, e2))
                | Div -> expr builder m (styp, SBinop(e1, Div, e2))
                | _ -> raise (Failure (string_of_op op ^ " not yet implemented"))
        in
        (match (snd e1) with
            SId s -> ignore(L.build_store new_v (lookup s) builder); new_v
         (* NEED LIST ACCESS IMPLEMENTATION *)
         | _ -> raise (Failure ("assignment for " ^ (string_of_sexpr e2)
                ^ "SAssign not implemented in codegen")))
      | SBinop (e1, op, e2) ->
          let (t, _) = e1
          and e1' = expr builder m e1
          and e2' = expr builder m e2 in
          (match snd e1, snd e2 with
             _ -> (match t with
                 SFloat -> (match op with
                     Add     -> L.build_fadd
                   | Sub     -> L.build_fsub
                   | Mul     -> L.build_fmul
                   | Div     -> L.build_fdiv
                   | Equal   -> L.build_fcmp L.Fcmp.Oeq
                   | Neq     -> L.build_fcmp L.Fcmp.One
                   | Less    -> L.build_fcmp L.Fcmp.Olt
                   | Leq     -> L.build_fcmp L.Fcmp.Ole
                   | Greater -> L.build_fcmp L.Fcmp.Ogt
                   | Geq     -> L.build_fcmp L.Fcmp.Oge
                   | _ ->
                     raise (Failure ("internal error: "
                       ^ "semant should have rejected and/or on float"))
                 ) e1' e2' "tmp" builder
               | SInt -> (match op with
                   | Add     -> L.build_add
                   | Sub     -> L.build_sub
                   | Mul     -> L.build_mul
                   | Div     -> L.build_sdiv
                   | And     -> L.build_and
                   | Or      -> L.build_or
                   | Equal   -> L.build_icmp L.Icmp.Eq
                   | Neq     -> L.build_icmp L.Icmp.Ne
                   | Less    -> L.build_icmp L.Icmp.Slt
                   | Leq     -> L.build_icmp L.Icmp.Sle
                   | Greater -> L.build_icmp L.Icmp.Sgt
                   | Geq     -> L.build_icmp L.Icmp.Sge
                 ) e1' e2' "tmp" builder
               | SBool -> (match op with
                     And     -> L.build_and
                   | Or      -> L.build_or
                   | Equal   -> L.build_icmp L.Icmp.Eq
                   | Neq     -> L.build_icmp L.Icmp.Ne
                   | Less    -> L.build_icmp L.Icmp.Slt
                   | Leq     -> L.build_icmp L.Icmp.Sle
                   | Greater -> L.build_icmp L.Icmp.Sgt
                   | Geq     -> L.build_icmp L.Icmp.Sge
                   | _         -> raise (Failure ("operation " ^ (string_of_op op)
                                                  ^ " not implemented for type "
                                                  ^ (string_of_styp t)))
                 ) e1' e2' "tmp" builder
               | SString -> (match op with
                     Add -> L.build_call (snd (StringMap.find "string_concat"
                       builtins)) [| e1'; e2'|] "string_concat" builder
                   | Equal -> (L.build_icmp L.Icmp.Ne) (L.const_int i32_t 0)
                                (L.build_call (snd (StringMap.find
                                  "string_equals" builtins)) [| e1'; e2'|]
                                  "string_equals" builder) "tmp" builder
                   | Neq -> (L.build_icmp L.Icmp.Eq) (L.const_int i32_t 0)
                              (L.build_call (snd (StringMap.find
                                "string_equals" builtins)) [| e1'; e2'|]
                                "string_equals" builder) "tmp" builder
                   | _ -> raise (Failure ("operation " ^ (string_of_op op)
                                          ^ " not implemented for type "
                                          ^ (string_of_styp t))))
               (* NEED STRING IMPLEMENTATION *)
               | _ -> (match op with
                     Equal -> (L.build_icmp L.Icmp.Eq) e1' e2' "tmp" builder
                   | Neq -> (L.build_icmp L.Icmp.Ne) e1' e2' "tmp" builder
                   | _ -> raise (Failure ("operation " ^ (string_of_op op)
                                          ^ " not implemented for type "
                                          ^ (string_of_styp t))))
  ))
    | SUnop(op, e) ->
      let (t, _) = e in
      let e' = expr builder m e in
      (match op with
         Neg when t = SFloat -> L.build_fneg
       | Neg when t = SInt -> L.build_neg
       | Not when t = SBool -> L.build_not
       | _ -> raise (Failure ("operation " ^ (string_of_uop op) ^
                              " not implemented for type "
                              ^ (string_of_styp t)))) e' "tmp" builder
    | SCall((t, SId(name)), args) when StringMap.mem name builtins ->
        (let func_t = match t with
              SFunc(func_t) -> func_t
            | _ -> raise (Failure "This should never happen")  in
         (match func_t.sreturn_typ with
            SVoid -> (let arg_array = Array.of_list
              (List.map (fun arg -> expr builder m arg) args) in
                L.build_call (snd (StringMap.find name builtins))
                arg_array "" builder)
          | _ -> (let arg_array = Array.of_list (List.map
            (fun arg -> expr builder m arg) args) in
              L.build_call (snd (StringMap.find name builtins))
              arg_array "_result" builder)))
    | SCall((t, s), args) ->
        let func_t = match t with
            SFunc(func_t) -> func_t
          | _ -> raise (Failure "wrong type for function call") in
        let clsr_val = expr builder m (t, s) in
        let func_ptr = L.build_extractvalue clsr_val 0 "fp" builder in
        let env_ptr = L.build_extractvalue clsr_val 1 "envp" builder in
        let llargs = env_ptr :: (List.rev (List.map (expr builder m)
          (List.rev args))) in
        let result =
            (match func_t.sreturn_typ with SVoid -> "" | _ -> "_result") in
        L.build_call func_ptr (Array.of_list llargs) result builder
    (* List *)
    | SListLit(contents) ->
      let rec list_fill m lst = (function
          [] -> lst
        | sx :: rest ->
          let (typ, _) = sx in
          let data = (match typ with
            SList _  -> expr builder m sx
          | _ -> let data = L.build_malloc (ltype_of_styp typ) "data" builder in
              let llvalue = expr builder m sx
              in ignore (L.build_store llvalue data builder); data)
          in
          let list_append_f = get_func "list_append" the_module in
          let data = L.build_bitcast data void_ptr_t "data" builder in
                    ignore (L.build_call list_append_f [| lst; data |] "" builder);
                    list_fill m lst rest)
      in
      let list_init_f = get_func "list_init" the_module in
      let lst = L.build_call list_init_f [||] "list_init" builder in
            ignore(list_fill m lst contents); lst
    | SListAccess(arr, i) ->
      let arr_var = expr builder m arr in
      let idx = expr builder m i in
      let list_access_f = get_func "list_get" the_module in
      let data_ptr = L.build_call list_access_f [|arr_var; idx|] "list_get" builder in
      let data_ptr = L.build_bitcast data_ptr (L.pointer_type (ltype_of_styp styp)) "data" builder in
      L.build_load data_ptr "data" builder
    | SListAppend(arr, it) ->
      let arr_var = expr builder m arr in
      let item = expr builder m it in
      let (typ, _) = it in
      let data_ptr = L.build_malloc (ltype_of_styp typ) "data_ptr" builder in
      let unused = L.build_store item data_ptr builder in
      let data = L.build_bitcast data_ptr void_ptr_t "data" builder in

      let list_append_f = get_func "list_append" the_module in
      L.build_call list_append_f [|arr_var; data|] "" builder

    (* Matrix Operations *)
    | SMatrixLit(sm) ->
      (* Function for fold_left for each SFloat, set in matrix *)
      let matrix_fill_row (m,mat,i,j) sx =
          let (typ,_) = sx in
          let data = expr builder m sx in
          let matrix_set_elem_f = get_func "matrix_set_elem" the_module in
          let jl = expr builder m (SInt,SIntLit(j)) in
          let il = expr builder m (SInt,SIntLit(i)) in
          ignore (L.build_call matrix_set_elem_f [| mat; il ; jl ; data |] "" builder);
          (m,mat,i,j+1)
      in
      (* Function for fold_left for each row (SList)  *)
      let matrix_fill (m,mat,i) (typ,sx)=
          match sx with
              SListLit l -> let (m,mat,_,_) = List.fold_left matrix_fill_row (m,mat,i,0) l in (m,mat,i+1)
            | _ -> raise (Failure ("Shoudn't happen"))
      in

      let matrix_init_f = get_func "matrix_init" the_module in
      let row = expr builder m (SInt,SIntLit(sm.srow)) in
      let col = expr builder m (SInt,SIntLit(sm.scol)) in
      let mat = L.build_call matrix_init_f [|row; col|] "matrix_init" builder in
      let (_,mat,_) = List.fold_left matrix_fill (m,mat,0) sm.scontent in
      mat

    | SMatrixGet (mat,i,j) ->
      let il = expr builder m i in
      let jl = expr builder m j in
      let mat = expr builder m mat in
      let matrix_get_elem_f = get_func "matrix_get_elem" the_module in
      L.build_call matrix_get_elem_f [|mat;il;jl|] "" builder;

    | SMatrixSet (mat, i, j, x) ->
      let matl = expr builder m mat in
      let il = expr builder m i in
      let jl = expr builder m j in
      let xl = expr builder m x in
      let matrix_set_elem_f = get_func "matrix_set_elem" the_module in
      L.build_call matrix_set_elem_f [|matl; il; jl; xl|] "" builder

    | SMatrixAdd (mat1, mat2) ->
      let mat1l = expr builder m mat1 in
      let mat2l = expr builder m mat2 in
      let matrix_add_f = get_func "matrix_add" the_module in
      L.build_call matrix_add_f [|mat1l; mat2l|] "" builder

    | SMatrixSub (mat1, mat2) ->
      let mat1l = expr builder m mat1 in
      let mat2l = expr builder m mat2 in
      let matrix_add_f = get_func "matrix_sub" the_module in
      L.build_call matrix_add_f [|mat1l; mat2l|] "" builder

    | SMatrixMulC (mat, x) ->
      let matl = expr builder m mat in
      let xl = expr builder m x in
      let matrix_add_f = get_func "matrix_mul_const" the_module in
      L.build_call matrix_add_f [|matl; xl|] "" builder

    | SMatrixAddC (mat, x) ->
      let matl = expr builder m mat in
      let xl = expr builder m x in
      let matrix_add_f = get_func "matrix_add_const" the_module in
      L.build_call matrix_add_f [|matl; xl|] "" builder

    | SMatrixMulE (m1, m2) ->
      let mat1l = expr builder m m1 in
      let mat2l = expr builder m m2 in
      let matrix_add_f = get_func "matrix_mul_elem" the_module in
      L.build_call matrix_add_f [|mat1l; mat2l|] "" builder

    | SMatrixDivE (m1, m2) ->
      let mat1l = expr builder m m1 in
      let mat2l = expr builder m m2 in
      let matrix_add_f = get_func "matrix_div_elem" the_module in
      L.build_call matrix_add_f [|mat1l; mat2l|] "" builder

    | _ as x -> print_endline(string_of_sexpr (styp, x));
        raise (Failure "expr not implemented in codegen")
  in
  (* Each basic block in a program ends with a "terminator" instruction i.e.
  one that ends the basic block. By definition, these instructions must
  indicate which basic block comes next -- they typically yield "void" value
  and produce control flow, not values *)
  (* Invoke "instr builder" if the current block doesn't already
     have a terminator (e.g., a branch). *)
  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
        None -> ignore (instr builder)
      | Some _ -> ()
  in

  (* Build the code for the given statement; return the builder for
     the statement's successor (i.e., the next instruction will be built
     after the one generated by this call) *)
  (* Imperative nature of statement processing entails imperative OCaml *)
  let rec stmt builder m = function
    (* Throw away the scope generated by block
    * since the block should not be modifying our current scope *)
      SBlock s1 ->
      let helper (bldr, map) = stmt bldr map in
      let (b, _) = List.fold_left helper (builder, m) s1 in
      (b, m)
    | SExpr e -> let _ = expr builder m e in (builder, m)
    (* Right now SVDecl and SFDecl has no difference except se is optional for VDecl*)
    | SDecl(t, n, se) ->
      let se' = expr builder m se in
      let alloc_clsr clsr =
        let func_name = ("f" ^ (string_of_int clsr.ind)) in
        let (_, lfexpr) = StringMap.find func_name function_decls in
        let func_t = L.pointer_type (ltype_of_lfexpr func_name lfexpr) in
        let llclosure_struct_t = L.struct_type context [|func_t; void_ptr_t|] in
        L.build_malloc llclosure_struct_t n builder
      in
      let (_, ex) = se in
      let local_var = match ex with
          SClosure(clsr) -> alloc_clsr clsr
        | _ -> L.build_malloc (ltype_of_styp t) n builder
      in
      let m' = StringMap.add n (t, local_var) m  in
      let _ = L.build_store se' local_var builder in
      (builder, m')
    (* The order that we create and add the basic blocks for an If statement
    doesnt 'really' matter (seemingly). What hooks them up in the right order
    are the build_br functions used at the end of the then and else blocks (if
    they don't already have a terminator) and the build_cond_br function at
    the end, which adds jump instructions to the "then" and "else" basic blocks *)
    | SIf (pred, then_stmts, else_stmts) ->
      let bool_val = expr builder m pred in
      (* Add "merge" basic block to our function's list of blocks *)
      let merge_bb = L.append_block context "merge" the_function in
      (* Partial function used to generate branch to merge block *)
      let branch_instr = L.build_br merge_bb in

      (* Same for "then" basic block *)
      let then_bb = L.append_block context "then" the_function in
      (* Position builder in "then" block and build the statement *)
      let (then_builder, _) = stmt (L.builder_at_end context then_bb) m then_stmts in
      (* Add a branch to the "then" block (to the merge block)
      * if a terminator doesn't already exist for the "then" block
      *)
      let () = add_terminal then_builder branch_instr in

      (* Identical to stuff we did for "then" *)
      let else_bb = L.append_block context "else" the_function in
      let (else_builder, _) = stmt (L.builder_at_end context else_bb) m else_stmts in
      let () = add_terminal else_builder branch_instr in

      (* Generate initial branch instruction perform the selection of "then"
       or "else". Note we're using the builder we had access to at the start
       of this alternative. *)
      let _ = L.build_cond_br bool_val then_bb else_bb builder in
      (L.builder_at_end context merge_bb, m)
    | SReturn e ->
      let _ = match lfexpr.lreturn_typ with
          SVoid -> L.build_ret_void builder
        | _ -> L.build_ret (expr builder m e) builder
      in (builder, m)
    | SWhile (predicate, body) ->
      (* First create basic block for condition instructions -- this will
      serve as destination in the case of a loop *)
      let pred_bb = L.append_block context "while" the_function in
      (* In current block, branch to predicate to execute the condition *)
      let _ = L.build_br pred_bb builder in

      (* Create the body's block, generate the code for it, and add a branch
      back to the predicate block (we always jump back at the end of a while
      loop's body, unless we returned or something) *)
      let body_bb = L.append_block context "while_body" the_function in
      let (while_builder, _) = stmt (L.builder_at_end context body_bb) m body in
      let () = add_terminal while_builder (L.build_br pred_bb) in

      (* Generate the predicate code in the predicate block *)
      let pred_builder = L.builder_at_end context pred_bb in
      let bool_val = expr pred_builder m predicate in

      (* Hook everything up *)
      let merge_bb = L.append_block context "merge" the_function in
      let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
      (L.builder_at_end context merge_bb, m)
    (* Implement for loops as while loops! *)
    | SFor (e1, e2, e3, body) -> stmt builder m
          ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
    | _ -> raise (Failure "stmt not implemented in codegen")

  in

  (* Build the code for each statement in the function *)
  let (builder, _) = stmt builder local_vars (SBlock lfexpr.lbody) in

  (* add a return if the last block falls off the end *)
  add_terminal builder (match lfexpr.lreturn_typ with
        SVoid -> L.build_ret_void
      | SString -> L.build_ret (L.build_global_stringptr "" "str" builder)
      | SFloat -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_styp t) 0))

in
List.iter build_function_body functions;
the_module
