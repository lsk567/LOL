(* Builtin function declarations as SFunc(sfunc_typ) *)

open Sast

module StringMap = Map.Make (String)

let builtins = [

  (* Printing *)
  ("print", SFunc({sparam_typs = [SString]; sreturn_typ = SVoid}));
  ("println", SFunc({ sparam_typs = [SString]; sreturn_typ = SVoid }));
  ("printm", SFunc ({sparam_typs = [SMatrix(0, 0)]; sreturn_typ = SVoid}));

  (* Casting *)
  ("int_of_float", SFunc({ sparam_typs = [SFloat]; sreturn_typ = SInt }));
  ("float_of_int", SFunc({ sparam_typs = [SInt]; sreturn_typ = SFloat }));

  (* String *)
  ("str_of_int", SFunc({ sparam_typs = [SInt]; sreturn_typ = SString }));
  ("int_of_str", SFunc({ sparam_typs = [SString]; sreturn_typ = SInt }));
  ("str_of_bool", SFunc({ sparam_typs = [SBool]; sreturn_typ = SString }));
  ("str_of_float", SFunc({ sparam_typs = [SFloat]; sreturn_typ = SString }));
  ("string_concat", SFunc({ sparam_typs = [SString; SString]; sreturn_typ = SString }));
  ("string_equals", SFunc({ sparam_typs = [SString; SString]; sreturn_typ = SInt }));

  (* List functions *)
  ("list_init", SFunc({sparam_typs = []; sreturn_typ = SList (SAny) }));
  ("list_append", SFunc({sparam_typs = [SList (SAny); SAny]; sreturn_typ = SVoid }));
  ("list_get", SFunc({sparam_typs = [SList (SAny); SInt]; sreturn_typ = SListElement (SAny) }));
  ("list_length", SFunc({sparam_typs= [SList (SAny)]; sreturn_typ = SInt }));
  ("list_set", SFunc({sparam_typs= [SList (SAny); SAny ; SInt]; sreturn_typ = SAny }));

  (* Matrix functions *)
  (* Basic *)
  ("minit", SFunc({sparam_typs = [SInt; SInt]; sreturn_typ = SMatrix(0,0) }));
  ("mget", SFunc({sparam_typs = [SMatrix(0, 0); SInt; SInt]; sreturn_typ = SFloat }));
  ("mset", SFunc({sparam_typs = [SMatrix(0, 0); SInt; SInt; SFloat]; sreturn_typ = SVoid }));
  ("matrix_row", SFunc({sparam_typs = [SMatrix(0,0)]; sreturn_typ = SInt}));
  ("matrix_col", SFunc({sparam_typs = [SMatrix(0,0)]; sreturn_typ = SInt}));
  (* Add, sub, mul, div *)
  ("madd", SFunc({sparam_typs = [SMatrix(0, 0); SMatrix(0, 0)]; sreturn_typ = SInt }));
  ("msub", SFunc({sparam_typs = [SMatrix(0, 0); SMatrix(0, 0)]; sreturn_typ = SInt }));
  ("mmulc", SFunc({sparam_typs = [SMatrix(0, 0); SFloat]; sreturn_typ = SInt }));
  ("maddc", SFunc({sparam_typs = [SMatrix(0, 0); SFloat]; sreturn_typ = SInt }));
  ("mmule", SFunc({sparam_typs = [SMatrix(0, 0); SMatrix(0, 0)]; sreturn_typ = SInt }));
  ("mdive", SFunc({sparam_typs = [SMatrix(0, 0); SMatrix(0, 0)]; sreturn_typ = SInt }));
  (* Swap rows & cols, transpose *)
  ("mswapr", SFunc({sparam_typs = [SMatrix(0, 0); SInt; SInt]; sreturn_typ = SVoid }));
  ("mswapc", SFunc({sparam_typs = [SMatrix(0, 0); SInt; SInt]; sreturn_typ = SVoid }));
  ("mtrans", SFunc({sparam_typs = [SMatrix(0, 0)]; sreturn_typ = SMatrix(0,0) }));
  ("mcopy", SFunc({sparam_typs = [SMatrix(0, 0)]; sreturn_typ = SMatrix(0,0) }));
  (* Matrix view *)
  ("mgetr", SFunc({sparam_typs = [SMatrix(0, 0); SInt]; sreturn_typ = SMatrix(0,0) }));
  ("mgetc", SFunc({sparam_typs = [SMatrix(0, 0); SInt]; sreturn_typ = SMatrix(0,0) }));
  ("mgetsub", SFunc({sparam_typs = [SMatrix(0, 0); SInt; SInt; SInt; SInt]; sreturn_typ = SMatrix(0,0) }));
  (* BLAS *)
  ("mdot", SFunc({sparam_typs = [SMatrix(0, 0); SMatrix(0, 0)]; sreturn_typ = SFloat }));
  
]
