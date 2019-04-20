(* Builtin function declarations as SFunc(sfunc_typ) *)

open Sast

module StringMap = Map.Make (String)

let builtins = [
  ("print", SFunc({sparam_typs= [SString]; sreturn_typ = SVoid}));
  ("println", SFunc({ sparam_typs = [SString]; sreturn_typ = SVoid }));
  ("print", SFunc({ sparam_typs = [SString]; sreturn_typ = SVoid }));
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
  ("list_init", SFunc({sparam_typs= []; sreturn_typ = SList (SAny) }));
  ("list_append", SFunc({sparam_typs= [SList (SAny); SAny]; sreturn_typ = SVoid }));
  ("list_get", SFunc({sparam_typs= [SList (SAny); SInt]; sreturn_typ = SListElement (SAny) }));

]
