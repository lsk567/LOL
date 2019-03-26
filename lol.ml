open Ast

module StringMap = Map.Make(String)

let rec eval env = function
    Lit(x)  -> (x,env)
  | Var(x) -> (StringMap.find x env, env)
  | Seq(e1,e2)-> let (_,env) = eval env e1 in
                  eval env e2
  | Asn(v,e) -> let (vv,env) = eval env e in (vv, StringMap.add v vv env)
  | Binop(e1, op, e2) ->
      let (v1,env) = eval env e1 in
      let (v2,env) = eval env e2 in
      ((match op with
          Add -> v1 + v2
        | Sub -> v1 - v2
        | Mul -> v1 * v2
        | Div -> v1 / v2), env)

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.token lexbuf in
  let (result,_) = eval StringMap.empty expr in
  print_endline (string_of_int result)
