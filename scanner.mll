(* Ocamllex scanner for MicroC *)

{open Parser}

rule token =
  parse [' ' '\t' '\r' '\n'] { token lexbuf }
      | "//"                 { linec lexbuf } (* Single-line  Comments *)
      | "/*"                 { comment 0 lexbuf} (* Multi-line  Comments *)
      | '('                  { LPAREN }
      | ')'                  { RPAREN }
      | '{'                  { LBRACE }
      | '}'                  { RBRACE }
      | ';'                  { SEMI }
      | ','                  { COMMA }
      | '+'                  { PLUS }
      | '-'                  { MINUS }
      | '*'                  { TIMES }
      | '/'                  { DIVIDE }
      | '='                  { ASSIGN }
      | "=="                 { EQ }
      | "!="                 { NEQ }
      | '<'                  { LT }
      | "<="                 { LEQ }
      | ">"                  { GT }
      | ">="                 { GEQ }
      | "&&"                 { AND }
      | "||"                 { OR }
      | "!"                  { NOT }
      | "func"               { FUNC }
      | "if"                 { IF }
      | "elif"               { ELIF }
      | "else"               { ELSE }
      | "for"                { FOR }
      | "while"              { WHILE }
      | "return"             { RETURN }
      | "int"                { INT }
      | "bool"               { BOOL }
      | "float"              { FLOAT }
      | "void"               { VOID }
      | "true"|"false" as lxm           { BOOLLIT(bool_of_string lxm)  }
      | ['0'-'9']+ as lxm               { INTLIT(int_of_string lxm) }
      | ['0'-'9']*"."['0'-'9']+ as lxm  { FLOATLIT(lxm) }
      | '"'                             { str (Buffer.create 16) lexbuf }
      | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm     { ID(lxm) }
      | eof                  { EOF }
      | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment level =
  parse "*/" { if level == 0 then token lexbuf else comment (level - 1) lexbuf}
      | "/*" { comment (level + 1) lexbuf }
      | _ { comment level lexbuf }

and linec =
  parse '\n' { token lexbuf }
      | eof { token lexbuf }
      | _ { linec lexbuf }

and str buf = parse
  '"' { STRLIT(Buffer.contents buf) }
| [^ '"'] { Buffer.add_string buf (Lexing.lexeme lexbuf); str buf lexbuf }
