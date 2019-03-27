{open Parser}

rule token =
  parse [' ' '\t' '\r' '\n'] { token lexbuf }
      | "//"                 { linec lexbuf } (* Single-line  Comments *)
      | "/*"                 { comment 0 lexbuf} (* Multi-line  Comments *)
      | '+'                  { PLUS }
      | '-'                  { MINUS }
      | '*'                  { TIMES }
      | '/'                  { DIVIDE }
      | '='                  { ASSIGN }
      | ';'                  { SEMI }
      | ['0'-'9']+ as lit    { LITERAL(int_of_string lit) }
      | ['a'-'z']+ as id     { VARIABLE(id) }
      | eof                  { EOF }

and comment level = 
  parse "*/" { if level == 0 then token lexbuf else comment (level - 1) lexbuf}
      | "/*" { comment (level + 1) lexbuf }
      | _ { comment level lexbuf }

and linec = 
  parse '\n' { token lexbuf }
      | eof { token lexbuf }
      | _ { linec lexbuf }
