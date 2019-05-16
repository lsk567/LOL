(* Ocamllex scanner for LOL *)

{open Parser}

let digit = ['0' - '9']
let digits = digit+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
(* Comments *)
| "//"                 { linec lexbuf } (* Single-line  Comments *)
| "/*"                 { comment 0 lexbuf} (* Multi-line  Comments *)
(* Member func *)
| ".append"            { DAPPEND }
| ".length"            { DLENGTH }
| ".col"               { DCOL }
| ".row"               { DROW }
| ".set"               { DSET }
| ".get"               { DGET }
| ".add"               { DADD }
| ".sub"               { DSUB }
| ".mulc"              { DMULC }
| ".addc"              { DADDC }
| ".mule"              { DMULE }
| ".dive"              { DDIVE }

(* Symbols *)
| '('                  { LPAREN }
| ')'                  { RPAREN }
| '{'                  { LBRACE }
| '}'                  { RBRACE }
| '['                  { LSQBRACE }
| ']'                  { RSQBRACE }
| ':'                  { COLON }
| ';'                  { SEMI }
| ','                  { COMMA }
| '.'                  { DOT }
| '"'                  { QUOTE }
| '='                  { ASSIGN }
(* Math *)
| '+'                  { PLUS }
| '-'                  { MINUS }
| '*'                  { TIMES }
| '^'                  { POW }
| '/'                  { DIVIDE }
| '%'                  { MOD }
| '@'                  { MMUL }
| "+="                 { PLUSASN }
| "-="                 { MINUSASN }
| "*="                 { TIMESASN }
| "/="                 { DIVIDEASN }
| "%="                 { MODASN }
| "++"                 { INC }
| "--"                 { DEC }
(* Bool Op *)
| "=="                 { EQ }
| "!="                 { NEQ }
| '<'                  { LT }
| "<="                 { LEQ }
| ">"                  { GT }
| ">="                 { GEQ }
| "!"                  { NOT }
(* Keywords *)
| "if"                 { IF }
| "elif"               { ELIF }
| "else"               { ELSE }
| "for"                { FOR }
| "in"                 { IN }
| "while"              { WHILE }
| "func"               { FUNC }
| "return"             { RETURN }
| "int"                { INT }
| "bool"               { BOOL }
| "float"              { FLOAT }
| "string"             { STRING }
| "void"               { VOID }
| "AND"                { AND }
| "OR"                 { OR }
| "List"               { LIST }
| "Matrix"             { MATRIX }
| "Tensor"             { TENSOR }
| "true"|"false" as lxm           { BOOLLIT(bool_of_string lxm)  }
| digits as lxm                   { INTLIT(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLOATLIT(lxm) }
| '"' ([^'"']* as lxm) '"'        { STRLIT(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm     { ID(lxm) }
| eof                             { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment level =
  parse "*/" { if level == 0 then token lexbuf else comment (level - 1) lexbuf}
      | "/*" { comment (level + 1) lexbuf }
      | _ { comment level lexbuf }

and linec =
  parse '\n' { token lexbuf }
      | eof { token lexbuf }
      | _ { linec lexbuf }
