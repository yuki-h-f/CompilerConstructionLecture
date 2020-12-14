(* File lexer.mll *)
{
 open Parser
 open Printf
 let numOfEol = ref 1
 let lexical = ref ""
 exception No_such_symbol
}
let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule lexer = parse
| digit+ as num  { NUM (int_of_string num) }
| "if"                    { lexical := "if"; IF }
| "else"                  { ELSE }
| "while"                 { WHILE }
| "scan"                  { SCAN }
| "sprint"                { SPRINT }
| "iprint"                { IPRINT }
| "int"                   { INT }
| "return"                { RETURN }
| "type"                  { TYPE }
| "void"                  { VOID }
| id as text              { ID text }
| '\"'[^'\"']*'\"' as str { STR str }
| '='                     { ASSIGN }
| "=="                    { EQ }
| "!="                    { NEQ }
| '>'                     { GT }
| '<'                     { LT }
| ">="                    { GE }
| "<="                    { LE }
| '+'                     { PLUS }
| '-'                     { MINUS }
| '*'                     { TIMES }
| '/'                     { DIV }
| '{'                     { LB  }
| '}'                     { RB  }
| '['                     { LS }
| ']'                     { RS }
| '('                     { LP  }
| ')'                     { RP  }
| ','                     { COMMA }
| ';'                     { SEMI }
| '\n'                    { numOfEol := !numOfEol + 1; lexer lexbuf }
| [' ' '\t' ]       { lexer lexbuf }(* eat up whitespace *)
| "//"                    { comment lexbuf }
| eof                     { raise End_of_file }
| _                       { raise No_such_symbol }
and comment = parse
| '\n'  { lexer lexbuf }
| eof   { raise End_of_file }
| _     { comment lexbuf }
