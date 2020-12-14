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
| digit+ as num  { lexical := num; NUM (int_of_string num) }
| "if"                    { lexical := "if"; IF }
| "else"                  { lexical := "else"; ELSE }
| "while"                 { lexical := "while"; WHILE }
| "scan"                  { lexical := "scan"; SCAN }
| "sprint"                { lexical := "sprint"; SPRINT }
| "iprint"                { lexical := "iprint"; IPRINT }
| "int"                   { lexical := "int"; INT }
| "return"                { lexical := "return"; RETURN }
| "type"                  { lexical := "type"; TYPE }
| "void"                  { lexical := "void"; VOID }
| id as text              { lexical := text; ID text }
| '\"'[^'\"']*'\"' as str { lexical := str; STR str }
| '='                     { lexical := "="; ASSIGN }
| "=="                    { lexical := "=="; EQ }
| "!="                    { lexical := "!="; NEQ }
| '>'                     { lexical := ">"; GT }
| '<'                     { lexical := "<"; LT }
| ">="                    { lexical := ">="; GE }
| "<="                    { lexical := "<="; LE }
| '+'                     { lexical := "+"; PLUS }
| '-'                     { lexical := "-"; MINUS }
| '*'                     { lexical := "*"; TIMES }
| '/'                     { lexical := "/"; DIV }
| '{'                     { lexical := "{"; LB  }
| '}'                     { lexical := "}"; RB  }
| '['                     { lexical := "["; LS }
| ']'                     { lexical := "]"; RS }
| '('                     { lexical := "("; LP  }
| ')'                     { lexical := ")"; RP  }
| ','                     { lexical := ","; COMMA } 
| ';'                     { lexical := ";"; SEMI }
| '\n'                    { numOfEol := !numOfEol + 1; lexical := "eol"; lexer lexbuf }
| [' ' '\t' ]       { lexer lexbuf }(* eat up whitespace *)
| "//"                    { comment lexbuf }
| eof                     { lexical := "eof"; raise End_of_file }
| _                       { lexical := "No_such_symbol"; raise No_such_symbol }
and comment = parse
| '\n'  { lexical := "end of comment"; lexer lexbuf }
| eof   { lexical := "eof"; raise End_of_file }
| _     { comment lexbuf }
