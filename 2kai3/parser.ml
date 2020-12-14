type token =
  | NUM of (int)
  | STR of (string)
  | ID of (string)
  | INT
  | IF
  | WHILE
  | SPRINT
  | IPRINT
  | SCAN
  | EQ
  | NEQ
  | GT
  | LT
  | GE
  | LE
  | ELSE
  | RETURN
  | NEW
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | LB
  | RB
  | LS
  | RS
  | LP
  | RP
  | ASSIGN
  | SEMI
  | COMMA
  | TYPE
  | VOID

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"

open Printf
open Ast
open Lexer
# 44 "parser.ml"
let yytransl_const = [|
  260 (* INT *);
  261 (* IF *);
  262 (* WHILE *);
  263 (* SPRINT *);
  264 (* IPRINT *);
  265 (* SCAN *);
  266 (* EQ *);
  267 (* NEQ *);
  268 (* GT *);
  269 (* LT *);
  270 (* GE *);
  271 (* LE *);
  272 (* ELSE *);
  273 (* RETURN *);
  274 (* NEW *);
  275 (* PLUS *);
  276 (* MINUS *);
  277 (* TIMES *);
  278 (* DIV *);
  279 (* LB *);
  280 (* RB *);
  281 (* LS *);
  282 (* RS *);
  283 (* LP *);
  284 (* RP *);
  285 (* ASSIGN *);
  286 (* SEMI *);
  287 (* COMMA *);
  288 (* TYPE *);
  289 (* VOID *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* STR *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\003\000\004\000\004\000\005\000\005\000\
\005\000\005\000\005\000\006\000\006\000\007\000\007\000\009\000\
\009\000\010\000\010\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\013\000\013\000\014\000\014\000\008\000\008\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\012\000\012\000\012\000\012\000\012\000\012\000\000\000"

let yylen = "\002\000\
\001\000\001\000\004\000\001\000\002\000\000\000\003\000\005\000\
\005\000\006\000\006\000\003\000\001\000\000\000\001\000\004\000\
\002\000\002\000\001\000\004\000\004\000\007\000\007\000\005\000\
\007\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\003\000\003\000\001\000\001\000\
\000\000\001\000\003\000\001\000\004\000\004\000\001\000\001\000\
\004\000\004\000\003\000\003\000\003\000\003\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\000\040\000\063\000\001\000\039\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\047\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\055\000\000\000\038\000\000\000\000\000\
\000\000\000\000\037\000\000\000\000\000\000\000\000\000\000\000\
\019\000\000\000\005\000\000\000\000\000\000\000\000\000\021\000\
\020\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\056\000\000\000\
\000\000\053\000\054\000\000\000\000\000\000\000\000\000\000\000\
\000\000\046\000\045\000\018\000\000\000\036\000\035\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\026\000\
\028\000\027\000\030\000\029\000\032\000\031\000\050\000\049\000\
\034\000\033\000\000\000\000\000\000\000\000\000\007\000\000\000\
\000\000\000\000\003\000\004\000\000\000\000\000\000\000\000\000\
\000\000\012\000\023\000\022\000\025\000\009\000\008\000\017\000\
\000\000\000\000\000\000\011\000\000\000\010\000\016\000"

let yydgoto = "\002\000\
\013\000\014\000\126\000\030\000\059\000\089\000\127\000\015\000\
\128\000\060\000\032\000\037\000\033\000\034\000"

let yysindex = "\002\000\
\205\255\000\000\064\255\245\254\024\255\037\255\061\255\063\255\
\055\255\076\255\000\000\000\000\000\000\000\000\000\000\055\255\
\055\255\055\255\055\255\055\255\058\255\055\255\081\255\000\000\
\034\255\055\255\055\255\009\255\107\255\160\255\134\255\185\255\
\084\255\083\255\033\255\219\255\097\255\099\255\102\255\052\255\
\104\255\055\255\055\255\000\000\152\255\000\000\055\255\055\255\
\055\255\055\255\000\000\105\255\064\255\101\255\133\255\135\255\
\000\000\144\255\000\000\179\255\119\255\005\255\055\255\000\000\
\000\000\055\255\055\255\055\255\055\255\055\255\055\255\205\255\
\205\255\006\255\010\255\012\255\198\255\106\255\000\000\001\255\
\001\255\000\000\000\000\013\255\148\255\123\255\143\255\149\255\
\007\255\000\000\000\000\000\000\055\255\000\000\000\000\185\255\
\185\255\185\255\185\255\185\255\185\255\185\255\142\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\155\255\098\255\098\255\098\255\000\000\172\255\
\057\255\205\255\000\000\000\000\020\255\186\255\163\255\164\255\
\166\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\175\255\098\255\175\255\000\000\196\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\173\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\085\255\000\000\000\000\000\000\000\000\000\000\000\000\239\254\
\000\000\187\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\173\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\197\255\004\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\109\255\
\131\255\000\000\000\000\000\000\000\000\000\000\000\000\087\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\252\254\
\188\255\193\255\199\255\208\255\209\255\084\255\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\214\255\214\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\215\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\228\255\226\255\000\000\000\000\000\000\107\000\200\255\
\000\000\000\000\255\255\206\000\201\000\000\000"

let yytablesize = 287
let yytable = "\058\000\
\024\000\057\000\001\000\002\000\094\000\105\000\002\000\028\000\
\046\000\107\000\044\000\109\000\113\000\044\000\031\000\019\000\
\035\000\036\000\036\000\134\000\040\000\049\000\050\000\043\000\
\044\000\045\000\043\000\047\000\048\000\049\000\050\000\092\000\
\064\000\002\000\095\000\106\000\119\000\120\000\051\000\108\000\
\077\000\110\000\114\000\103\000\104\000\080\000\081\000\082\000\
\083\000\135\000\020\000\047\000\048\000\049\000\050\000\024\000\
\131\000\025\000\042\000\039\000\043\000\096\000\065\000\021\000\
\097\000\098\000\099\000\100\000\101\000\102\000\047\000\048\000\
\049\000\050\000\026\000\047\000\048\000\049\000\050\000\075\000\
\140\000\027\000\142\000\041\000\048\000\125\000\132\000\022\000\
\016\000\023\000\017\000\121\000\018\000\133\000\048\000\048\000\
\048\000\048\000\048\000\048\000\124\000\054\000\029\000\048\000\
\048\000\048\000\048\000\141\000\051\000\052\000\048\000\062\000\
\048\000\063\000\048\000\048\000\013\000\013\000\051\000\051\000\
\051\000\051\000\051\000\051\000\072\000\085\000\073\000\051\000\
\051\000\074\000\052\000\076\000\084\000\112\000\051\000\086\000\
\051\000\087\000\051\000\051\000\052\000\052\000\052\000\052\000\
\052\000\052\000\088\000\093\000\115\000\052\000\052\000\116\000\
\047\000\048\000\049\000\050\000\052\000\122\000\052\000\061\000\
\052\000\052\000\053\000\054\000\004\000\005\000\006\000\007\000\
\008\000\117\000\047\000\048\000\049\000\050\000\130\000\118\000\
\009\000\010\000\090\000\079\000\123\000\003\000\011\000\004\000\
\005\000\006\000\007\000\008\000\136\000\012\000\137\000\055\000\
\056\000\139\000\138\000\009\000\010\000\011\000\143\000\004\000\
\041\000\011\000\091\000\047\000\048\000\049\000\050\000\003\000\
\012\000\004\000\005\000\006\000\007\000\008\000\042\000\057\000\
\047\000\048\000\049\000\050\000\058\000\009\000\010\000\111\000\
\129\000\038\000\059\000\011\000\066\000\067\000\068\000\069\000\
\070\000\071\000\012\000\060\000\061\000\047\000\048\000\049\000\
\050\000\014\000\015\000\078\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\024\000\000\000\000\000\024\000\000\000\024\000\024\000\024\000\
\024\000\024\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\024\000\024\000\000\000\000\000\000\000\000\000\024\000\
\024\000\000\000\000\000\000\000\000\000\000\000\024\000"

let yycheck = "\030\000\
\000\000\030\000\001\000\000\001\000\001\000\001\003\001\009\000\
\000\001\000\001\028\001\000\001\000\001\031\001\016\000\027\001\
\018\000\019\000\020\000\000\001\022\000\021\001\022\001\028\001\
\026\000\027\000\031\001\019\001\020\001\021\001\022\001\060\000\
\000\001\030\001\030\001\030\001\030\001\031\001\030\001\030\001\
\042\000\030\001\030\001\072\000\073\000\047\000\048\000\049\000\
\050\000\030\001\027\001\019\001\020\001\021\001\022\001\001\001\
\000\001\003\001\025\001\002\001\027\001\063\000\030\001\027\001\
\066\000\067\000\068\000\069\000\070\000\071\000\019\001\020\001\
\021\001\022\001\020\001\019\001\020\001\021\001\022\001\028\001\
\137\000\027\001\139\000\003\001\000\001\116\000\030\001\027\001\
\025\001\027\001\027\001\093\000\029\001\122\000\010\001\011\001\
\012\001\013\001\014\001\015\001\003\001\004\001\027\001\019\001\
\020\001\021\001\022\001\138\000\000\001\003\001\026\001\028\001\
\028\001\031\001\030\001\031\001\030\001\031\001\010\001\011\001\
\012\001\013\001\014\001\015\001\028\001\025\001\028\001\019\001\
\020\001\028\001\000\001\028\001\028\001\028\001\026\001\003\001\
\028\001\003\001\030\001\031\001\010\001\011\001\012\001\013\001\
\014\001\015\001\003\001\029\001\001\001\019\001\020\001\029\001\
\019\001\020\001\021\001\022\001\026\001\016\001\028\001\026\001\
\030\001\031\001\003\001\004\001\005\001\006\001\007\001\008\001\
\009\001\027\001\019\001\020\001\021\001\022\001\003\001\027\001\
\017\001\018\001\000\001\028\001\026\001\003\001\023\001\005\001\
\006\001\007\001\008\001\009\001\003\001\030\001\028\001\032\001\
\033\001\028\001\031\001\017\001\018\001\023\001\003\001\003\001\
\028\001\023\001\024\001\019\001\020\001\021\001\022\001\003\001\
\030\001\005\001\006\001\007\001\008\001\009\001\028\001\028\001\
\019\001\020\001\021\001\022\001\028\001\017\001\018\001\026\001\
\118\000\020\000\028\001\023\001\010\001\011\001\012\001\013\001\
\014\001\015\001\030\001\028\001\028\001\019\001\020\001\021\001\
\022\001\028\001\028\001\043\000\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\001\255\255\255\255\003\001\255\255\005\001\006\001\007\001\
\008\001\009\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\017\001\018\001\255\255\255\255\255\255\255\255\023\001\
\024\001\255\255\255\255\255\255\255\255\255\255\030\001"

let yynames_const = "\
  INT\000\
  IF\000\
  WHILE\000\
  SPRINT\000\
  IPRINT\000\
  SCAN\000\
  EQ\000\
  NEQ\000\
  GT\000\
  LT\000\
  GE\000\
  LE\000\
  ELSE\000\
  RETURN\000\
  NEW\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  LB\000\
  RB\000\
  LS\000\
  RS\000\
  LP\000\
  RP\000\
  ASSIGN\000\
  SEMI\000\
  COMMA\000\
  TYPE\000\
  VOID\000\
  "

let yynames_block = "\
  NUM\000\
  STR\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 26 "parser.mly"
             (  _1  )
# 295 "parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 29 "parser.mly"
           ( IntTyp )
# 301 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 30 "parser.mly"
                     ( ArrayTyp (_3, IntTyp) )
# 308 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 31 "parser.mly"
               ( NameTyp _1 )
# 315 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'dec) in
    Obj.repr(
# 34 "parser.mly"
                ( _1@_2 )
# 323 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "parser.mly"
                ( [] )
# 329 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ids) in
    Obj.repr(
# 38 "parser.mly"
                     ( List.map (fun x -> VarDec (_1,x)) _2 )
# 337 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 39 "parser.mly"
                              (
          printf "syntax error\nrow: %d, a lexical of syntax error: %s\n" (!(Lexer.numOfEol)) (!(Lexer.lexical));
          [TypeDec (_2,_4)]
     )
# 348 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 43 "parser.mly"
                               ( [TypeDec (_2,_4)] )
# 356 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'fargs_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 44 "parser.mly"
                                    ( [FuncDec(_2, _4, _1, _6)] )
# 366 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'fargs_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 45 "parser.mly"
                                      ( [FuncDec(_2, _4, VoidTyp, _6)] )
# 375 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ids) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 48 "parser.mly"
                       ( _1@[_3] )
# 383 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 49 "parser.mly"
                       ( [_1]  )
# 390 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
                        ( [] )
# 396 "parser.ml"
               : 'fargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fargs) in
    Obj.repr(
# 53 "parser.mly"
                        ( _1 )
# 403 "parser.ml"
               : 'fargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'fargs) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "parser.mly"
                             ( _1@[(_3,_4)] )
# 412 "parser.ml"
               : 'fargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "parser.mly"
                             ( [(_1,_2)] )
# 420 "parser.ml"
               : 'fargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 60 "parser.mly"
                   ( _1@[_2] )
# 428 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 61 "parser.mly"
                   ( [_1] )
# 435 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                              ( Assign (Var _1, _3) )
# 443 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                               (
          printf "syntax error\nrow: %d, a lexical of syntax error: %s\n" (!(Lexer.numOfEol)) (!(Lexer.lexical));
          Assign (Var _1, _3)
     )
# 454 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 70 "parser.mly"
                                       ( Assign (IndexedVar (Var _1, _3), _6) )
# 463 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                                        ( 
          printf "syntax error\nrow: %d, a lexical of syntax error: %s\n" (!(Lexer.numOfEol)) (!(Lexer.lexical));
          Assign (IndexedVar (Var _1, _3), _6)
     )
# 475 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 75 "parser.mly"
                              ( If (_3, _5, None) )
# 483 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 77 "parser.mly"
                              ( If (_3, _5, Some _7) )
# 492 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 78 "parser.mly"
                              ( While (_3, _5) )
# 500 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 79 "parser.mly"
                              ( CallProc ("sprint", [StrExp _3]) )
# 507 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 80 "parser.mly"
                               (
          printf "syntax error\nrow: %d, a lexical of syntax error: %s\n" (!(Lexer.numOfEol)) (!(Lexer.lexical));
          CallProc ("sprint", [StrExp _3]) 
     )
# 517 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 84 "parser.mly"
                              ( CallProc ("iprint", [_3]) )
# 524 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 85 "parser.mly"
                               (
          printf "syntax error\nrow: %d, a lexical of syntax error: %s\n" (!(Lexer.numOfEol)) (!(Lexer.lexical));
          CallProc ("iprint", [_3])
     )
# 534 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 89 "parser.mly"
                           ( CallProc ("scan", [VarExp (Var _3)]) )
# 541 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 90 "parser.mly"
                            (
          printf "syntax error\nrow: %d, a lexical of syntax error: %s\n" (!(Lexer.numOfEol)) (!(Lexer.lexical)); 
          CallProc ("scan", [VarExp (Var _3)])
     )
# 551 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 94 "parser.mly"
                           ( CallProc ("new", [ VarExp (Var _3)]) )
# 558 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 95 "parser.mly"
                            (
          printf "syntax error\nrow: %d, a lexical of syntax error: %s\n" (!(Lexer.numOfEol)) (!(Lexer.lexical));
          CallProc ("new", [ VarExp (Var _3)])
     )
# 568 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'aargs_opt) in
    Obj.repr(
# 99 "parser.mly"
                                ( CallProc (_1, _3) )
# 576 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'aargs_opt) in
    Obj.repr(
# 100 "parser.mly"
                                 ( 
          printf "syntax error\nrow: %d, a lexical of syntax error: %s\n" (!(Lexer.numOfEol)) (!(Lexer.lexical));
          CallProc (_1, _3)
     )
# 587 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                           ( CallProc ("return", [_2]) )
# 594 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                            (
          printf "syntax error\nrow: %d, a lexical of syntax error: %s\n" (!(Lexer.numOfEol)) (!(Lexer.lexical));
          CallProc ("return", [_2])
     )
# 604 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 109 "parser.mly"
             ( _1 )
# 611 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
            ( NilStmt )
# 617 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
                           ( [] )
# 623 "parser.ml"
               : 'aargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aargs) in
    Obj.repr(
# 114 "parser.mly"
                           ( _1 )
# 630 "parser.ml"
               : 'aargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aargs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
                          ( _1@[_3] )
# 638 "parser.ml"
               : 'aargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                           ( [_1] )
# 645 "parser.ml"
               : 'aargs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'decs) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 121 "parser.mly"
                         ( Block (_2, _3) )
# 653 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'decs) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 122 "parser.mly"
                           ( 
          printf "syntax error\nrow: %d, a lexical of syntax error: %s\n" (!(Lexer.numOfEol)) (!(Lexer.lexical));
          Block (_2, _3)
     )
# 664 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 129 "parser.mly"
           ( IntExp _1  )
# 671 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 130 "parser.mly"
          ( VarExp (Var _1) )
# 678 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'aargs_opt) in
    Obj.repr(
# 131 "parser.mly"
                          ( CallFunc (_1, _3) )
# 686 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                      ( VarExp (IndexedVar (Var _1, _3)) )
# 694 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
                      ( CallFunc ("+", [_1; _3]) )
# 702 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                       ( CallFunc ("-", [_1; _3]) )
# 710 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                       ( CallFunc ("*", [_1; _3]) )
# 718 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 136 "parser.mly"
                     ( CallFunc ("/", [_1; _3]) )
# 726 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 137 "parser.mly"
                               ( CallFunc("!", [_2]) )
# 733 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 138 "parser.mly"
                   ( _2 )
# 740 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "parser.mly"
                     ( CallFunc ("==", [_1; _3]) )
# 748 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 142 "parser.mly"
                     ( CallFunc ("!=", [_1; _3]) )
# 756 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 143 "parser.mly"
                     ( CallFunc (">", [_1; _3]) )
# 764 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 144 "parser.mly"
                     ( CallFunc ("<", [_1; _3]) )
# 772 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 145 "parser.mly"
                     ( CallFunc (">=", [_1; _3]) )
# 780 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 146 "parser.mly"
                     ( CallFunc ("<=", [_1; _3]) )
# 788 "parser.ml"
               : 'cond))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.stmt)
;;
