type token =
  | NUM of (int)
  | IDENT of (string)
  | LPAR
  | RPAR
  | LBRA
  | RBRA
  | ECHO
  | INT
  | BOOL
  | TRUE
  | FALSE
  | AND
  | OR
  | ADR
  | VAR
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | EQ
  | LT
  | NOT
  | VOID
  | IF
  | CONST
  | FUN
  | REC
  | DP
  | PV
  | V
  | FLECHE
  | ETOILE
  | REF
  | PROC
  | SET
  | WHILE
  | CALL

open Parsing;;
# 2 "parser.mly"
(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017                          == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: parser.mly                                                  == *)
(* == Analyse syntaxique                                                   == *)
(* ========================================================================== *)

open Ast

# 55 "parser.ml"
let yytransl_const = [|
  259 (* LPAR *);
  260 (* RPAR *);
  261 (* LBRA *);
  262 (* RBRA *);
  263 (* ECHO *);
  264 (* INT *);
  265 (* BOOL *);
  266 (* TRUE *);
  267 (* FALSE *);
  268 (* AND *);
  269 (* OR *);
  270 (* ADR *);
  271 (* VAR *);
  272 (* PLUS *);
  273 (* MINUS *);
  274 (* TIMES *);
  275 (* DIV *);
  276 (* EQ *);
  277 (* LT *);
  278 (* NOT *);
  279 (* VOID *);
  280 (* IF *);
  281 (* CONST *);
  282 (* FUN *);
  283 (* REC *);
  284 (* DP *);
  285 (* PV *);
  286 (* V *);
  287 (* FLECHE *);
  288 (* ETOILE *);
  289 (* REF *);
  290 (* PROC *);
  291 (* SET *);
  292 (* WHILE *);
  293 (* CALL *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\004\000\005\000\003\000\003\000\003\000\006\000\006\000\006\000\
\006\000\006\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\002\000\002\000\010\000\010\000\008\000\008\000\
\011\000\011\000\011\000\011\000\011\000\012\000\012\000\013\000\
\009\000\009\000\014\000\014\000\015\000\015\000\007\000\007\000\
\007\000\007\000\007\000\007\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\002\000\003\000\004\000\
\003\000\003\000\001\000\001\000\001\000\001\000\006\000\004\000\
\004\000\004\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\001\000\002\000\001\000\004\000\001\000\002\000\
\001\000\001\000\001\000\005\000\002\000\001\000\003\000\003\000\
\001\000\003\000\003\000\004\000\001\000\003\000\004\000\007\000\
\008\000\003\000\006\000\007\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\053\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\011\000\012\000\000\000\000\000\014\000\013\000\006\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\034\000\033\000\035\000\000\000\
\050\000\000\000\000\000\000\000\000\000\000\000\000\000\007\000\
\009\000\000\000\029\000\010\000\000\000\005\000\004\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\037\000\008\000\047\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\032\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\018\000\000\000\028\000\017\000\
\040\000\016\000\042\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\025\000\026\000\019\000\
\020\000\021\000\022\000\023\000\024\000\000\000\039\000\000\000\
\000\000\000\000\043\000\000\000\046\000\051\000\000\000\030\000\
\015\000\036\000\048\000\000\000\044\000\052\000\049\000"

let yydgoto = "\002\000\
\048\000\083\000\015\000\004\000\005\000\016\000\017\000\068\000\
\050\000\069\000\087\000\088\000\051\000\096\000\097\000"

let yysindex = "\008\000\
\005\255\000\000\075\255\000\000\000\000\084\255\010\255\084\255\
\013\255\002\255\020\255\014\255\084\255\084\255\011\255\247\254\
\015\255\000\000\000\000\160\255\046\255\000\000\000\000\000\000\
\022\255\005\255\022\255\022\255\047\255\049\255\048\255\084\255\
\005\255\116\255\000\000\075\255\075\255\084\255\084\255\084\255\
\084\255\084\255\084\255\084\255\084\255\084\255\084\255\084\255\
\028\255\052\255\029\255\022\255\000\000\000\000\000\000\022\255\
\000\000\005\255\084\255\055\255\022\255\255\254\056\255\000\000\
\000\000\136\255\000\000\000\000\116\255\000\000\000\000\084\255\
\084\255\084\255\084\255\084\255\084\255\084\255\084\255\058\255\
\084\255\084\255\060\255\022\255\084\255\046\255\033\255\050\255\
\000\000\000\000\000\000\046\255\072\255\063\255\076\255\053\255\
\078\255\255\254\084\255\000\000\088\255\092\255\093\255\094\255\
\098\255\099\255\101\255\102\255\000\000\084\255\000\000\000\000\
\000\000\000\000\000\000\022\255\022\255\082\255\046\255\022\255\
\079\255\255\254\005\255\108\255\111\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\118\255\000\000\119\255\
\084\255\114\255\000\000\022\255\000\000\000\000\005\255\000\000\
\000\000\000\000\000\000\084\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\123\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\124\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\045\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\120\255\000\000\000\000\000\000\000\000\103\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\126\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\250\255\043\000\225\255\000\000\241\255\000\000\000\000\064\000\
\183\255\000\000\252\255\019\000\000\000\000\000\161\255"

let yytablesize = 184
let yytable = "\024\000\
\094\000\026\000\124\000\028\000\070\000\071\000\033\000\034\000\
\001\000\003\000\058\000\025\000\115\000\095\000\027\000\032\000\
\035\000\065\000\118\000\036\000\057\000\030\000\059\000\060\000\
\052\000\064\000\141\000\067\000\029\000\053\000\054\000\072\000\
\073\000\074\000\075\000\076\000\077\000\078\000\079\000\080\000\
\081\000\082\000\090\000\037\000\055\000\138\000\031\000\049\000\
\061\000\063\000\031\000\089\000\091\000\062\000\056\000\084\000\
\093\000\085\000\086\000\092\000\098\000\109\000\067\000\112\000\
\116\000\101\000\102\000\103\000\104\000\105\000\106\000\107\000\
\108\000\031\000\110\000\082\000\119\000\121\000\114\000\113\000\
\117\000\006\000\122\000\123\000\018\000\019\000\020\000\137\000\
\021\000\007\000\120\000\126\000\125\000\022\000\023\000\127\000\
\128\000\129\000\008\000\009\000\010\000\130\000\131\000\134\000\
\132\000\133\000\140\000\142\000\011\000\012\000\013\000\014\000\
\136\000\143\000\144\000\139\000\018\000\019\000\066\000\148\000\
\021\000\145\000\146\000\027\000\111\000\022\000\023\000\150\000\
\003\000\041\000\147\000\045\000\100\000\038\000\135\000\149\000\
\018\000\019\000\020\000\000\000\021\000\151\000\000\000\000\000\
\000\000\022\000\023\000\038\000\039\000\099\000\000\000\040\000\
\041\000\042\000\043\000\044\000\045\000\046\000\000\000\047\000\
\018\000\019\000\020\000\000\000\021\000\000\000\000\000\000\000\
\000\000\022\000\023\000\038\000\039\000\000\000\000\000\040\000\
\041\000\042\000\043\000\044\000\045\000\046\000\000\000\047\000"

let yycheck = "\006\000\
\002\001\008\000\098\000\002\001\036\000\037\000\013\000\014\000\
\001\000\005\001\026\000\002\001\086\000\015\001\002\001\002\001\
\006\001\033\000\092\000\029\001\025\000\002\001\027\000\028\000\
\003\001\032\000\122\000\034\000\027\001\008\001\009\001\038\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\046\000\
\047\000\048\000\058\000\029\001\023\001\119\000\027\001\002\001\
\002\001\002\001\006\001\056\000\059\000\005\001\033\001\028\001\
\061\000\006\001\030\001\005\001\005\001\004\001\069\000\004\001\
\032\001\072\000\073\000\074\000\075\000\076\000\077\000\078\000\
\079\000\029\001\081\000\082\000\005\001\002\001\085\000\084\000\
\031\001\007\001\030\001\006\001\001\001\002\001\003\001\006\001\
\005\001\015\001\028\001\004\001\099\000\010\001\011\001\004\001\
\004\001\004\001\024\001\025\001\026\001\004\001\004\001\110\000\
\004\001\004\001\028\001\123\000\034\001\035\001\036\001\037\001\
\117\000\006\001\004\001\120\000\001\001\002\001\003\001\006\001\
\005\001\004\001\004\001\004\001\082\000\010\001\011\001\143\000\
\006\001\006\001\137\000\006\001\069\000\031\001\116\000\140\000\
\001\001\002\001\003\001\255\255\005\001\148\000\255\255\255\255\
\255\255\010\001\011\001\012\001\013\001\014\001\255\255\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\255\255\024\001\
\001\001\002\001\003\001\255\255\005\001\255\255\255\255\255\255\
\255\255\010\001\011\001\012\001\013\001\255\255\255\255\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\255\255\024\001"

let yynames_const = "\
  LPAR\000\
  RPAR\000\
  LBRA\000\
  RBRA\000\
  ECHO\000\
  INT\000\
  BOOL\000\
  TRUE\000\
  FALSE\000\
  AND\000\
  OR\000\
  ADR\000\
  VAR\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  EQ\000\
  LT\000\
  NOT\000\
  VOID\000\
  IF\000\
  CONST\000\
  FUN\000\
  REC\000\
  DP\000\
  PV\000\
  V\000\
  FLECHE\000\
  ETOILE\000\
  REF\000\
  PROC\000\
  SET\000\
  WHILE\000\
  CALL\000\
  "

let yynames_block = "\
  NUM\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 38 "parser.mly"
               (_1)
# 289 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd list) in
    Obj.repr(
# 41 "parser.mly"
                          ( _2 )
# 296 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 45 "parser.mly"
                        ( [ASTStat _1] )
# 303 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 46 "parser.mly"
                  ( ASTDef(_1)::_3)
# 311 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 47 "parser.mly"
                    ( ASTStat(_1)::_3)
# 319 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 51 "parser.mly"
                        ( ASTEcho(_2) )
# 326 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 52 "parser.mly"
                        ( ASTSet(_2,_3))
# 334 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 53 "parser.mly"
                        (ASTIfStat(_2,_3,_4))
# 343 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 54 "parser.mly"
                        ( ASTWhile(_2,_3))
# 351 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 55 "parser.mly"
                     (ASTCall(_2,_3))
# 359 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 59 "parser.mly"
                        ( ASTNum(_1) )
# 366 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 60 "parser.mly"
                        ( ASTId(_1) )
# 373 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
            ( ASTBool(false))
# 379 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
           ( ASTBool(true))
# 385 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 63 "parser.mly"
                               (ASTIf(_3,_4,_5))
# 394 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 64 "parser.mly"
                       (ASTFun(_2,_4))
# 402 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 65 "parser.mly"
                        ( ASTApp(_2, _3) )
# 410 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 66 "parser.mly"
                            ( ASTNot(_3) )
# 417 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 67 "parser.mly"
                            ( ASTBinary(Ast.Add, _3, _4) )
# 425 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 68 "parser.mly"
                            ( ASTBinary(Ast.Sub, _3, _4) )
# 433 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 69 "parser.mly"
                            ( ASTBinary(Ast.Mul, _3, _4) )
# 441 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 70 "parser.mly"
                            ( ASTBinary(Ast.Div, _3, _4) )
# 449 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 71 "parser.mly"
                            ( ASTBinary(Ast.Eq, _3, _4) )
# 457 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 72 "parser.mly"
                            ( ASTBinary(Ast.Lt, _3, _4) )
# 465 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 73 "parser.mly"
                          ( ASTBinary(Ast.And,_3,_4))
# 473 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 74 "parser.mly"
                         ( ASTBinary(Ast.Or,_3,_4))
# 481 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 78 "parser.mly"
             ( [_1] )
# 488 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 79 "parser.mly"
             ( _1::_2 )
# 496 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 83 "parser.mly"
       ( ASTExprpExpr(_1))
# 503 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 84 "parser.mly"
                      (ASTExprpAdr(_3))
# 510 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exprp) in
    Obj.repr(
# 88 "parser.mly"
             ( [_1] )
# 517 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exprp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 89 "parser.mly"
                ( _1::_2 )
# 525 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
      ( ASTTypBool )
# 531 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
      ( ASTTypInt )
# 537 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
       (ASTTypVoid)
# 543 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'typs) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 96 "parser.mly"
                             ( ASTTypFleche(_2,_4) )
# 551 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 97 "parser.mly"
           (ASTref(_2))
# 558 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 100 "parser.mly"
     ( [_1] )
# 565 "parser.ml"
               : 'typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typs) in
    Obj.repr(
# 101 "parser.mly"
                   (_1::_3)
# 573 "parser.ml"
               : 'typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 104 "parser.mly"
              (ASTArg(_1,_3))
# 581 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 107 "parser.mly"
      ([_1])
# 588 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 108 "parser.mly"
               (_1::_3)
# 596 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 111 "parser.mly"
              (ASTArgp(_1,_3))
# 604 "parser.ml"
               : 'argp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 112 "parser.mly"
                    (ASTArgpVar(_2,_4))
# 612 "parser.ml"
               : 'argp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'argp) in
    Obj.repr(
# 115 "parser.mly"
      ([_1])
# 619 "parser.ml"
               : 'argps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'argp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'argps) in
    Obj.repr(
# 116 "parser.mly"
                 (_1::_3)
# 627 "parser.ml"
               : 'argps))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 119 "parser.mly"
                                       ( ASTDefConst(_2, _3, _4) )
# 636 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 120 "parser.mly"
                                          ( ASTDefFun(_2, _3, _5, _7) )
# 646 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 121 "parser.mly"
                                          ( ASTDefFunRec(_3,_4,_6,_8) )
# 656 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 122 "parser.mly"
                          ( ASTDefVar(_2,_3))
# 664 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'argps) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 123 "parser.mly"
                                           ( ASTDefProc(_2,_4,_6))
# 673 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'argps) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 124 "parser.mly"
                                           ( ASTDefProcRec(_3,_5,_7))
# 682 "parser.ml"
               : 'def))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.cmd list)
