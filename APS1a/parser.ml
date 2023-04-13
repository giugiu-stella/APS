<<<<<<< HEAD
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
let _ = parse_error;;
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

# 48 "parser.ml"
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
  272 (* IF *);
  273 (* CONST *);
  274 (* FUN *);
  275 (* REC *);
  276 (* DP *);
  277 (* PV *);
  278 (* V *);
  279 (* FLECHE *);
  280 (* ETOILE *);
  281 (* REF *);
  282 (* PROC *);
  283 (* SET *);
  284 (* WHILE *);
  285 (* CALL *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\004\000\005\000\003\000\003\000\003\000\006\000\006\000\006\000\
\006\000\006\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\002\000\002\000\010\000\010\000\008\000\
\008\000\011\000\011\000\011\000\011\000\012\000\012\000\013\000\
\009\000\009\000\014\000\014\000\015\000\015\000\007\000\007\000\
\007\000\007\000\007\000\007\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\002\000\003\000\004\000\
\003\000\003\000\001\000\001\000\001\000\001\000\006\000\005\000\
\005\000\004\000\004\000\001\000\002\000\001\000\004\000\001\000\
\002\000\001\000\001\000\005\000\002\000\001\000\003\000\003\000\
\001\000\003\000\003\000\004\000\001\000\003\000\004\000\007\000\
\008\000\003\000\006\000\007\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\045\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\011\000\012\000\000\000\000\000\014\000\013\000\006\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\027\000\026\000\000\000\
\042\000\000\000\000\000\000\000\000\000\000\000\000\000\007\000\
\009\000\000\000\022\000\010\000\000\000\005\000\004\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\029\000\008\000\039\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\025\000\000\000\000\000\000\000\
\021\000\019\000\032\000\018\000\034\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\
\017\000\000\000\031\000\000\000\000\000\000\000\035\000\000\000\
\038\000\043\000\000\000\023\000\015\000\028\000\040\000\000\000\
\036\000\044\000\041\000"

let yydgoto = "\002\000\
\041\000\068\000\015\000\004\000\005\000\016\000\017\000\060\000\
\043\000\061\000\072\000\073\000\044\000\081\000\082\000"

let yysindex = "\011\000\
\019\255\000\000\078\255\000\000\000\000\107\255\018\255\107\255\
\041\255\004\255\012\255\042\255\107\255\044\255\046\255\033\255\
\035\255\000\000\000\000\068\255\060\255\000\000\000\000\000\000\
\002\255\019\255\002\255\002\255\061\255\062\255\063\255\107\255\
\019\255\118\255\000\000\078\255\078\255\107\255\107\255\107\255\
\107\255\048\255\066\255\052\255\002\255\000\000\000\000\002\255\
\000\000\019\255\107\255\070\255\002\255\015\255\072\255\000\000\
\000\000\037\255\000\000\000\000\118\255\000\000\000\000\107\255\
\107\255\107\255\107\255\083\255\002\255\107\255\060\255\064\255\
\043\255\000\000\000\000\000\000\060\255\084\255\071\255\090\255\
\075\255\092\255\015\255\097\255\000\000\098\255\109\255\107\255\
\000\000\000\000\000\000\000\000\000\000\002\255\002\255\095\255\
\060\255\002\255\091\255\015\255\019\255\110\255\111\255\000\000\
\000\000\120\255\000\000\121\255\107\255\116\255\000\000\002\255\
\000\000\000\000\019\255\000\000\000\000\000\000\000\000\107\255\
\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\124\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\125\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\016\255\000\000\000\000\000\000\
\000\000\000\000\122\255\000\000\000\000\000\000\000\000\104\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\126\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\250\255\066\000\238\255\000\000\231\255\000\000\000\000\073\000\
\188\255\000\000\244\255\041\000\000\000\000\000\177\255"

let yytablesize = 135
let yytable = "\024\000\
\050\000\026\000\093\000\102\000\045\000\028\000\033\000\057\000\
\096\000\046\000\047\000\001\000\049\000\030\000\051\000\052\000\
\079\000\062\000\063\000\025\000\113\000\024\000\029\000\003\000\
\075\000\056\000\048\000\059\000\110\000\080\000\031\000\064\000\
\065\000\066\000\067\000\074\000\024\000\018\000\019\000\020\000\
\078\000\021\000\027\000\032\000\076\000\034\000\022\000\023\000\
\038\000\039\000\084\000\035\000\040\000\036\000\059\000\037\000\
\091\000\086\000\087\000\088\000\067\000\042\000\053\000\092\000\
\055\000\095\000\054\000\069\000\018\000\019\000\020\000\070\000\
\021\000\071\000\077\000\114\000\083\000\022\000\023\000\038\000\
\039\000\106\000\108\000\040\000\006\000\111\000\090\000\094\000\
\097\000\122\000\098\000\099\000\007\000\008\000\009\000\010\000\
\100\000\101\000\103\000\121\000\109\000\104\000\119\000\011\000\
\012\000\013\000\014\000\018\000\019\000\020\000\112\000\021\000\
\105\000\123\000\116\000\115\000\022\000\023\000\018\000\019\000\
\058\000\120\000\021\000\117\000\118\000\020\000\030\000\022\000\
\023\000\003\000\033\000\037\000\089\000\085\000\107\000"

let yycheck = "\006\000\
\026\000\008\000\071\000\083\000\003\001\002\001\013\000\033\000\
\077\000\008\001\009\001\001\000\025\000\002\001\027\000\028\000\
\002\001\036\000\037\000\002\001\100\000\006\001\019\001\005\001\
\050\000\032\000\025\001\034\000\097\000\015\001\019\001\038\000\
\039\000\040\000\041\000\048\000\021\001\001\001\002\001\003\001\
\053\000\005\001\002\001\002\001\051\000\002\001\010\001\011\001\
\012\001\013\001\014\001\006\001\016\001\021\001\061\000\021\001\
\069\000\064\000\065\000\066\000\067\000\002\001\002\001\070\000\
\002\001\023\001\005\001\020\001\001\001\002\001\003\001\006\001\
\005\001\022\001\005\001\101\000\005\001\010\001\011\001\012\001\
\013\001\088\000\095\000\016\001\007\001\098\000\004\001\024\001\
\005\001\115\000\020\001\002\001\015\001\016\001\017\001\018\001\
\022\001\006\001\002\001\112\000\006\001\004\001\109\000\026\001\
\027\001\028\001\029\001\001\001\002\001\003\001\020\001\005\001\
\004\001\120\000\004\001\006\001\010\001\011\001\001\001\002\001\
\003\001\006\001\005\001\004\001\004\001\004\001\023\001\010\001\
\011\001\006\001\006\001\006\001\067\000\061\000\094\000"

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
# 36 "parser.mly"
               (_1)
# 243 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd list) in
    Obj.repr(
# 39 "parser.mly"
                          ( _2 )
# 250 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 43 "parser.mly"
                        ( [ASTStat _1] )
# 257 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 44 "parser.mly"
                  ( ASTDef(_1)::_3)
# 265 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 45 "parser.mly"
                    ( ASTStat(_1)::_3)
# 273 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 49 "parser.mly"
                        ( ASTEcho(_2) )
# 280 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 50 "parser.mly"
                        ( ASTSet(_2,_3))
# 288 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 51 "parser.mly"
                        (ASTIfStat(_2,_3,_4))
# 297 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 52 "parser.mly"
                        ( ASTWhile(_2,_3))
# 305 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 53 "parser.mly"
                      (ASTCall(_2,_3))
# 313 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 57 "parser.mly"
                        ( ASTNum(_1) )
# 320 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "parser.mly"
                        ( ASTId(_1) )
# 327 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
            ( ASTBool(false))
# 333 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
           ( ASTBool(true))
# 339 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 61 "parser.mly"
                               (ASTIf(_3,_4,_5))
# 348 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 62 "parser.mly"
                          (ASTAnd(_3,_4))
# 356 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 63 "parser.mly"
                         (ASTOr(_3,_4))
# 364 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 64 "parser.mly"
                       (ASTFun(_2,_4))
# 372 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 65 "parser.mly"
                        ( ASTApp(_2, _3) )
# 380 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 69 "parser.mly"
             ( [_1] )
# 387 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 70 "parser.mly"
             ( _1::_2 )
# 395 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 74 "parser.mly"
       ( ASTExprpExpr(_1))
# 402 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 75 "parser.mly"
                       (ASTExprpAdr(_3))
# 409 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exprp) in
    Obj.repr(
# 79 "parser.mly"
             ( [_1] )
# 416 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exprp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 80 "parser.mly"
                ( _1::_2 )
# 424 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
      ( ASTTypBool )
# 430 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
      ( ASTTypInt )
# 436 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'typs) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 86 "parser.mly"
                             ( ASTTypFleche(_2,_4) )
# 444 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 87 "parser.mly"
           (ASTref(_2))
# 451 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 90 "parser.mly"
     ( [_1] )
# 458 "parser.ml"
               : 'typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typs) in
    Obj.repr(
# 91 "parser.mly"
                   (_1::_3)
# 466 "parser.ml"
               : 'typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 94 "parser.mly"
              (ASTArg(_1,_3))
# 474 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 97 "parser.mly"
      ([_1])
# 481 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 98 "parser.mly"
               (_1::_3)
# 489 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 101 "parser.mly"
              (ASTArgp(_1,_3))
# 497 "parser.ml"
               : 'argp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 102 "parser.mly"
                    (ASTArgpVar(_2,_4))
# 505 "parser.ml"
               : 'argp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'argp) in
    Obj.repr(
# 105 "parser.mly"
      ([_1])
# 512 "parser.ml"
               : 'argps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'argp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'argps) in
    Obj.repr(
# 106 "parser.mly"
                 (_1::_3)
# 520 "parser.ml"
               : 'argps))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 109 "parser.mly"
                                       ( ASTDefConst(_2, _3, _4) )
# 529 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 110 "parser.mly"
                                          ( ASTDefFun(_2, _3, _5, _7) )
# 539 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 111 "parser.mly"
                                          ( ASTDefFunRec(_3,_4,_6,_8) )
# 549 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 112 "parser.mly"
                          ( ASTDefVar(_2,_3))
# 557 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'argps) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 113 "parser.mly"
                                           ( ASTDefProc(_2,_4,_6))
# 566 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'argps) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 114 "parser.mly"
                                           ( ASTDefProcRec(_3,_5,_7))
# 575 "parser.ml"
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
=======
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

# 54 "parser.ml"
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
  279 (* IF *);
  280 (* CONST *);
  281 (* FUN *);
  282 (* REC *);
  283 (* DP *);
  284 (* PV *);
  285 (* V *);
  286 (* FLECHE *);
  287 (* ETOILE *);
  288 (* REF *);
  289 (* PROC *);
  290 (* SET *);
  291 (* WHILE *);
  292 (* CALL *);
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
\011\000\011\000\011\000\011\000\012\000\012\000\013\000\009\000\
\009\000\014\000\014\000\015\000\015\000\007\000\007\000\007\000\
\007\000\007\000\007\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\002\000\003\000\004\000\
\003\000\003\000\001\000\001\000\001\000\001\000\006\000\005\000\
\005\000\004\000\004\000\004\000\005\000\005\000\005\000\005\000\
\005\000\005\000\001\000\002\000\001\000\004\000\001\000\002\000\
\001\000\001\000\005\000\002\000\001\000\003\000\003\000\001\000\
\003\000\003\000\004\000\001\000\003\000\004\000\007\000\008\000\
\003\000\006\000\007\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\052\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\011\000\012\000\000\000\000\000\014\000\013\000\006\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\034\000\033\000\000\000\049\000\
\000\000\000\000\000\000\000\000\000\000\000\000\007\000\009\000\
\000\000\029\000\010\000\000\000\005\000\004\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\036\000\
\008\000\046\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\032\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\020\000\000\000\028\000\019\000\039\000\
\018\000\041\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\016\000\017\000\021\000\022\000\
\023\000\024\000\025\000\026\000\000\000\038\000\000\000\000\000\
\000\000\042\000\000\000\045\000\050\000\000\000\030\000\015\000\
\035\000\047\000\000\000\043\000\051\000\048\000"

let yydgoto = "\002\000\
\048\000\082\000\015\000\004\000\005\000\016\000\017\000\067\000\
\050\000\068\000\086\000\087\000\051\000\095\000\096\000"

let yysindex = "\005\000\
\005\255\000\000\077\255\000\000\000\000\053\255\009\255\053\255\
\011\255\255\254\001\255\014\255\053\255\042\255\017\255\020\255\
\023\255\000\000\000\000\155\255\047\255\000\000\000\000\000\000\
\021\255\005\255\021\255\021\255\055\255\054\255\058\255\053\255\
\005\255\078\255\000\000\077\255\077\255\053\255\053\255\053\255\
\053\255\053\255\053\255\053\255\053\255\053\255\053\255\053\255\
\034\255\070\255\048\255\021\255\000\000\000\000\021\255\000\000\
\005\255\053\255\080\255\021\255\006\255\081\255\000\000\000\000\
\132\255\000\000\000\000\078\255\000\000\000\000\053\255\053\255\
\053\255\053\255\053\255\053\255\053\255\053\255\083\255\053\255\
\053\255\086\255\021\255\053\255\047\255\051\255\061\255\000\000\
\000\000\000\000\047\255\088\255\067\255\093\255\068\255\090\255\
\006\255\096\255\000\000\095\255\100\255\101\255\103\255\110\255\
\111\255\112\255\113\255\000\000\053\255\000\000\000\000\000\000\
\000\000\000\000\021\255\021\255\114\255\047\255\021\255\091\255\
\006\255\005\255\115\255\118\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\119\255\000\000\120\255\053\255\
\121\255\000\000\021\255\000\000\000\000\005\255\000\000\000\000\
\000\000\000\000\053\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\125\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\126\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\003\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\122\255\000\000\000\000\000\000\000\000\089\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\130\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\250\255\044\000\224\255\000\000\242\255\000\000\000\000\070\000\
\185\255\000\000\246\255\024\000\000\000\000\000\181\255"

let yytablesize = 178
let yytable = "\024\000\
\028\000\026\000\030\000\069\000\070\000\001\000\033\000\093\000\
\031\000\003\000\025\000\057\000\027\000\114\000\056\000\032\000\
\058\000\059\000\064\000\117\000\094\000\123\000\035\000\052\000\
\029\000\063\000\031\000\066\000\053\000\054\000\031\000\071\000\
\072\000\073\000\074\000\075\000\076\000\077\000\078\000\079\000\
\080\000\081\000\089\000\034\000\088\000\140\000\137\000\036\000\
\049\000\092\000\037\000\090\000\055\000\018\000\019\000\020\000\
\060\000\021\000\061\000\062\000\083\000\066\000\022\000\023\000\
\100\000\101\000\102\000\103\000\104\000\105\000\106\000\107\000\
\112\000\109\000\081\000\084\000\085\000\113\000\018\000\019\000\
\065\000\115\000\021\000\006\000\091\000\097\000\108\000\022\000\
\023\000\111\000\116\000\007\000\118\000\119\000\120\000\122\000\
\121\000\124\000\125\000\008\000\009\000\010\000\133\000\126\000\
\127\000\135\000\128\000\141\000\138\000\011\000\012\000\013\000\
\014\000\129\000\130\000\131\000\132\000\139\000\037\000\136\000\
\142\000\143\000\144\000\145\000\110\000\027\000\147\000\149\000\
\148\000\146\000\003\000\040\000\018\000\019\000\020\000\044\000\
\021\000\099\000\134\000\000\000\150\000\022\000\023\000\038\000\
\039\000\098\000\000\000\040\000\041\000\042\000\043\000\044\000\
\045\000\046\000\047\000\018\000\019\000\020\000\000\000\021\000\
\000\000\000\000\000\000\000\000\022\000\023\000\038\000\039\000\
\000\000\000\000\040\000\041\000\042\000\043\000\044\000\045\000\
\046\000\047\000"

let yycheck = "\006\000\
\002\001\008\000\002\001\036\000\037\000\001\000\013\000\002\001\
\006\001\005\001\002\001\026\000\002\001\085\000\025\000\002\001\
\027\000\028\000\033\000\091\000\015\001\097\000\006\001\003\001\
\026\001\032\000\026\001\034\000\008\001\009\001\028\001\038\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\046\000\
\047\000\048\000\057\000\002\001\055\000\121\000\118\000\028\001\
\002\001\060\000\028\001\058\000\032\001\001\001\002\001\003\001\
\002\001\005\001\005\001\002\001\027\001\068\000\010\001\011\001\
\071\000\072\000\073\000\074\000\075\000\076\000\077\000\078\000\
\083\000\080\000\081\000\006\001\029\001\084\000\001\001\002\001\
\003\001\031\001\005\001\007\001\005\001\005\001\004\001\010\001\
\011\001\004\001\030\001\015\001\005\001\027\001\002\001\006\001\
\029\001\002\001\004\001\023\001\024\001\025\001\109\000\004\001\
\004\001\116\000\004\001\122\000\119\000\033\001\034\001\035\001\
\036\001\004\001\004\001\004\001\004\001\027\001\030\001\006\001\
\006\001\004\001\004\001\004\001\081\000\004\001\006\001\142\000\
\139\000\136\000\006\001\006\001\001\001\002\001\003\001\006\001\
\005\001\068\000\115\000\255\255\147\000\010\001\011\001\012\001\
\013\001\014\001\255\255\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\001\001\002\001\003\001\255\255\005\001\
\255\255\255\255\255\255\255\255\010\001\011\001\012\001\013\001\
\255\255\255\255\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001"

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
# 286 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd list) in
    Obj.repr(
# 41 "parser.mly"
                          ( _2 )
# 293 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 45 "parser.mly"
                        ( [ASTStat _1] )
# 300 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 46 "parser.mly"
                  ( ASTDef(_1)::_3)
# 308 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 47 "parser.mly"
                    ( ASTStat(_1)::_3)
# 316 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 51 "parser.mly"
                        ( ASTEcho(_2) )
# 323 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 52 "parser.mly"
                        ( ASTSet(_2,_3))
# 331 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 53 "parser.mly"
                        (ASTIfStat(_2,_3,_4))
# 340 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 54 "parser.mly"
                        ( ASTWhile(_2,_3))
# 348 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 55 "parser.mly"
                      (ASTCall(_2,_3))
# 356 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 59 "parser.mly"
                        ( ASTNum(_1) )
# 363 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 60 "parser.mly"
                        ( ASTId(_1) )
# 370 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
            ( ASTBool(false))
# 376 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
           ( ASTBool(true))
# 382 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 63 "parser.mly"
                               (ASTIf(_3,_4,_5))
# 391 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 64 "parser.mly"
                          (ASTAnd(_3,_4))
# 399 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 65 "parser.mly"
                         (ASTOr(_3,_4))
# 407 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 66 "parser.mly"
                       (ASTFun(_2,_4))
# 415 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 67 "parser.mly"
                        ( ASTApp(_2, _3) )
# 423 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 68 "parser.mly"
                            ( ASTNot(_3) )
# 430 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 69 "parser.mly"
                            ( ASTBinary(Ast.Add, _3, _4) )
# 438 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 70 "parser.mly"
                            ( ASTBinary(Ast.Sub, _3, _4) )
# 446 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 71 "parser.mly"
                            ( ASTBinary(Ast.Mul, _3, _4) )
# 454 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 72 "parser.mly"
                            ( ASTBinary(Ast.Div, _3, _4) )
# 462 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 73 "parser.mly"
                            ( ASTBinary(Ast.Eq, _3, _4) )
# 470 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 74 "parser.mly"
                            ( ASTBinary(Ast.Lt, _3, _4) )
# 478 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 78 "parser.mly"
             ( [_1] )
# 485 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 79 "parser.mly"
             ( _1::_2 )
# 493 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 83 "parser.mly"
       ( ASTExprpExpr(_1))
# 500 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 84 "parser.mly"
                       (ASTExprpAdr(_3))
# 507 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exprp) in
    Obj.repr(
# 88 "parser.mly"
             ( [_1] )
# 514 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exprp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 89 "parser.mly"
                ( _1::_2 )
# 522 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
      ( ASTTypBool )
# 528 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
      ( ASTTypInt )
# 534 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'typs) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 95 "parser.mly"
                             ( ASTTypFleche(_2,_4) )
# 542 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 96 "parser.mly"
           (ASTref(_2))
# 549 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 99 "parser.mly"
     ( [_1] )
# 556 "parser.ml"
               : 'typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typs) in
    Obj.repr(
# 100 "parser.mly"
                   (_1::_3)
# 564 "parser.ml"
               : 'typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 103 "parser.mly"
              (ASTArg(_1,_3))
# 572 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 106 "parser.mly"
      ([_1])
# 579 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 107 "parser.mly"
               (_1::_3)
# 587 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 110 "parser.mly"
              (ASTArgp(_1,_3))
# 595 "parser.ml"
               : 'argp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 111 "parser.mly"
                    (ASTArgpVar(_2,_4))
# 603 "parser.ml"
               : 'argp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'argp) in
    Obj.repr(
# 114 "parser.mly"
      ([_1])
# 610 "parser.ml"
               : 'argps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'argp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'argps) in
    Obj.repr(
# 115 "parser.mly"
                 (_1::_3)
# 618 "parser.ml"
               : 'argps))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 118 "parser.mly"
                                       ( ASTDefConst(_2, _3, _4) )
# 627 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 119 "parser.mly"
                                          ( ASTDefFun(_2, _3, _5, _7) )
# 637 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 120 "parser.mly"
                                          ( ASTDefFunRec(_3,_4,_6,_8) )
# 647 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 121 "parser.mly"
                          ( ASTDefVar(_2,_3))
# 655 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'argps) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 122 "parser.mly"
                                           ( ASTDefProc(_2,_4,_6))
# 664 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'argps) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 123 "parser.mly"
                                           ( ASTDefProcRec(_3,_5,_7))
# 673 "parser.ml"
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
>>>>>>> 3d36c87 (correction eval APS1a)
