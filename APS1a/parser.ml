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

# 46 "parser.ml"
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
  281 (* PROC *);
  282 (* SET *);
  283 (* WHILE *);
  284 (* CALL *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\004\000\005\000\003\000\003\000\003\000\006\000\006\000\006\000\
\006\000\006\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\002\000\002\000\010\000\010\000\008\000\
\008\000\011\000\011\000\011\000\012\000\012\000\013\000\009\000\
\009\000\014\000\014\000\015\000\015\000\007\000\007\000\007\000\
\007\000\007\000\007\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\002\000\003\000\004\000\
\003\000\003\000\001\000\001\000\001\000\001\000\006\000\005\000\
\005\000\004\000\004\000\001\000\002\000\001\000\004\000\001\000\
\002\000\001\000\001\000\005\000\001\000\003\000\003\000\001\000\
\003\000\003\000\004\000\001\000\003\000\004\000\007\000\008\000\
\003\000\006\000\007\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\044\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\011\000\012\000\000\000\000\000\014\000\013\000\006\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\027\000\026\000\041\000\
\000\000\000\000\000\000\000\000\000\000\000\000\007\000\009\000\
\000\000\022\000\010\000\000\000\005\000\004\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\008\000\038\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\025\000\000\000\000\000\000\000\021\000\019\000\
\031\000\018\000\033\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\016\000\017\000\000\000\
\030\000\000\000\000\000\000\000\034\000\000\000\037\000\042\000\
\000\000\023\000\015\000\028\000\039\000\000\000\035\000\043\000\
\040\000"

let yydgoto = "\002\000\
\041\000\067\000\015\000\004\000\005\000\016\000\017\000\059\000\
\043\000\060\000\071\000\072\000\044\000\079\000\080\000"

let yysindex = "\010\000\
\014\255\000\000\023\255\000\000\000\000\112\255\013\255\112\255\
\019\255\001\255\003\255\034\255\112\255\040\255\037\255\004\255\
\024\255\000\000\000\000\074\255\044\255\000\000\000\000\000\000\
\053\255\014\255\053\255\053\255\045\255\047\255\062\255\112\255\
\014\255\123\255\000\000\023\255\023\255\112\255\112\255\112\255\
\112\255\035\255\059\255\046\255\053\255\000\000\000\000\000\000\
\014\255\112\255\061\255\053\255\012\255\064\255\000\000\000\000\
\095\255\000\000\000\000\123\255\000\000\000\000\112\255\112\255\
\112\255\112\255\063\255\053\255\112\255\044\255\048\255\050\255\
\000\000\000\000\044\255\065\255\051\255\080\255\067\255\077\255\
\012\255\089\255\000\000\088\255\090\255\112\255\000\000\000\000\
\000\000\000\000\000\000\053\255\053\255\087\255\044\255\053\255\
\079\255\012\255\014\255\096\255\099\255\000\000\000\000\100\255\
\000\000\106\255\112\255\110\255\000\000\053\255\000\000\000\000\
\014\255\000\000\000\000\000\000\000\000\112\255\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\113\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\114\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\010\255\000\000\000\000\000\000\000\000\
\000\000\117\255\000\000\000\000\000\000\000\000\104\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\124\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\250\255\052\000\237\255\000\000\231\255\000\000\000\000\069\000\
\190\255\000\000\241\255\039\000\000\000\000\000\181\255"

let yytablesize = 134
let yytable = "\024\000\
\049\000\026\000\028\000\091\000\030\000\100\000\033\000\056\000\
\094\000\048\000\001\000\050\000\051\000\077\000\025\000\024\000\
\061\000\062\000\003\000\029\000\027\000\031\000\111\000\073\000\
\036\000\055\000\078\000\058\000\108\000\006\000\024\000\063\000\
\064\000\065\000\066\000\032\000\076\000\007\000\008\000\009\000\
\010\000\034\000\035\000\074\000\037\000\042\000\052\000\011\000\
\012\000\013\000\014\000\053\000\089\000\058\000\068\000\045\000\
\084\000\085\000\086\000\066\000\046\000\047\000\090\000\054\000\
\069\000\075\000\088\000\070\000\081\000\095\000\096\000\092\000\
\093\000\112\000\018\000\019\000\020\000\106\000\021\000\104\000\
\109\000\097\000\099\000\022\000\023\000\038\000\039\000\120\000\
\098\000\040\000\101\000\102\000\107\000\103\000\119\000\018\000\
\019\000\020\000\110\000\021\000\117\000\113\000\114\000\115\000\
\022\000\023\000\038\000\039\000\082\000\116\000\040\000\121\000\
\018\000\019\000\020\000\118\000\021\000\087\000\003\000\032\000\
\020\000\022\000\023\000\018\000\019\000\057\000\029\000\021\000\
\083\000\036\000\105\000\000\000\022\000\023\000"

let yycheck = "\006\000\
\026\000\008\000\002\001\070\000\002\001\081\000\013\000\033\000\
\075\000\025\000\001\000\027\000\028\000\002\001\002\001\006\001\
\036\000\037\000\005\001\019\001\002\001\019\001\098\000\049\000\
\021\001\032\000\015\001\034\000\095\000\007\001\021\001\038\000\
\039\000\040\000\041\000\002\001\052\000\015\001\016\001\017\001\
\018\001\002\001\006\001\050\000\021\001\002\001\002\001\025\001\
\026\001\027\001\028\001\005\001\068\000\060\000\020\001\003\001\
\063\000\064\000\065\000\066\000\008\001\009\001\069\000\002\001\
\006\001\005\001\004\001\022\001\005\001\005\001\020\001\024\001\
\023\001\099\000\001\001\002\001\003\001\093\000\005\001\086\000\
\096\000\002\001\006\001\010\001\011\001\012\001\013\001\113\000\
\022\001\016\001\002\001\004\001\006\001\004\001\110\000\001\001\
\002\001\003\001\020\001\005\001\107\000\006\001\004\001\004\001\
\010\001\011\001\012\001\013\001\014\001\004\001\016\001\118\000\
\001\001\002\001\003\001\006\001\005\001\066\000\006\001\006\001\
\004\001\010\001\011\001\001\001\002\001\003\001\023\001\005\001\
\060\000\006\001\092\000\255\255\010\001\011\001"

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
# 239 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd list) in
    Obj.repr(
# 39 "parser.mly"
                          ( _2 )
# 246 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 43 "parser.mly"
                        ( [ASTStat _1] )
# 253 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 44 "parser.mly"
                  ( ASTDef(_1)::_3)
# 261 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 45 "parser.mly"
                    ( ASTStat(_1)::_3)
# 269 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 49 "parser.mly"
                        ( ASTEcho(_2) )
# 276 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 50 "parser.mly"
                        ( ASTSet(_2,_3))
# 284 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 51 "parser.mly"
                        (ASTIfStat(_2,_3,_4))
# 293 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 52 "parser.mly"
                        ( ASTWhile(_2,_3))
# 301 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 53 "parser.mly"
                      (ASTCall(_2,_3))
# 309 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 57 "parser.mly"
                        ( ASTNum(_1) )
# 316 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "parser.mly"
                        ( ASTId(_1) )
# 323 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
            ( ASTBool(false))
# 329 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
           ( ASTBool(true))
# 335 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 61 "parser.mly"
                               (ASTIf(_3,_4,_5))
# 344 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 62 "parser.mly"
                          (ASTAnd(_3,_4))
# 352 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 63 "parser.mly"
                         (ASTOr(_3,_4))
# 360 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 64 "parser.mly"
                       (ASTFun(_2,_4))
# 368 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 65 "parser.mly"
                        ( ASTApp(_2, _3) )
# 376 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 69 "parser.mly"
             ( [_1] )
# 383 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 70 "parser.mly"
             ( _1::_2 )
# 391 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 74 "parser.mly"
       ( ASTExprpExpr(_1))
# 398 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 75 "parser.mly"
                       (ASTExprpAdr(_3))
# 405 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exprp) in
    Obj.repr(
# 79 "parser.mly"
             ( [_1] )
# 412 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exprp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 80 "parser.mly"
                ( _1::_2 )
# 420 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
      ( ASTTypBool )
# 426 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
      ( ASTTypInt )
# 432 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'typs) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 86 "parser.mly"
                             ( ASTTypFleche(_2,_4) )
# 440 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 89 "parser.mly"
     ( [_1] )
# 447 "parser.ml"
               : 'typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typs) in
    Obj.repr(
# 90 "parser.mly"
                   (_1::_3)
# 455 "parser.ml"
               : 'typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 93 "parser.mly"
              (ASTArg(_1,_3))
# 463 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 96 "parser.mly"
      ([_1])
# 470 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 97 "parser.mly"
               (_1::_3)
# 478 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 100 "parser.mly"
              (ASTArgp(_1,_3))
# 486 "parser.ml"
               : 'argp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 101 "parser.mly"
                    (ASTArgpVar(_2,_4))
# 494 "parser.ml"
               : 'argp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'argp) in
    Obj.repr(
# 104 "parser.mly"
      ([_1])
# 501 "parser.ml"
               : 'argps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'argp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'argps) in
    Obj.repr(
# 105 "parser.mly"
                 (_1::_3)
# 509 "parser.ml"
               : 'argps))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 108 "parser.mly"
                                       ( ASTDefConst(_2, _3, _4) )
# 518 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 109 "parser.mly"
                                          ( ASTDefFun(_2, _3, _5, _7) )
# 528 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 110 "parser.mly"
                                          ( ASTDefFunRec(_3,_4,_6,_8) )
# 538 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 111 "parser.mly"
                          ( ASTDefVar(_2,_3))
# 546 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'argps) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 112 "parser.mly"
                                           ( ASTDefProc(_2,_4,_6))
# 555 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'argps) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 113 "parser.mly"
                                           ( ASTDefProcRec(_3,_5,_7))
# 564 "parser.ml"
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
