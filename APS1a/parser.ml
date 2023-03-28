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
\001\000\002\000\002\000\010\000\010\000\008\000\008\000\011\000\
\011\000\011\000\012\000\012\000\013\000\009\000\009\000\014\000\
\014\000\015\000\015\000\007\000\007\000\007\000\007\000\007\000\
\007\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\002\000\003\000\004\000\
\003\000\003\000\001\000\001\000\006\000\005\000\005\000\004\000\
\004\000\001\000\002\000\001\000\004\000\001\000\002\000\001\000\
\001\000\005\000\001\000\003\000\003\000\001\000\003\000\003\000\
\004\000\001\000\003\000\004\000\007\000\008\000\003\000\006\000\
\007\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\042\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\011\000\012\000\000\000\000\000\006\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\025\000\024\000\039\000\000\000\000\000\
\000\000\000\000\000\000\000\000\007\000\009\000\000\000\020\000\
\010\000\000\000\005\000\004\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\008\000\036\000\
\000\000\000\000\000\000\000\000\000\000\023\000\000\000\000\000\
\000\000\019\000\017\000\029\000\016\000\031\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\014\000\015\000\000\000\
\028\000\000\000\000\000\000\000\040\000\000\000\021\000\013\000\
\026\000\037\000\000\000\041\000\038\000"

let yydgoto = "\002\000\
\039\000\065\000\015\000\004\000\005\000\016\000\017\000\057\000\
\041\000\058\000\069\000\070\000\042\000\000\000\000\000"

let yysindex = "\014\000\
\252\254\000\000\061\255\000\000\000\000\043\255\015\255\043\255\
\021\255\001\255\003\255\023\255\043\255\026\255\029\255\017\255\
\018\255\000\000\000\000\109\255\034\255\000\000\005\255\252\254\
\005\255\005\255\038\255\036\255\045\255\043\255\252\254\062\255\
\000\000\061\255\061\255\043\255\043\255\043\255\043\255\030\255\
\047\255\027\255\005\255\000\000\000\000\000\000\252\254\043\255\
\049\255\005\255\034\255\055\255\000\000\000\000\093\255\000\000\
\000\000\062\255\000\000\000\000\043\255\043\255\043\255\043\255\
\058\255\005\255\043\255\034\255\042\255\046\255\000\000\000\000\
\034\255\065\255\066\255\034\255\071\255\000\000\070\255\076\255\
\043\255\000\000\000\000\000\000\000\000\000\000\005\255\005\255\
\077\255\034\255\252\254\078\255\081\255\000\000\000\000\086\255\
\000\000\087\255\043\255\091\255\000\000\252\254\000\000\000\000\
\000\000\000\000\043\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\094\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\096\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\255\000\000\000\000\000\000\000\000\000\000\088\255\
\000\000\000\000\000\000\000\000\080\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\250\255\035\000\231\255\000\000\236\255\000\000\000\000\046\000\
\217\255\000\000\249\255\021\000\000\000\000\000\000\000"

let yytablesize = 125
let yytable = "\022\000\
\003\000\024\000\026\000\047\000\028\000\022\000\031\000\043\000\
\059\000\060\000\054\000\075\000\044\000\045\000\001\000\046\000\
\023\000\048\000\049\000\027\000\022\000\029\000\025\000\053\000\
\030\000\056\000\071\000\032\000\086\000\061\000\062\000\063\000\
\064\000\089\000\033\000\040\000\092\000\034\000\035\000\050\000\
\051\000\072\000\074\000\018\000\019\000\020\000\052\000\021\000\
\068\000\066\000\100\000\056\000\067\000\073\000\079\000\080\000\
\081\000\064\000\084\000\076\000\085\000\083\000\018\000\019\000\
\055\000\087\000\021\000\006\000\088\000\090\000\101\000\091\000\
\093\000\094\000\096\000\007\000\008\000\009\000\010\000\095\000\
\098\000\108\000\099\000\102\000\103\000\011\000\012\000\013\000\
\014\000\104\000\105\000\018\000\106\000\018\000\019\000\020\000\
\107\000\021\000\082\000\003\000\109\000\030\000\027\000\078\000\
\036\000\037\000\077\000\097\000\038\000\018\000\019\000\020\000\
\000\000\021\000\000\000\000\000\000\000\000\000\000\000\000\000\
\036\000\037\000\000\000\000\000\038\000"

let yycheck = "\006\000\
\005\001\008\000\002\001\024\000\002\001\006\001\013\000\003\001\
\034\000\035\000\031\000\051\000\008\001\009\001\001\000\023\000\
\002\001\025\000\026\000\019\001\021\001\019\001\002\001\030\000\
\002\001\032\000\047\000\002\001\068\000\036\000\037\000\038\000\
\039\000\073\000\006\001\002\001\076\000\021\001\021\001\002\001\
\005\001\048\000\050\000\001\001\002\001\003\001\002\001\005\001\
\022\001\020\001\090\000\058\000\006\001\005\001\061\000\062\000\
\063\000\064\000\066\000\005\001\067\000\004\001\001\001\002\001\
\003\001\024\001\005\001\007\001\023\001\005\001\091\000\006\001\
\002\001\004\001\081\000\015\001\016\001\017\001\018\001\004\001\
\088\000\102\000\006\001\006\001\004\001\025\001\026\001\027\001\
\028\001\004\001\004\001\004\001\099\000\001\001\002\001\003\001\
\006\001\005\001\064\000\006\001\107\000\006\001\023\001\058\000\
\012\001\013\001\014\001\087\000\016\001\001\001\002\001\003\001\
\255\255\005\001\255\255\255\255\255\255\255\255\255\255\255\255\
\012\001\013\001\255\255\255\255\016\001"

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
# 231 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd list) in
    Obj.repr(
# 39 "parser.mly"
                          ( _2 )
# 238 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 43 "parser.mly"
                        ( [ASTStat _1] )
# 245 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 44 "parser.mly"
                  ( ASTDef(_1)::_3)
# 253 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 45 "parser.mly"
                    ( ASTStat(_1)::_3)
# 261 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 49 "parser.mly"
                        ( ASTEcho(_2) )
# 268 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 50 "parser.mly"
                        ( ASTSet(_2,_3))
# 276 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 51 "parser.mly"
                        (ASTIfStat(_2,_3,_4))
# 285 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 52 "parser.mly"
                        ( ASTWhile(_2,_3))
# 293 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 53 "parser.mly"
                      (ASTCall(_2,_3))
# 301 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 57 "parser.mly"
                        ( ASTNum(_1) )
# 308 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "parser.mly"
                        ( ASTId(_1) )
# 315 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 59 "parser.mly"
                               (ASTIf(_3,_4,_5))
# 324 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 60 "parser.mly"
                          (ASTAnd(_3,_4))
# 332 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 61 "parser.mly"
                         (ASTOr(_3,_4))
# 340 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 62 "parser.mly"
                       (ASTFun(_2,_4))
# 348 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 63 "parser.mly"
                        ( ASTApp(_2, _3) )
# 356 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 67 "parser.mly"
             ( [_1] )
# 363 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 68 "parser.mly"
             ( _1::_2 )
# 371 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 72 "parser.mly"
       ( ASTExprpExpr(_1))
# 378 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 73 "parser.mly"
                       (ASTExprpAdr(_3))
# 385 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exprp) in
    Obj.repr(
# 77 "parser.mly"
             ( [_1] )
# 392 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exprp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 78 "parser.mly"
                ( _1::_2 )
# 400 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
      ( ASTTypBool )
# 406 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
      ( ASTTypInt )
# 412 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'typs) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 84 "parser.mly"
                             ( ASTTypFleche(_2,_4) )
# 420 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 87 "parser.mly"
     ( [_1] )
# 427 "parser.ml"
               : 'typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typs) in
    Obj.repr(
# 88 "parser.mly"
                   (_1::_3)
# 435 "parser.ml"
               : 'typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 91 "parser.mly"
              (ASTArg(_1,_3))
# 443 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 94 "parser.mly"
      ([_1])
# 450 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 95 "parser.mly"
               (_1::_3)
# 458 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 98 "parser.mly"
              (ASTArgp(_1,_3))
# 466 "parser.ml"
               : 'argp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 99 "parser.mly"
                    (ASTArgpVar(_2,_4))
# 474 "parser.ml"
               : 'argp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'argp) in
    Obj.repr(
# 102 "parser.mly"
      ([_1])
# 481 "parser.ml"
               : 'argps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'argp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'argps) in
    Obj.repr(
# 103 "parser.mly"
                 (_1::_3)
# 489 "parser.ml"
               : 'argps))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 106 "parser.mly"
                                       ( ASTDefConst(_2, _3, _4) )
# 498 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 107 "parser.mly"
                                          ( ASTDefFun(_2, _3, _5, _7) )
# 508 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 108 "parser.mly"
                                          ( ASTDefFunRec(_3,_4,_6,_8) )
# 518 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 109 "parser.mly"
                          ( ASTDefVar(_2,_3))
# 526 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 110 "parser.mly"
                                          ( ASTDefProc(_2,_4,_6))
# 535 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 111 "parser.mly"
                                              ( ASTDefProcRec(_3,_5,_7))
# 544 "parser.ml"
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
