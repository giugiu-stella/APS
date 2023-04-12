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
  | ALLOC
  | VEC
  | LEN
  | NTH
  | VSET

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

# 53 "parser.ml"
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
  286 (* ALLOC *);
  287 (* VEC *);
  288 (* LEN *);
  289 (* NTH *);
  290 (* VSET *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\004\000\006\000\003\000\003\000\003\000\007\000\007\000\007\000\
\007\000\007\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\002\000\
\002\000\011\000\011\000\009\000\009\000\012\000\012\000\012\000\
\012\000\014\000\014\000\014\000\013\000\013\000\015\000\010\000\
\010\000\016\000\016\000\017\000\017\000\008\000\008\000\008\000\
\008\000\008\000\008\000\005\000\005\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\002\000\003\000\004\000\
\003\000\003\000\001\000\001\000\001\000\001\000\006\000\005\000\
\005\000\004\000\004\000\004\000\004\000\004\000\005\000\001\000\
\002\000\001\000\004\000\001\000\002\000\001\000\001\000\005\000\
\002\000\001\000\001\000\004\000\001\000\003\000\003\000\001\000\
\003\000\003\000\004\000\001\000\003\000\004\000\007\000\008\000\
\003\000\006\000\007\000\001\000\005\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\054\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\011\000\012\000\000\000\000\000\014\000\013\000\006\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\052\000\
\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\031\000\030\000\000\000\049\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\007\000\009\000\
\000\000\026\000\010\000\000\000\005\000\004\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\033\000\008\000\046\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\029\000\000\000\000\000\000\000\020\000\021\000\022\000\000\000\
\025\000\019\000\039\000\018\000\041\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\016\000\017\000\000\000\023\000\038\000\000\000\000\000\000\000\
\042\000\000\000\045\000\050\000\000\000\053\000\027\000\015\000\
\032\000\047\000\000\000\043\000\051\000\048\000"

let yydgoto = "\002\000\
\047\000\079\000\015\000\004\000\034\000\005\000\016\000\017\000\
\067\000\049\000\068\000\083\000\084\000\000\000\050\000\092\000\
\093\000"

let yysindex = "\016\000\
\016\255\000\000\124\255\000\000\000\000\085\255\025\255\085\255\
\030\255\003\255\004\255\017\255\085\255\031\255\040\255\032\255\
\033\255\000\000\000\000\104\255\053\255\000\000\000\000\000\000\
\000\255\016\255\000\255\000\255\054\255\059\255\067\255\000\000\
\041\255\085\255\016\255\144\255\000\000\124\255\124\255\085\255\
\085\255\085\255\085\255\085\255\085\255\085\255\085\255\056\255\
\072\255\051\255\000\255\000\000\000\000\000\255\000\000\016\255\
\085\255\077\255\000\255\009\255\078\255\017\255\000\000\000\000\
\047\255\000\000\000\000\144\255\000\000\000\000\085\255\085\255\
\085\255\080\255\081\255\085\255\085\255\085\255\087\255\000\255\
\085\255\053\255\069\255\074\255\000\000\000\000\000\000\053\255\
\095\255\083\255\099\255\082\255\105\255\009\255\085\255\108\255\
\000\000\109\255\115\255\085\255\000\000\000\000\000\000\085\255\
\000\000\000\000\000\000\000\000\000\000\000\255\000\255\106\255\
\053\255\000\255\102\255\009\255\016\255\117\255\120\255\121\255\
\000\000\000\000\122\255\000\000\000\000\123\255\085\255\126\255\
\000\000\000\255\000\000\000\000\016\255\000\000\000\000\000\000\
\000\000\000\000\085\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\129\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\137\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\008\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\125\255\000\000\000\000\
\000\000\000\000\107\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\138\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\250\255\050\000\006\000\000\000\086\000\231\255\000\000\000\000\
\088\000\186\255\000\000\244\255\047\000\000\000\000\000\000\000\
\166\255"

let yytablesize = 157
let yytable = "\024\000\
\056\000\026\000\051\000\118\000\028\000\030\000\035\000\052\000\
\053\000\064\000\090\000\109\000\055\000\028\000\057\000\058\000\
\001\000\112\000\032\000\033\000\003\000\029\000\031\000\091\000\
\054\000\131\000\025\000\063\000\028\000\066\000\086\000\027\000\
\036\000\071\000\072\000\073\000\074\000\075\000\076\000\077\000\
\078\000\085\000\128\000\069\000\070\000\037\000\089\000\018\000\
\019\000\020\000\087\000\021\000\038\000\039\000\048\000\059\000\
\022\000\023\000\040\000\041\000\096\000\066\000\042\000\060\000\
\098\000\099\000\100\000\107\000\061\000\103\000\104\000\078\000\
\082\000\062\000\108\000\080\000\043\000\081\000\044\000\045\000\
\046\000\088\000\094\000\101\000\102\000\018\000\019\000\020\000\
\119\000\021\000\106\000\132\000\110\000\123\000\022\000\023\000\
\111\000\124\000\126\000\113\000\115\000\129\000\114\000\116\000\
\018\000\019\000\020\000\141\000\021\000\120\000\117\000\127\000\
\121\000\022\000\023\000\040\000\041\000\140\000\122\000\042\000\
\138\000\130\000\133\000\134\000\135\000\136\000\137\000\105\000\
\024\000\037\000\006\000\139\000\142\000\043\000\003\000\044\000\
\045\000\046\000\007\000\008\000\009\000\010\000\040\000\044\000\
\018\000\019\000\065\000\095\000\021\000\011\000\012\000\013\000\
\014\000\022\000\023\000\097\000\125\000"

let yycheck = "\006\000\
\026\000\008\000\003\001\094\000\002\001\002\001\013\000\008\001\
\009\001\035\000\002\001\082\000\025\000\006\001\027\000\028\000\
\001\000\088\000\002\001\003\001\005\001\019\001\019\001\015\001\
\025\001\116\000\002\001\034\000\021\001\036\000\056\000\002\001\
\002\001\040\000\041\000\042\000\043\000\044\000\045\000\046\000\
\047\000\054\000\113\000\038\000\039\000\006\001\059\000\001\001\
\002\001\003\001\057\000\005\001\021\001\021\001\002\001\002\001\
\010\001\011\001\012\001\013\001\014\001\068\000\016\001\005\001\
\071\000\072\000\073\000\080\000\002\001\076\000\077\000\078\000\
\022\001\033\001\081\000\020\001\030\001\006\001\032\001\033\001\
\034\001\005\001\005\001\004\001\004\001\001\001\002\001\003\001\
\095\000\005\001\004\001\117\000\024\001\100\000\010\001\011\001\
\023\001\104\000\111\000\005\001\002\001\114\000\020\001\022\001\
\001\001\002\001\003\001\133\000\005\001\002\001\006\001\006\001\
\004\001\010\001\011\001\012\001\013\001\130\000\004\001\016\001\
\127\000\020\001\006\001\004\001\004\001\004\001\004\001\078\000\
\004\001\023\001\007\001\006\001\139\000\030\001\006\001\032\001\
\033\001\034\001\015\001\016\001\017\001\018\001\006\001\006\001\
\001\001\002\001\003\001\062\000\005\001\026\001\027\001\028\001\
\029\001\010\001\011\001\068\000\110\000"

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
  ALLOC\000\
  VEC\000\
  LEN\000\
  NTH\000\
  VSET\000\
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
# 274 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd list) in
    Obj.repr(
# 41 "parser.mly"
                          ( _2 )
# 281 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 45 "parser.mly"
                        ( [ASTStat _1] )
# 288 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 46 "parser.mly"
                  ( ASTDef(_1)::_3)
# 296 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 47 "parser.mly"
                    ( ASTStat(_1)::_3)
# 304 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 51 "parser.mly"
                        ( ASTEcho(_2) )
# 311 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 52 "parser.mly"
                         ( ASTSet(_2,_3))
# 319 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 53 "parser.mly"
                        (ASTIfStat(_2,_3,_4))
# 328 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 54 "parser.mly"
                        ( ASTWhile(_2,_3))
# 336 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 55 "parser.mly"
                      (ASTCall(_2,_3))
# 344 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 59 "parser.mly"
                        ( ASTNum(_1) )
# 351 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 60 "parser.mly"
                        ( ASTId(_1) )
# 358 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
            ( ASTBool(false))
# 364 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
           ( ASTBool(true))
# 370 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 63 "parser.mly"
                               (ASTIf(_3,_4,_5))
# 379 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 64 "parser.mly"
                          (ASTAnd(_3,_4))
# 387 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 65 "parser.mly"
                         (ASTOr(_3,_4))
# 395 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 66 "parser.mly"
                       (ASTFun(_2,_4))
# 403 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 67 "parser.mly"
                        ( ASTApp(_2, _3) )
# 411 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 68 "parser.mly"
                        (ASTAlloc(_3))
# 418 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 69 "parser.mly"
                        (ASTLen(_3))
# 425 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 70 "parser.mly"
                        (ASTnth(_3, _4))
# 433 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 71 "parser.mly"
                            (ASTvset(_3, _4, _5))
# 442 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 75 "parser.mly"
             ( [_1] )
# 449 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 76 "parser.mly"
             ( _1::_2 )
# 457 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 80 "parser.mly"
       ( ASTExprpExpr(_1))
# 464 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 81 "parser.mly"
                       (ASTExprpAdr(_3))
# 471 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exprp) in
    Obj.repr(
# 85 "parser.mly"
             ( [_1] )
# 478 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exprp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 86 "parser.mly"
                ( _1::_2 )
# 486 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
      ( ASTTypBool )
# 492 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
      ( ASTTypInt )
# 498 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'typs) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 92 "parser.mly"
                             ( ASTTypFleche(_2,_4) )
# 506 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 93 "parser.mly"
           (ASTref(_2))
# 513 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
                        (ASTTypBool)
# 519 "parser.ml"
               : 'styp))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser.mly"
                        (ASTTypInt)
# 525 "parser.ml"
               : 'styp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'styp) in
    Obj.repr(
# 98 "parser.mly"
                        (ASTTTypVec(_3))
# 532 "parser.ml"
               : 'styp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 101 "parser.mly"
     ( [_1] )
# 539 "parser.ml"
               : 'typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typs) in
    Obj.repr(
# 102 "parser.mly"
                   (_1::_3)
# 547 "parser.ml"
               : 'typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 105 "parser.mly"
              (ASTArg(_1,_3))
# 555 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 108 "parser.mly"
      ([_1])
# 562 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 109 "parser.mly"
               (_1::_3)
# 570 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 112 "parser.mly"
              (ASTArgp(_1,_3))
# 578 "parser.ml"
               : 'argp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 113 "parser.mly"
                    (ASTArgpVar(_2,_4))
# 586 "parser.ml"
               : 'argp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'argp) in
    Obj.repr(
# 116 "parser.mly"
      ([_1])
# 593 "parser.ml"
               : 'argps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'argp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'argps) in
    Obj.repr(
# 117 "parser.mly"
                 (_1::_3)
# 601 "parser.ml"
               : 'argps))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 120 "parser.mly"
                                       ( ASTDefConst(_2, _3, _4) )
# 610 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 121 "parser.mly"
                                          ( ASTDefFun(_2, _3, _5, _7) )
# 620 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 122 "parser.mly"
                                          ( ASTDefFunRec(_3,_4,_6,_8) )
# 630 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 123 "parser.mly"
                          ( ASTDefVar(_2,_3))
# 638 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'argps) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 124 "parser.mly"
                                           ( ASTDefProc(_2,_4,_6))
# 647 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'argps) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 125 "parser.mly"
                                           ( ASTDefProcRec(_3,_5,_7))
# 656 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 129 "parser.mly"
                              (ASTvalueId(_1))
# 663 "parser.ml"
               : Ast.lvalue))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.lvalue) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 130 "parser.mly"
                                (ASTValue(_3, _4))
# 671 "parser.ml"
               : Ast.lvalue))
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
