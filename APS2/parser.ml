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
  | NOT
  | PLUS
  | MINUS
  | DIV
  | EQ
  | LT
  | TIMES
  | IF
  | CONST
  | FUN
  | REC
  | VOID
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

# 60 "parser.ml"
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
  272 (* NOT *);
  273 (* PLUS *);
  274 (* MINUS *);
  275 (* DIV *);
  276 (* EQ *);
  277 (* LT *);
  278 (* TIMES *);
  279 (* IF *);
  280 (* CONST *);
  281 (* FUN *);
  282 (* REC *);
  283 (* VOID *);
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
  294 (* ALLOC *);
  295 (* VEC *);
  296 (* LEN *);
  297 (* NTH *);
  298 (* VSET *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\004\000\006\000\003\000\003\000\003\000\007\000\007\000\007\000\
\007\000\007\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\002\000\002\000\
\011\000\011\000\009\000\009\000\012\000\012\000\012\000\013\000\
\013\000\014\000\014\000\014\000\014\000\015\000\010\000\010\000\
\016\000\016\000\017\000\017\000\008\000\008\000\008\000\008\000\
\008\000\008\000\005\000\005\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\002\000\003\000\004\000\
\003\000\003\000\001\000\001\000\001\000\001\000\006\000\004\000\
\004\000\004\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\004\000\004\000\005\000\006\000\001\000\002\000\
\001\000\004\000\001\000\002\000\001\000\001\000\004\000\001\000\
\003\000\001\000\005\000\002\000\001\000\003\000\001\000\003\000\
\003\000\004\000\001\000\003\000\004\000\007\000\008\000\003\000\
\006\000\007\000\001\000\005\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\061\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\011\000\012\000\000\000\000\000\014\000\013\000\006\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\059\000\
\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\038\000\037\000\056\000\000\000\000\000\045\000\
\000\000\042\000\000\000\000\000\000\000\000\000\000\000\000\000\
\007\000\009\000\000\000\033\000\010\000\000\000\005\000\004\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\008\000\000\000\000\000\044\000\
\053\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\036\000\000\000\000\000\018\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\027\000\028\000\000\000\
\000\000\032\000\017\000\046\000\016\000\048\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\025\000\026\000\019\000\020\000\022\000\023\000\
\024\000\021\000\000\000\029\000\000\000\039\000\000\000\041\000\
\000\000\000\000\049\000\000\000\052\000\057\000\000\000\060\000\
\034\000\015\000\030\000\043\000\054\000\000\000\050\000\058\000\
\055\000"

let yydgoto = "\002\000\
\054\000\096\000\015\000\004\000\034\000\005\000\016\000\017\000\
\077\000\056\000\078\000\066\000\102\000\103\000\057\000\110\000\
\111\000"

let yysindex = "\003\000\
\007\255\000\000\196\255\000\000\000\000\057\255\012\255\057\255\
\023\255\001\255\003\255\018\255\057\255\057\255\020\255\236\254\
\025\255\000\000\000\000\168\255\031\255\000\000\000\000\000\000\
\015\255\007\255\087\255\087\255\054\255\058\255\062\255\000\000\
\024\255\057\255\007\255\099\255\000\000\196\255\196\255\057\255\
\057\255\057\255\057\255\057\255\057\255\057\255\057\255\057\255\
\057\255\057\255\057\255\057\255\057\255\057\255\038\255\063\255\
\040\255\032\255\000\000\000\000\000\000\007\255\174\255\000\000\
\087\255\000\000\057\255\068\255\087\255\004\255\069\255\018\255\
\000\000\000\000\124\255\000\000\000\000\099\255\000\000\000\000\
\057\255\057\255\073\255\057\255\057\255\057\255\057\255\057\255\
\057\255\057\255\089\255\090\255\057\255\057\255\057\255\093\255\
\087\255\057\255\031\255\015\255\000\000\060\255\066\255\000\000\
\000\000\031\255\094\255\075\255\103\255\076\255\105\255\004\255\
\057\255\057\255\000\000\108\255\109\255\000\000\111\255\112\255\
\113\255\114\255\117\255\118\255\057\255\000\000\000\000\127\255\
\057\255\000\000\000\000\000\000\000\000\000\000\128\255\087\255\
\087\255\133\255\031\255\087\255\120\255\004\255\007\255\143\255\
\129\255\146\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\147\255\000\000\149\255\000\000\152\255\000\000\
\057\255\151\255\000\000\087\255\000\000\000\000\007\255\000\000\
\000\000\000\000\000\000\000\000\000\000\057\255\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\153\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\154\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\026\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\157\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\132\255\000\000\
\000\000\000\000\000\000\000\000\000\000\161\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\250\255\063\000\013\000\000\000\100\000\243\255\000\000\000\000\
\096\000\167\255\000\000\242\255\038\000\244\255\000\000\000\000\
\145\255"

let yytablesize = 233
let yytable = "\024\000\
\144\000\026\000\028\000\001\000\030\000\108\000\035\000\036\000\
\038\000\134\000\061\000\003\000\062\000\025\000\067\000\068\000\
\138\000\058\000\109\000\032\000\033\000\074\000\059\000\060\000\
\027\000\037\000\029\000\073\000\031\000\076\000\165\000\035\000\
\055\000\081\000\082\000\083\000\084\000\085\000\086\000\087\000\
\088\000\089\000\090\000\091\000\092\000\093\000\094\000\095\000\
\101\000\162\000\079\000\080\000\104\000\039\000\035\000\069\000\
\107\000\018\000\019\000\020\000\105\000\021\000\070\000\071\000\
\072\000\097\000\022\000\023\000\098\000\099\000\100\000\076\000\
\106\000\112\000\116\000\117\000\118\000\119\000\120\000\121\000\
\122\000\123\000\124\000\125\000\132\000\135\000\128\000\129\000\
\095\000\063\000\136\000\133\000\126\000\127\000\059\000\060\000\
\131\000\137\000\139\000\018\000\019\000\075\000\140\000\021\000\
\141\000\142\000\145\000\146\000\022\000\023\000\143\000\147\000\
\148\000\064\000\149\000\150\000\151\000\152\000\155\000\065\000\
\153\000\154\000\157\000\159\000\018\000\019\000\020\000\163\000\
\021\000\166\000\156\000\158\000\168\000\022\000\023\000\040\000\
\041\000\114\000\161\000\042\000\043\000\044\000\045\000\046\000\
\047\000\048\000\049\000\164\000\167\000\169\000\170\000\175\000\
\171\000\176\000\173\000\172\000\174\000\130\000\003\000\047\000\
\031\000\050\000\040\000\051\000\052\000\053\000\051\000\177\000\
\018\000\019\000\020\000\113\000\021\000\115\000\160\000\000\000\
\063\000\022\000\023\000\040\000\041\000\059\000\060\000\042\000\
\043\000\044\000\045\000\046\000\047\000\048\000\049\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\064\000\000\000\006\000\000\000\000\000\050\000\065\000\051\000\
\052\000\053\000\007\000\000\000\100\000\000\000\000\000\000\000\
\000\000\000\000\008\000\009\000\010\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\011\000\012\000\013\000\
\014\000"

let yycheck = "\006\000\
\112\000\008\000\002\001\001\000\002\001\002\001\013\000\014\000\
\029\001\099\000\025\000\005\001\026\000\002\001\027\000\028\000\
\106\000\003\001\015\001\002\001\003\001\035\000\008\001\009\001\
\002\001\006\001\026\001\034\000\026\001\036\000\142\000\006\001\
\002\001\040\000\041\000\042\000\043\000\044\000\045\000\046\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\062\000\139\000\038\000\039\000\065\000\029\001\029\001\002\001\
\069\000\001\001\002\001\003\001\067\000\005\001\005\001\002\001\
\041\001\028\001\010\001\011\001\006\001\030\001\039\001\078\000\
\005\001\005\001\081\000\082\000\004\001\084\000\085\000\086\000\
\087\000\088\000\089\000\090\000\097\000\100\000\093\000\094\000\
\095\000\003\001\031\001\098\000\004\001\004\001\008\001\009\001\
\004\001\032\001\005\001\001\001\002\001\003\001\028\001\005\001\
\002\001\030\001\113\000\114\000\010\001\011\001\006\001\004\001\
\004\001\027\001\004\001\004\001\004\001\004\001\125\000\033\001\
\004\001\004\001\129\000\136\000\001\001\002\001\003\001\140\000\
\005\001\143\000\004\001\004\001\004\001\010\001\011\001\012\001\
\013\001\014\001\006\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\028\001\006\001\004\001\004\001\164\000\
\004\001\167\000\161\000\004\001\006\001\095\000\006\001\006\001\
\004\001\038\001\031\001\040\001\041\001\042\001\006\001\174\000\
\001\001\002\001\003\001\072\000\005\001\078\000\137\000\255\255\
\003\001\010\001\011\001\012\001\013\001\008\001\009\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\027\001\255\255\007\001\255\255\255\255\038\001\033\001\040\001\
\041\001\042\001\015\001\255\255\039\001\255\255\255\255\255\255\
\255\255\255\255\023\001\024\001\025\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\034\001\035\001\036\001\
\037\001"

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
  NOT\000\
  PLUS\000\
  MINUS\000\
  DIV\000\
  EQ\000\
  LT\000\
  TIMES\000\
  IF\000\
  CONST\000\
  FUN\000\
  REC\000\
  VOID\000\
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
# 334 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd list) in
    Obj.repr(
# 41 "parser.mly"
                          ( _2 )
# 341 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 45 "parser.mly"
                        ( [ASTStat _1] )
# 348 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 46 "parser.mly"
                  ( ASTDef(_1)::_3)
# 356 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 47 "parser.mly"
                    ( ASTStat(_1)::_3)
# 364 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 51 "parser.mly"
                        ( ASTEcho(_2) )
# 371 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 52 "parser.mly"
                         ( ASTSet(_2,_3))
# 379 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 53 "parser.mly"
                        (ASTIfStat(_2,_3,_4))
# 388 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 54 "parser.mly"
                        ( ASTWhile(_2,_3))
# 396 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 55 "parser.mly"
                     (ASTCall(_2,_3))
# 404 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 59 "parser.mly"
                          ( ASTNum(_1) )
# 411 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 60 "parser.mly"
                        ( ASTId(_1) )
# 418 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
            ( ASTBool(false))
# 424 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
           ( ASTBool(true))
# 430 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 63 "parser.mly"
                               (ASTIf(_3,_4,_5))
# 439 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 64 "parser.mly"
                       (ASTFun(_2,_4))
# 447 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 65 "parser.mly"
                        ( ASTApp(_2, _3) )
# 455 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 66 "parser.mly"
                            ( ASTNot(_3) )
# 462 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 67 "parser.mly"
                            ( ASTBinary(Ast.Add, _3, _4) )
# 470 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 68 "parser.mly"
                            ( ASTBinary(Ast.Sub, _3, _4) )
# 478 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 69 "parser.mly"
                            ( ASTBinary(Ast.Mul, _3, _4) )
# 486 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 70 "parser.mly"
                            ( ASTBinary(Ast.Div, _3, _4) )
# 494 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 71 "parser.mly"
                            ( ASTBinary(Ast.Eq, _3, _4) )
# 502 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 72 "parser.mly"
                            ( ASTBinary(Ast.Lt, _3, _4) )
# 510 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 73 "parser.mly"
                          ( ASTBinary(Ast.And,_3,_4))
# 518 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 74 "parser.mly"
                         ( ASTBinary(Ast.Or,_3,_4))
# 526 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 75 "parser.mly"
                        (ASTAlloc(_3))
# 533 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 76 "parser.mly"
                        (ASTLen(_3))
# 540 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 77 "parser.mly"
                            (ASTnth(_3, _4))
# 548 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 78 "parser.mly"
                                 (ASTvset(_3, _4, _5))
# 557 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 82 "parser.mly"
             ( [_1] )
# 564 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 83 "parser.mly"
             ( _1::_2 )
# 572 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 87 "parser.mly"
       ( ASTExprpExpr(_1))
# 579 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 88 "parser.mly"
                      (ASTExprpAdr(_3))
# 586 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exprp) in
    Obj.repr(
# 92 "parser.mly"
             ( [_1] )
# 593 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exprp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 93 "parser.mly"
                ( _1::_2 )
# 601 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser.mly"
                        (ASTTypBool)
# 607 "parser.ml"
               : 'styp))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parser.mly"
                        (ASTTypInt)
# 613 "parser.ml"
               : 'styp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'styp) in
    Obj.repr(
# 99 "parser.mly"
                        (ASTTTypVec(_3))
# 620 "parser.ml"
               : 'styp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 102 "parser.mly"
     ( [_1] )
# 627 "parser.ml"
               : 'typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typs) in
    Obj.repr(
# 103 "parser.mly"
                   (_1::_3)
# 635 "parser.ml"
               : 'typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'styp) in
    Obj.repr(
# 106 "parser.mly"
      (ASTStyp(_1))
# 642 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'typs) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 107 "parser.mly"
                             ( ASTTypFleche(_2,_4) )
# 650 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 108 "parser.mly"
           (ASTref(_2))
# 657 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 109 "parser.mly"
          (ASTTypVoid)
# 663 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 113 "parser.mly"
              (ASTArg(_1,_3))
# 671 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 116 "parser.mly"
      ([_1])
# 678 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 117 "parser.mly"
               (_1::_3)
# 686 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 120 "parser.mly"
              (ASTArgp(_1,_3))
# 694 "parser.ml"
               : 'argp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 121 "parser.mly"
                    (ASTArgpVar(_2,_4))
# 702 "parser.ml"
               : 'argp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'argp) in
    Obj.repr(
# 124 "parser.mly"
      ([_1])
# 709 "parser.ml"
               : 'argps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'argp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'argps) in
    Obj.repr(
# 125 "parser.mly"
                 (_1::_3)
# 717 "parser.ml"
               : 'argps))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 128 "parser.mly"
                                       ( ASTDefConst(_2, _3, _4) )
# 726 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 129 "parser.mly"
                                          ( ASTDefFun(_2, _3, _5, _7) )
# 736 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 130 "parser.mly"
                                          ( ASTDefFunRec(_3,_4,_6,_8) )
# 746 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'styp) in
    Obj.repr(
# 131 "parser.mly"
                           ( ASTDefVar(_2,_3))
# 754 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'argps) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 132 "parser.mly"
                                           ( ASTDefProc(_2,_4,_6))
# 763 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'argps) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 133 "parser.mly"
                                           ( ASTDefProcRec(_3,_5,_7))
# 772 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 137 "parser.mly"
                              (ASTvalueId(_1))
# 779 "parser.ml"
               : Ast.lvalue))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.lvalue) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 138 "parser.mly"
                                (ASTValue(_3, _4))
# 787 "parser.ml"
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
