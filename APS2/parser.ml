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

# 59 "parser.ml"
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
  293 (* ALLOC *);
  294 (* VEC *);
  295 (* LEN *);
  296 (* NTH *);
  297 (* VSET *);
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
\013\000\014\000\014\000\014\000\015\000\010\000\010\000\016\000\
\016\000\017\000\017\000\008\000\008\000\008\000\008\000\008\000\
\008\000\005\000\005\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\002\000\003\000\004\000\
\003\000\003\000\001\000\001\000\001\000\001\000\006\000\004\000\
\004\000\004\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\004\000\004\000\005\000\006\000\001\000\002\000\
\001\000\004\000\001\000\002\000\001\000\001\000\004\000\001\000\
\003\000\001\000\005\000\002\000\003\000\001\000\003\000\003\000\
\004\000\001\000\003\000\004\000\007\000\008\000\003\000\006\000\
\007\000\001\000\005\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\060\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\011\000\012\000\000\000\000\000\014\000\013\000\006\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\058\000\
\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\038\000\037\000\055\000\000\000\000\000\000\000\
\042\000\000\000\000\000\000\000\000\000\000\000\000\000\007\000\
\009\000\000\000\033\000\010\000\000\000\005\000\004\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\008\000\000\000\000\000\044\000\052\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\036\000\000\000\000\000\018\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\027\000\028\000\000\000\000\000\
\032\000\017\000\045\000\016\000\047\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\025\000\026\000\019\000\020\000\022\000\023\000\024\000\
\021\000\000\000\029\000\000\000\039\000\000\000\041\000\000\000\
\000\000\048\000\000\000\051\000\056\000\000\000\059\000\034\000\
\015\000\030\000\043\000\053\000\000\000\049\000\057\000\054\000"

let yydgoto = "\002\000\
\054\000\095\000\015\000\004\000\034\000\005\000\016\000\017\000\
\076\000\056\000\077\000\065\000\101\000\102\000\057\000\109\000\
\110\000"

let yysindex = "\003\000\
\007\255\000\000\186\255\000\000\000\000\092\255\012\255\092\255\
\025\255\255\254\003\255\018\255\092\255\092\255\051\255\237\254\
\030\255\000\000\000\000\167\255\057\255\000\000\000\000\000\000\
\015\255\007\255\060\255\060\255\062\255\061\255\063\255\000\000\
\022\255\092\255\007\255\109\255\000\000\186\255\186\255\092\255\
\092\255\092\255\092\255\092\255\092\255\092\255\092\255\092\255\
\092\255\092\255\092\255\092\255\092\255\092\255\040\255\064\255\
\043\255\038\255\000\000\000\000\000\000\007\255\023\255\060\255\
\000\000\092\255\084\255\060\255\004\255\085\255\018\255\000\000\
\000\000\123\255\000\000\000\000\109\255\000\000\000\000\092\255\
\092\255\094\255\092\255\092\255\092\255\092\255\092\255\092\255\
\092\255\095\255\096\255\092\255\092\255\092\255\097\255\060\255\
\092\255\057\255\015\255\000\000\066\255\073\255\000\000\000\000\
\057\255\100\255\081\255\107\255\086\255\110\255\004\255\092\255\
\092\255\000\000\113\255\117\255\000\000\126\255\127\255\128\255\
\134\255\143\255\144\255\092\255\000\000\000\000\145\255\092\255\
\000\000\000\000\000\000\000\000\000\000\146\255\060\255\060\255\
\149\255\057\255\060\255\125\255\004\255\007\255\150\255\153\255\
\154\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\155\255\000\000\157\255\000\000\161\255\000\000\092\255\
\160\255\000\000\060\255\000\000\000\000\007\255\000\000\000\000\
\000\000\000\000\000\000\000\000\092\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\165\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\169\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\045\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\170\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\083\255\000\000\000\000\
\000\000\000\000\000\000\000\000\175\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\250\255\079\000\015\000\000\000\105\000\243\255\000\000\000\000\
\114\000\168\255\000\000\242\255\046\000\244\255\000\000\000\000\
\148\255"

let yytablesize = 222
let yytable = "\024\000\
\028\000\026\000\143\000\001\000\030\000\107\000\035\000\036\000\
\038\000\133\000\061\000\003\000\062\000\025\000\066\000\067\000\
\137\000\058\000\108\000\032\000\033\000\073\000\059\000\060\000\
\029\000\063\000\027\000\072\000\031\000\075\000\059\000\060\000\
\164\000\080\000\081\000\082\000\083\000\084\000\085\000\086\000\
\087\000\088\000\089\000\090\000\091\000\092\000\093\000\094\000\
\100\000\161\000\035\000\103\000\078\000\079\000\064\000\106\000\
\037\000\039\000\055\000\104\000\099\000\071\000\063\000\068\000\
\070\000\069\000\096\000\059\000\060\000\097\000\075\000\098\000\
\035\000\115\000\116\000\099\000\118\000\119\000\120\000\121\000\
\122\000\123\000\124\000\131\000\134\000\127\000\128\000\094\000\
\105\000\111\000\132\000\064\000\018\000\019\000\020\000\135\000\
\021\000\117\000\125\000\126\000\130\000\022\000\023\000\136\000\
\138\000\144\000\145\000\139\000\140\000\018\000\019\000\074\000\
\040\000\021\000\141\000\142\000\146\000\154\000\022\000\023\000\
\147\000\156\000\158\000\018\000\019\000\020\000\162\000\021\000\
\165\000\148\000\149\000\150\000\022\000\023\000\040\000\041\000\
\113\000\151\000\042\000\043\000\044\000\045\000\046\000\047\000\
\048\000\049\000\152\000\153\000\155\000\157\000\174\000\163\000\
\175\000\172\000\160\000\166\000\167\000\168\000\169\000\050\000\
\170\000\051\000\052\000\053\000\171\000\173\000\176\000\018\000\
\019\000\020\000\003\000\021\000\129\000\031\000\046\000\112\000\
\022\000\023\000\040\000\041\000\050\000\159\000\042\000\043\000\
\044\000\045\000\046\000\047\000\048\000\049\000\114\000\000\000\
\006\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\007\000\000\000\000\000\050\000\000\000\051\000\052\000\053\000\
\008\000\009\000\010\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\011\000\012\000\013\000\014\000"

let yycheck = "\006\000\
\002\001\008\000\111\000\001\000\002\001\002\001\013\000\014\000\
\028\001\098\000\025\000\005\001\026\000\002\001\027\000\028\000\
\105\000\003\001\015\001\002\001\003\001\035\000\008\001\009\001\
\026\001\003\001\002\001\034\000\026\001\036\000\008\001\009\001\
\141\000\040\000\041\000\042\000\043\000\044\000\045\000\046\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\062\000\138\000\006\001\064\000\038\000\039\000\032\001\068\000\
\006\001\028\001\002\001\066\000\038\001\040\001\003\001\002\001\
\002\001\005\001\027\001\008\001\009\001\006\001\077\000\029\001\
\028\001\080\000\081\000\038\001\083\000\084\000\085\000\086\000\
\087\000\088\000\089\000\096\000\099\000\092\000\093\000\094\000\
\005\001\005\001\097\000\032\001\001\001\002\001\003\001\030\001\
\005\001\004\001\004\001\004\001\004\001\010\001\011\001\031\001\
\005\001\112\000\113\000\027\001\002\001\001\001\002\001\003\001\
\030\001\005\001\029\001\006\001\004\001\124\000\010\001\011\001\
\004\001\128\000\135\000\001\001\002\001\003\001\139\000\005\001\
\142\000\004\001\004\001\004\001\010\001\011\001\012\001\013\001\
\014\001\004\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\004\001\004\001\004\001\004\001\163\000\027\001\
\166\000\160\000\006\001\006\001\004\001\004\001\004\001\037\001\
\004\001\039\001\040\001\041\001\004\001\006\001\173\000\001\001\
\002\001\003\001\006\001\005\001\094\000\004\001\006\001\071\000\
\010\001\011\001\012\001\013\001\006\001\136\000\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\077\000\255\255\
\007\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\015\001\255\255\255\255\037\001\255\255\039\001\040\001\041\001\
\023\001\024\001\025\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\033\001\034\001\035\001\036\001"

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
# 324 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd list) in
    Obj.repr(
# 41 "parser.mly"
                          ( _2 )
# 331 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 45 "parser.mly"
                        ( [ASTStat _1] )
# 338 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 46 "parser.mly"
                  ( ASTDef(_1)::_3)
# 346 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmd list) in
    Obj.repr(
# 47 "parser.mly"
                    ( ASTStat(_1)::_3)
# 354 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 51 "parser.mly"
                        ( ASTEcho(_2) )
# 361 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.lvalue) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 52 "parser.mly"
                         ( ASTSet(_2,_3))
# 369 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 53 "parser.mly"
                        (ASTIfStat(_2,_3,_4))
# 378 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 54 "parser.mly"
                        ( ASTWhile(_2,_3))
# 386 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 55 "parser.mly"
                     (ASTCall(_2,_3))
# 394 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 59 "parser.mly"
                          ( ASTNum(_1) )
# 401 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 60 "parser.mly"
                        ( ASTId(_1) )
# 408 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
            ( ASTBool(false))
# 414 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
           ( ASTBool(true))
# 420 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 63 "parser.mly"
                               (ASTIf(_3,_4,_5))
# 429 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 64 "parser.mly"
                       (ASTFun(_2,_4))
# 437 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 65 "parser.mly"
                        ( ASTApp(_2, _3) )
# 445 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 66 "parser.mly"
                            ( ASTNot(_3) )
# 452 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 67 "parser.mly"
                            ( ASTBinary(Ast.Add, _3, _4) )
# 460 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 68 "parser.mly"
                            ( ASTBinary(Ast.Sub, _3, _4) )
# 468 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 69 "parser.mly"
                            ( ASTBinary(Ast.Mul, _3, _4) )
# 476 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 70 "parser.mly"
                            ( ASTBinary(Ast.Div, _3, _4) )
# 484 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 71 "parser.mly"
                            ( ASTBinary(Ast.Eq, _3, _4) )
# 492 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 72 "parser.mly"
                            ( ASTBinary(Ast.Lt, _3, _4) )
# 500 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 73 "parser.mly"
                          ( ASTBinary(Ast.And,_3,_4))
# 508 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 74 "parser.mly"
                         ( ASTBinary(Ast.Or,_3,_4))
# 516 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 75 "parser.mly"
                        (ASTAlloc(_3))
# 523 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 76 "parser.mly"
                        (ASTLen(_3))
# 530 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 77 "parser.mly"
                            (ASTnth(_3, _4))
# 538 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 78 "parser.mly"
                                 (ASTvset(_3, _4, _5))
# 547 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 82 "parser.mly"
             ( [_1] )
# 554 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 83 "parser.mly"
             ( _1::_2 )
# 562 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 87 "parser.mly"
       ( ASTExprpExpr(_1))
# 569 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 88 "parser.mly"
                      (ASTExprpAdr(_3))
# 576 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exprp) in
    Obj.repr(
# 92 "parser.mly"
             ( [_1] )
# 583 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exprp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 93 "parser.mly"
                ( _1::_2 )
# 591 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser.mly"
                        (ASTTypBool)
# 597 "parser.ml"
               : 'styp))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parser.mly"
                        (ASTTypInt)
# 603 "parser.ml"
               : 'styp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'styp) in
    Obj.repr(
# 99 "parser.mly"
                        (ASTTTypVec(_3))
# 610 "parser.ml"
               : 'styp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 102 "parser.mly"
     ( [_1] )
# 617 "parser.ml"
               : 'typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typs) in
    Obj.repr(
# 103 "parser.mly"
                   (_1::_3)
# 625 "parser.ml"
               : 'typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'styp) in
    Obj.repr(
# 106 "parser.mly"
      ([_1])
# 632 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'typs) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    Obj.repr(
# 107 "parser.mly"
                             ( ASTTypFleche(_2,_4) )
# 640 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 108 "parser.mly"
           (ASTref(_2))
# 647 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 112 "parser.mly"
              (ASTArg(_1,_3))
# 655 "parser.ml"
               : 'arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg) in
    Obj.repr(
# 115 "parser.mly"
      ([_1])
# 662 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 116 "parser.mly"
               (_1::_3)
# 670 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 119 "parser.mly"
              (ASTArgp(_1,_3))
# 678 "parser.ml"
               : 'argp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 120 "parser.mly"
                    (ASTArgpVar(_2,_4))
# 686 "parser.ml"
               : 'argp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'argp) in
    Obj.repr(
# 123 "parser.mly"
      ([_1])
# 693 "parser.ml"
               : 'argps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'argp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'argps) in
    Obj.repr(
# 124 "parser.mly"
                 (_1::_3)
# 701 "parser.ml"
               : 'argps))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 127 "parser.mly"
                                       ( ASTDefConst(_2, _3, _4) )
# 710 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 128 "parser.mly"
                                          ( ASTDefFun(_2, _3, _5, _7) )
# 720 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 129 "parser.mly"
                                          ( ASTDefFunRec(_3,_4,_6,_8) )
# 730 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'styp) in
    Obj.repr(
# 130 "parser.mly"
                           ( ASTDefVar(_2,_3))
# 738 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'argps) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 131 "parser.mly"
                                           ( ASTDefProc(_2,_4,_6))
# 747 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'argps) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 132 "parser.mly"
                                           ( ASTDefProcRec(_3,_5,_7))
# 756 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 136 "parser.mly"
                              (ASTvalueId(_1))
# 763 "parser.ml"
               : Ast.lvalue))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.lvalue) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 137 "parser.mly"
                                (ASTValue(_3, _4))
# 771 "parser.ml"
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
