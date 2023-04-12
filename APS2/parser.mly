%{
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

%}
  
%token <int> NUM
%token <string> IDENT
%token LPAR RPAR 
%token LBRA RBRA
%token ECHO
%token INT BOOL 
%token TRUE FALSE
%token AND OR ADR VAR
%token IF ECHO CONST FUN REC
%token DP PV V FLECHE ETOILE REF
%token VAR PROC SET IF WHILE CALL
%token VEC LEN NTH VSET

%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.cmd list> cmds
%type <Ast.cmd list> prog
%type <Ast.lvalue> lvalue

%start prog

%%
prog: block    {$1}
;

block : LBRA cmds RBRA    { $2 }
;

cmds:
  stat                  { [ASTStat $1] }
  | def PV cmds			{ ASTDef($1)::$3}
  | stat PV cmds 			{ ASTStat($1)::$3}
;

stat:
  ECHO expr             { ASTEcho($2) }
  | SET lvalue expr      { ASTSet($2,$3)}
  | IF expr block block {ASTIfStat($2,$3,$4)}
  | WHILE expr block    { ASTWhile($2,$3)}
  | CALL IDENT exprsp {ASTCall($2,$3)}
;

expr:
  NUM                   { ASTNum($1) }
| IDENT                 { ASTId($1) }
| FALSE					{ ASTBool(false)}
| TRUE					{ ASTBool(true)}
| LPAR IF expr expr expr RPAR 	{ASTIf($3,$4,$5)}
| LPAR AND expr expr RPAR	{ASTAnd($3,$4)}
| LPAR OR expr expr RPAR	{ASTOr($3,$4)}
| LBRA args RBRA expr		{ASTFun($2,$4)}
| LPAR expr exprs RPAR  { ASTApp($2, $3) }
| LPAR ALLOC expr RPAR  {ASTAlloc($3)}
| LPAR LEN expr RPAR    {ASTLen($3)}
| LPAR NTH expr expr    {ASTnth($3, $4)}
| LPAR VSET expr exp expr  {ASTvset($3, $4, $5)}
;

exprs :
  expr       { [$1] }
| expr exprs { $1::$2 }
;

exprp:
	expr 	{ ASTExprpExpr($1)}
	| LPAR ADR IDENT RPAR {ASTExprpAdr($3)}
;
	
exprsp:
 	exprp      { [$1] }
	| exprp exprsp { $1::$2 }
;

 typ:
 BOOL { ASTTypBool }
 |INT { ASTTypInt }
 | LPAR typs FLECHE typ RPAR { ASTTypFleche($2,$4) }
 | REF typ {ASTref($2)};

 styp;
    BOOL                {ASTTypBool}
  | INT                 {ASTTypInt}  
  | LPAR VEC styp RPAR  {ASTTTypVec($3)}; 
 
 typs:
 typ { [$1] }
 | typ ETOILE typs {$1::$3};
 
 arg:
 IDENT DP typ {ASTArg($1,$3)};
 
 args:
 	arg {[$1]}
 	| arg V args {$1::$3};
 	
argp: 
	IDENT DP typ {ASTArgp($1,$3)}
	| VAR IDENT DP typ {ASTArgpVar($2,$4)};

argps:
	argp {[$1]}
 	| argp V argps {$1::$3};
 
 def:
	CONST IDENT typ expr                  { ASTDefConst($2, $3, $4) }
  | FUN IDENT typ LBRA args RBRA expr     { ASTDefFun($2, $3, $5, $7) }
  | FUN REC IDENT typ LBRA args RBRA expr { ASTDefFunRec($3,$4,$6,$8) }
  | VAR IDENT typ 						  { ASTDefVar($2,$3)}
  | PROC IDENT LBRA argps RBRA block       { ASTDefProc($2,$4,$6)}
  | PROC REC IDENT LBRA argps RBRA block   { ASTDefProcRec($3,$5,$7)}
  ;

lvalue:
    IDENT                     {ASTvalueId($1)}
  | LPAR NTH lvalue expr RPAR   {ASTValue($3, $4)};
 

