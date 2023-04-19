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
%token PLUS MINUS TIMES DIV
%token EQ LT NOT VOID
%token IF ECHO CONST FUN REC
%token DP PV V FLECHE ETOILE REF
%token VAR PROC SET IF WHILE CALL

%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.cmd list> cmds
%type <Ast.cmd list> prog

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
  | SET IDENT expr      { ASTSet($2,$3)}
  | IF expr block block {ASTIfStat($2,$3,$4)}
  | WHILE expr block    { ASTWhile($2,$3)}
  | CALL expr exprsp {ASTCall($2,$3)}
;

expr:
  NUM                   { ASTNum($1) }
| IDENT                 { ASTId($1) }
| FALSE					{ ASTBool(false)}
| TRUE					{ ASTBool(true)}
| LPAR IF expr expr expr RPAR 	{ASTIf($3,$4,$5)}
| LBRA args RBRA expr		{ASTFun($2,$4)}
| LPAR expr exprs RPAR  { ASTApp($2, $3) }
| LPAR NOT expr RPAR        { ASTNot($3) }
| LPAR PLUS expr expr RPAR  { ASTBinary(Ast.Add, $3, $4) }
| LPAR MINUS expr expr RPAR { ASTBinary(Ast.Sub, $3, $4) }
| LPAR TIMES expr expr RPAR { ASTBinary(Ast.Mul, $3, $4) }
| LPAR DIV expr expr RPAR   { ASTBinary(Ast.Div, $3, $4) }
| LPAR EQ expr expr RPAR    { ASTBinary(Ast.Eq, $3, $4) }
| LPAR LT expr expr RPAR    { ASTBinary(Ast.Lt, $3, $4) }
| LPAR AND expr expr RPAR	{ ASTBinary(Ast.And,$3,$4)}
| LPAR OR expr expr RPAR	{ ASTBinary(Ast.Or,$3,$4)}
;

exprs :
  expr       { [$1] }
| expr exprs { $1::$2 }
;

exprp:
	expr 	{ ASTExprpExpr($1)}
	| LPAR ADR expr RPAR {ASTExprpAdr($3)}
;
	
exprsp:
 	exprp      { [$1] }
	| exprp exprsp { $1::$2 }
;

 typ:
 BOOL { ASTTypBool }
 |INT { ASTTypInt }
 |VOID {ASTTypVoid}
 | LPAR typs FLECHE typ RPAR { ASTTypFleche($2,$4) }
 | REF typ {ASTref($2)};
 
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
 