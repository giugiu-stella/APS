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
%token AND OR 
%token IF ECHO CONST FUN REC
%token DP PV V FLECHE ETOILE

%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.cmd list> cmds
%type <Ast.cmd list> prog


%start prog

%%
prog: LBRA cmds RBRA    { $2 }
;

cmds:
  stat                  { [ASTStat $1] }
;

stat:
  ECHO expr             { ASTEcho($2) }
;

expr:
  NUM                   { ASTNum($1) }
| IDENT                 { ASTId($1) }
| LPAR IF expr expr expr RPAR 	{ASTIf($3,$4,$5)}
| LPAR AND expr expr RPAR	{ASTAnd($3,$4)}
| LPAR OR expr expr RPAR	{ASTOr($3,$4)}
| LBRA args RBRA expr		{ASTFun($2,$4)}
| LPAR expr exprs RPAR  { ASTApp($2, $3) }
;

exprs :
  expr       { [$1] }
| expr exprs { $1::$2 }
;

 typ:
 BOOL { ASTTypBool }
 |INT { ASTTypInt }
 | LPAR typs FLECHE typ RPAR { ASTTypFleche($2,$4) };
 
 typs:
 typ { [$1] }
 | typ ETOILE typs {$1::$3};
 
 arg:
 IDENT DP typ {ASTArg($1,$3)};
 
 args:
 	arg {[$1]}
 	| arg V args {$1::$3};
 	
 def:
	CONST IDENT typ expr                  { ASTDefConst($2, $3, $4) }
  | FUN IDENT typ LBRA args RBRA expr     { ASTDefFun($2, $3, $5, $7) }
  | FUN REC IDENT typ LBRA args RBRA expr { ASTDefFunRec($3,$4,$6,$8) }
  ;
 

