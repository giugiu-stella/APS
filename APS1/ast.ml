(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)


type expr =
    ASTNum of int
  | ASTId of string
  | ASTApp of expr * expr list
  | ASTIf of expr * expr * expr 
  | ASTAnd of expr * expr 
  | ASTFun of arg list * expr
  | ASTOr of expr * expr

and stat = 
    ASTEcho of expr
    | ASTSET of string * expr
    | ASTIF of expr * block * block 
    | ASTWHILE of expr * block 
    | ASTCALL of string * expr list

and cmd =
    ASTStat of stat
    | ASTDef of def 

and block = cmd list

and prog= block

and typ=
    ASTTypBool
    | ASTTypInt
    | ASTTypFleche of typ list * typ
	
and arg=
    ASTArg of string * typ

and def = 
    ASTDefConst of string * typ * expr
  | ASTDefFun of string * typ * arg list * expr
  | ASTDefFunRec of string * typ * arg list * expr
  | ASTDefVar of string * typ
  | ASTDefProc of string * arg list * block
  | ASTDefProcRec of string * arg list * block
