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
| ASTBool of bool
| ASTApp of expr * expr list
| ASTIf of expr * expr * expr 
| ASTFun of arg list * expr
| ASTBinary of ops * expr * expr
| ASTNot of expr
| ASTAlloc of expr 
| ASTLen of expr 
| ASTnth of expr * expr 
| ASTvset of expr * expr * expr 

and ops = Add | Mul | Sub | Div | Eq | Lt | Or | And

and exprp= 
    ASTExprpExpr of expr
    | ASTExprpAdr of expr

and stat = 
    ASTEcho of expr
    | ASTSet of lvalue * expr
    | ASTIfStat of expr * block * block 
    | ASTWhile of expr * block 
    | ASTCall of expr * exprp list
and cmd =
    ASTStat of stat
    | ASTDef of def 

and block = cmd list

and prog= block

and styp=
    ASTTypBool 
    | ASTTypInt
    | ASTTTypVec of styp 

and typ=
    ASTStyp of styp
    |ASTTypFleche of typ list * typ
    | ASTref of typ
    | ASTTypVoid 
	
and arg=
    ASTArg of string * typ

and argp= 
    ASTArgp of string * typ
    | ASTArgpVar of string *typ

and def = 
    ASTDefConst of string * typ * expr
  | ASTDefFun of string * typ * arg list * expr
  | ASTDefFunRec of string * typ * arg list * expr
  | ASTDefVar of string * styp
  | ASTDefProc of string * argp list * block
  | ASTDefProcRec of string * argp list * block

and lvalue = 
      ASTvalueId of string 
    | ASTValue of lvalue * expr
