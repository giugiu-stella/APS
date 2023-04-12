(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: lexer.mll                                                   == *)
(* ==  Lexique                                                             == *)
(* ========================================================================== *)

{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof

}
rule token = parse
    [' ' '\t' '\n']       { token lexbuf }     (* skip blanks *)
  | '['              { LBRA }
  | ']'              { RBRA }
  | '('              { LPAR }
  | ')'              { RPAR }
  | ':' 			 { DP }
  | ';'				 { PV }
  | ','				 { V }
  | "->"			 { FLECHE }
  | '*' 			 {ETOILE}
  | "ECHO"           { ECHO }
  | "CONST"			 { CONST }
  | "FUN"			 { FUN }
  | "REC"			 { REC }
  | "VAR"			 { VAR }
  | "PROC"			 {PROC}
  | "SET"			 {SET}
  | "IF"			 {IF}
  | "WHILE"          {WHILE}
  | "CALL"           {CALL}
  | "true"			 { TRUE }
  | "false"			 {FALSE}
  | "and" 			 { AND}
  | "or"			 { OR }
  | "int"			 { INT }
  | "bool"			 { BOOL }
  | "if"			 { IF }
  | "var" 			 {VAR}
  | "adr"            {ADR}
  | "ref"			 {REF}
  | "alloc"   {ALLOC}
  | "vec"     {VEC}
  | "len"     {LEN}
  | "nth"     {NTH}
  | "vset"    {VSET}
  | ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
  | ['a'-'z']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }
  | eof              { raise Eof }
