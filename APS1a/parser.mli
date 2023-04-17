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
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | EQ
  | LT
  | NOT
  | IFBK
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
  | IF
  | WHILE
  | CALL

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.cmd list
