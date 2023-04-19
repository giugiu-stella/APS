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

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.cmd list
