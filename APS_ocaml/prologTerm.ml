(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: prologTerm.ml                                               == *)
(* ==  Génération de termes Prolog                                         == *)
(* ========================================================================== *)
open Ast
  
let rec print_expr e =
  match e with
      ASTNum n -> Printf.printf"num(%d)" n
    | ASTId x -> Printf.printf"id(%s)" x
    | ASTApp(e, es) -> (
      Printf.printf"app(";
      print_expr e;
      Printf.printf",[";
      print_exprs es;
      Printf.printf"])"
    )
    | ASTIf(cond,vrai,faux) -> (
      Printf.printf "if";
      Printf.printf "(";
      print_expr cond;
      Printf.printf ",";
      print_expr vrai;
      Printf.printf ",";
      print_expr faux;
      Printf.printf ")";
    )
    | ASTAnd(e1,e2) -> (
      Printf.printf "and";
      Printf.printf "(";
      print_expr e1;
      Printf.printf ",";
      print_expr e2;
      Printf.printf ")";
    )
    | ASTFun(args,e) -> (
      Printf.printf "abs";
      Printf.printf "(";
      Printf.printf "[";
      print_args args;
      Printf.printf "]";
      Printf.printf ",";
      print_expr e;
      Printf.printf ")";
    )
    | ASTOr(e1,e2) -> (
      Printf.printf "or";
      Printf.printf "(";
      print_expr e1;
      Printf.printf ",";
      print_expr e2;
      Printf.printf ")";
    )
      
and print_exprs es =
  match es with
      [] -> ()
    | [e] -> print_expr e
    | e::es -> (
	print_expr e;
	print_char ',';
	print_exprs es
      )

let print_stat s =
  match s with
      ASTEcho e -> (
	Printf.printf("echo(");
	print_expr(e);
	Printf.printf(")")
      )

let print_cmd c =
  match c with
      ASTStat s -> print_stat s
	
let rec print_cmds cs =
  match cs with
      c::[] -> print_cmd c
    | _ -> failwith "not yet implemented"

let rec print_type t=
  match t with 
  ASTTypBool -> ????
  | ASTTypInt -> ????
  | ASTTypFleche(typ,typs) -> (
      print_type typ;
      Printf.printf ",";
      ????? typs;
    )

let print_arg arg = 
  match arg with
    ASTArg(nom, typ) -> (
      Printf.printf "(";
      Printf.printf "%s" nom;
      Printf.printf ",";
      print_type typ;
      Printf.printf ")"
    )

let print_def d = 
  match d with
    ASTDefConst(nom, type, e) -> (
      Printf.printf "const";
      Printf.printf "(";
      Printf.printf "%s" nom;
      Printf.printf ",";
      print_type type;
      Printf.printf ",";
      print_expr e;
      Printf.printf ")";
    )
  | ASTDefFun(nom, type, args, e) -> (
      Printf.printf "fun";
      Printf.printf "(";
      Printf.printf "%s" nom;
      Printf.printf ",";
      print_type type;
      Printf.printf ",";
      Printf.printf "[";
      ?????? args;
      Printf.printf "]";
      Printf.printf ",";
      print_expr e;
      Printf.printf ")";
    )
  | ASTDefFunRec(nom, type, args, e) -> (
      Printf.printf "funrec";
      Printf.printf "(";
      Printf.printf "%s" nom;
      Printf.printf ",";
      print_type type;
      Printf.printf ",";
      Printf.printf "[";
      ???? args;
      Printf.printf "]";
      Printf.printf ",";
      print_expr e;
      Printf.printf ")";
  )

let print_prog p =
  Printf.printf("prog([");
  print_cmds p;
  Printf.printf("])")
;;
	
let fname = Sys.argv.(1) in
let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
      print_prog p;
      print_string ".\n"
  with Lexer.Eof ->
    exit 0
      
