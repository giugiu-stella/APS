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
  

let rec print_list print l sep =
  match l with
  [] -> ()
  | [x] -> print x;
  | x::reste -> print x; Printf.printf "%s" sep; print_list print reste sep


let rec print_expr e =
  match e with
      ASTNum n -> Printf.printf "%d" n
    | ASTId x -> Printf.printf"id(%s)" x
    | ASTBool x -> Printf.printf "%B" x
    | ASTApp(e, es) -> (
      Printf.printf"app(";
      print_expr e;
      Printf.printf",[";
      print_list print_expr es ",";
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
      print_list print_arg args ",";
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

and print_exprp ep = 
  match ep with
    ASTExprpExpr(e) -> print_expr e;
    | ASTExprpAdr(x)->(
      Printf.printf "adr";
      Printf.printf "(";
      Printf.printf "%s" x;
      Printf.printf ")";
    )

and print_exprsp esp =
  match esp with
      [] -> ()
    | [e] -> print_exprp e
    | e::es -> (
	print_exprp e;
	print_char ',';
	print_exprsp es)


and print_stat s =
  match s with
      ASTEcho e -> (
        Printf.printf("echo(");
        print_expr(e);
        Printf.printf(")")
        )
      | ASTSet(x,e)-> 
        Printf.printf("set");
        Printf.printf "(";
        Printf.printf "%s" x;
        print_char ',';
        print_expr e;
        Printf.printf ")";
      | ASTIfStat(e,bk1,bk2)-> (
        Printf.printf "if";
        Printf.printf "(";
        print_expr e;
        Printf.printf ",";
        print_block bk1;
        Printf.printf ",";
        print_block bk2;
        Printf.printf ")";
        )
      | ASTWhile(e,bk)->(
        Printf.printf "while";
        Printf.printf "(";
        print_expr e;
        Printf.printf ",";
        print_block bk;
        Printf.printf ")";
        )
      | ASTCall(x,eps)->(
        Printf.printf "call";
        Printf.printf "(";
        Printf.printf "%s" x;
        Printf.printf ",";
        print_exprsp eps;
        Printf.printf ")";
        )

and print_cmd c =
  match c with
      ASTStat(s) -> print_stat s
      | ASTDef(d) -> print_def d
      
	
and print_cmds cs =
  match cs with
      c::[] -> print_cmd c
    | c:: cbis -> 
      print_cmd c ; 
      Printf.printf ",";
      print_cmds cbis;
    | _ -> failwith "not yet implemented 2"

and print_type t=
  match t with 
  ASTTypBool -> Printf.printf "bool";
  | ASTTypInt -> Printf.printf "int";
  | ASTTypVoid ->  Printf.printf "void";
  | ASTTypFleche(typs,typ) -> (
      Printf.printf "[";
      print_list print_type typs ","; 
      Printf.printf "]";
      Printf.printf ",";
      print_type typ;
    )

and print_arg arg = 
  match arg with
    ASTArg(nom, typ) -> (
      Printf.printf "(";
      Printf.printf "%s" nom;
      Printf.printf ",";
      print_type typ;
      Printf.printf ")"
    )


and print_argp ap= 
  match ap with
    ASTArgp(nom, typ) -> (
      Printf.printf "(";
      Printf.printf "%s" nom;
      Printf.printf ",";
      print_type typ;
      Printf.printf ")"
      )
    | ASTArgpVar(nom,typ) -> (
      Printf.printf "(";
      Printf.printf "%s" nom;
      Printf.printf ",";
      print_type typ;
      Printf.printf ")"
      )



and print_def d = 
  match d with
    ASTDefConst(nom, ty, e) -> (
      Printf.printf "const";
      Printf.printf "(";
      Printf.printf "%s" nom;
      Printf.printf ",";
      print_type ty;
      Printf.printf ",";
      print_expr e;
      Printf.printf ")";
    )
  | ASTDefFun(nom, ty, args, e) -> (
      Printf.printf "fun";
      Printf.printf "(";
      Printf.printf "%s" nom;
      Printf.printf ",";
      print_type ty;
      Printf.printf ",";
      Printf.printf "[";
      print_list print_arg args ",";
      Printf.printf "]";
      Printf.printf ",";
      print_expr e;
      Printf.printf ")";
    )
  | ASTDefFunRec(nom, ty, args, e) -> (
      Printf.printf "funrec";
      Printf.printf "(";
      Printf.printf "%s" nom;
      Printf.printf ",";
      print_type ty;
      Printf.printf ",";
      Printf.printf "[";
      print_list print_arg args ",";
      Printf.printf "]";
      Printf.printf ",";
      print_expr e;
      Printf.printf ")";
    )
  | ASTDefVar(nom,ty)->(
      Printf.printf "var";
      Printf.printf "(";
      Printf.printf "%s" nom;
      Printf.printf ",";
      print_type ty;
      Printf.printf ")";
    )
  | ASTDefProc(x,argsp,bk)-> (
      Printf.printf "proc";
      Printf.printf "(";
      Printf.printf "%s" x;
      Printf.printf ",";
      Printf.printf "[";
      print_list print_argp argsp ",";
      Printf.printf "]";
      Printf.printf ",";
      print_block bk;
      Printf.printf ")";
    )
  | ASTDefProcRec(x,argsp,bk)-> (
    Printf.printf "procrec";
    Printf.printf "(";
    Printf.printf "%s" x;
    Printf.printf ",";
    Printf.printf "[";
    print_list print_argp argsp ",";
    Printf.printf "]";
    Printf.printf ",";
    print_block bk;
    Printf.printf ")";
    )

and print_block cs =
  Printf.printf("block([");
  print_cmds cs;
  Printf.printf("])")

and print_prog p =
  Printf.printf("prog(");
  print_block p;
  Printf.printf(")")
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
      
