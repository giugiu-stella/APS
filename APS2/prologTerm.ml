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
    | ASTnth(e1, e2) -> (
      Printf.printf "nth(";
      print_expr e1;
      Printf.printf ",";
      print_expr e2;
      Printf.printf ")";
    )
    | ASTLen(e) -> (
      Printf.printf "len(";
      print_expr e;
      Printf.printf ")";
    )
    | ASTAlloc(e) -> (
      Printf.printf "alloc(";
      print_expr e;
      Printf.printf ")";
    )
    | ASTvset(e1, e2, e3) -> (
      Printf.printf "vset(";
      print_expr e1;
      Printf.printf ",";
      print_expr e2;
      Printf.printf ",";
      print_expr e3;
      Printf.printf ")";    
      )
    | ASTBinary(op, e1, e2) ->(
      Printf.printf "app(id(";
      Printf.printf "%s" (op_to_string op);
      Printf.printf "),";
      Printf.printf "[";
      print_expr e1;
      Printf.printf ",";
      print_expr e2;
      Printf.printf "]";
      Printf.printf ")";
    )
    | ASTNot(e) -> (
        Printf.printf "app(" ;
        Printf.printf "id(not), [";
        print_expr e;
        Printf.printf "])";
      )

and print_exprp ep = 
  match ep with
    ASTExprpExpr(e) -> print_expr e;
    | ASTExprpAdr(x)->(
      Printf.printf "adr";
      Printf.printf "(";
      print_expr x;
      Printf.printf ")";
    )

 and op_to_string op = 
    match op with
      Add -> "add"
    | Mul -> "mul"
    | Sub -> "sub"
    | Div -> "div"
    | Eq -> "eq"
    | Lt -> "lt"
    | And -> "and"
    | Or -> "or"

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
        print_value x;
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
        print_expr x;
        Printf.printf ",";
        Printf.printf"[";
        print_list print_exprp eps ",";
        Printf.printf"]";
        Printf.printf ")";
        )

and print_cmd c =
  match c with
      ASTStat(s) -> 
        Printf.printf "stat(";
        print_stat s;
        Printf.printf ")";
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
      Printf.printf "fleche(";
      Printf.printf "[";
      print_list print_type typs ","; 
      Printf.printf "]";
      Printf.printf ",";
      print_type typ;
      Printf.printf ")";
    )
  | ASTref(typ)-> 
      Printf.printf "ref(";
      print_type typ;
      Printf.printf ")";
  | ASTTTypVec(typ) -> (
      Printf.printf "vec(";
      print_type typ;
      Printf.printf ")";
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
      Printf.printf "var";
      Printf.printf "(";
      Printf.printf "%s" nom;
      Printf.printf ",";
      print_type typ;
      Printf.printf ")")



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

and print_value v =
    match v with 
        ASTvalueId(id) -> Printf.printf "ident(%s)" id;
      | ASTValue(id, e) -> 
        (Printf.printf "nth(";
        print_value id;
        Printf.printf ",";
        print_expr e;
        Printf.printf ")";
        )
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
      
