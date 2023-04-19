
open Ast
type value = INZ of int | INF of expr * string list * (string * value) list | INFR of expr * string * string list * (string * value) list |
             INA of int | INP of block * string list * (string * value) list | INPR of block * string * string list * (string * value) list 



let lookup x env =
  try List.assoc x env with 
  Not_found -> failwith "lookup: variable not found in environment"


(*renvoie la valeur de la variable stocké à l'adresse a dans la memoire m*)
let rec get_mem a  (m: (int * (value ref)) list) =
    match m with
        | (a1,v1)::cs ->if(a == a1) then v1 else get_mem a cs
        | _ -> assert false

(*indice du tableau qui représente la mémoire ou on doit écrire la nouvelle valeur*)
let index_mem = ref 0

(*
    case mémoire -> (adresse, valeur)
    la variable est égale à -1 si aucune valeur ne lui a été affecté
*)
let alloc m =
    let i = (!index_mem, (!index_mem, ref(INZ(-1)))::m ) in
    index_mem := (!index_mem + 1);
    i


let eval_ops op e1 e2 =
    match op with
    | Eq -> let INZ(n1)=e1 in let INZ(n2)=e2 in if(n1=n2)then INZ(1) 
                            else INZ(0)
    | Lt ->let INZ(n1)=e1 in let INZ(n2)=e2 in if(n1<n2)then INZ(1) else INZ(0)
    | Add ->  let INZ(n1)=e1 in let INZ(n2)=e2 in INZ(n1 + n2)
    | Sub -> let INZ(n1)=e1 in let INZ(n2)=e2 in INZ(n1 - n2)
    | Mul -> let INZ(n1)=e1 in let INZ(n2)=e2 in INZ(n1 * n2)
    | Div -> let INZ(n1)=e1 in let INZ(n2)=e2 in INZ(n1 / n2)
    | Or -> let INZ(n1)=e1 in let INZ(n2)=e2 in if(n1+n2 >=1) then INZ(1) else INZ(0)
    | And -> let INZ(n1)=e1 in let INZ(n2)=e2 in if(n1+n2=2) then INZ(1) else INZ(0)

let rec eval_expr env e m = match e with
                    | ASTBool(true)-> INZ(1)
                    | ASTBool(false) -> INZ(0)
                    | ASTNum(n) -> INZ(n)
                    | ASTId(x)-> (Printf.printf "id\n"; match (lookup x env) with
                                INA(a)-> !(get_mem a m)
                                | v -> v)
                    | ASTIf(e1,e2,e3)-> (match (eval_expr env e1 m) with
                                        INZ(1)-> eval_expr env e2 m
                                        | INZ(0) -> eval_expr env e3 m)
                    |ASTFun(args, e) ->let xs = List.map (fun (ASTArg(x,t)) -> x) args in
                                        INF(e,xs,env) 
                    | ASTApp(e,es) ->Printf.printf "app\n"; ((match (eval_expr env e m) with
                                        INF(e1, xs, env1) ->
                                            let env2 = List.fold_right2
                                              (fun x elem env_acc -> env_acc@[x,(eval_expr env elem m)])
                                              xs es env1 in
                                            eval_expr env2 e1 m
                                        | INFR(e1,fonc,xs,env1) -> 
                                            let env2 = (List.fold_right2 (fun x elem env_acc -> env_acc@[x,(eval_expr env elem m)]) xs es env1) in 
                                            eval_expr (env2@[fonc,INFR(e1,fonc,xs,env1)]) e1 m)                                        
                                    )
                    | ASTNot(el)-> let n = eval_expr env el m in if(n=INZ(1)) then INZ(0) 
                                        else if(n=INZ(0)) then INZ(1)
                                        else failwith "error not"

                    |ASTBinary(op,e1,e2) -> let elem1 = eval_expr env e1 m in
                                            let elem2 = eval_expr env e2 m
                                    in eval_ops op elem1 elem2

and eval_expar env m e = match e with 
                        ASTExprpAdr(ASTId(x))->Printf.printf "expar adr \n"; lookup x env
                        | ASTExprpExpr(v) -> Printf.printf "expar v\n"; eval_expr env v m
                        |_ -> failwith "error expar"

and eval_stat env w s m = match s with
                    | ASTEcho(e) -> (match (eval_expr env e m) with 
                                    INZ(n)-> (m,n::w) 
                                    |_ -> failwith "error echo pas INZ")
                    | ASTSet(x,e) -> (Printf.printf "set \n";
                        match (lookup x env) with
                            | INA(a) -> 
                                let v = (get_mem a m) and res = eval_expr env e m
                                in v := res;
                                (m,w)
                            | _ -> failwith ("address not in memory")
                        ) 
                    | ASTIfStat(e, b1, b2) -> Printf.printf "set\n"; (match (eval_expr env e m) with
                        | INZ(1) -> eval_block env w b1 m
                        | INZ(0) -> eval_block env w b2 m
                        |_-> assert false)
                    | ASTWhile(e, b) -> (match (eval_expr env e m) with
                        | INZ(1) -> let (m1, w1) = (eval_block env w b m) in
                            eval_stat env w1 (ASTWhile(e, b)) m1
                        | INZ(0) -> (m, w))
                    | ASTCall(x, exprs) -> Printf.printf " call\n";
                                         match (eval_expr env x m) with
                                        INP(bk,xs,env1) -> let env2=(List.fold_right2 
                                        (fun x e env -> env@[x,(eval_expar env m e)]) 
                                        xs exprs env) 
                                        in eval_block env2 w bk m
                                        | INPR(bk,x,xs,env1) -> Printf.printf "call INPR\n";  let env2=(List.fold_right2 (fun x e env -> [x,(eval_expar env m e)]@env) xs exprs env) 
                                    in eval_block (env2@[x,INPR(bk,x,xs,env1)]) w bk m

and eval_def env d m = match d with
                    | ASTDefConst(x,_,e)-> (env@[x,(eval_expr env e m)],m)
                    | ASTDefFun(x,_,args,e)-> Printf.printf "fun\n"; let xs =
                        List.map (fun (ASTArg (x,t))-> x) args in
                        let f = INF(e,xs,env) in (env@[x,f],m)
                    | ASTDefFunRec(x,_,args,e)-> let xs =
                        List.map (fun (ASTArg (x,t))-> x) args in
                        let f = INFR(e,x,xs,env) in (env@[x,f],m)
                    | ASTDefVar(x,_) -> Printf.printf "var\n"; let(a,m1) = alloc(m)
                        in (env@[x,INA(a)],m1)
                    | ASTDefProc(x,args, b) -> let xs  =
                        List.map (fun argp -> match argp with
                            ASTArgp (x,t)-> x
                            |ASTArgpVar(x,t)-> x) 
                            args in
                        let f = INP(b, xs, env) in (env@[x,f],m)
                    | ASTDefProcRec(x,args, b) -> Printf.printf "proc rec\n";let xs  =
                        List.map (fun argp -> match argp with
                                            ASTArgp (x,t)-> x
                                            |ASTArgpVar(x,t)-> x) args in
                        let f = INPR(b,x, xs, env) in (env@[x,f],m)

and eval_cmd_stat env w (cmd: Ast.stat) m = eval_stat env w cmd m

and eval_cmd_def env w (cmd: Ast.def) m = eval_def env cmd m

and eval_cmds env w (cmds: Ast.cmd list)  m =match cmds with
                        | (ASTStat(s)::cs) -> let (m1, w1) = Printf.printf "cmds_stat\n"; eval_cmd_stat env w s m in
                            eval_cmds env w1 cs m1
                        | (ASTDef(d)::cs) -> let (env1, m1) =Printf.printf "cmds_def\n";  eval_cmd_def env w d m in
                            eval_cmds env1 w cs m1
                        |[]-> (m,w)
                         

and eval_block env w block m = Printf.printf "block\n"; eval_cmds env w block m

and  print_output output =
    List.iter (function x -> Printf.printf "%d\n" x) output

and eval_prog prog = Printf.printf "prog\n"; let (_, out) = eval_block [] [] prog [] in print_output out

let _ =
	try
		let fl = open_in Sys.argv.(1) in
		let lexbuf = Lexing.from_channel fl in
		let p = Parser.prog Lexer.token lexbuf in
			(eval_prog p)
	with Lexer.Eof -> exit 0


    

