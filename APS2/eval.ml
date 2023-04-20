open Ast
type value = INZ of int | INF of expr * string list * (string * value) list | INFR of expr * string * string list * (string * value) list |
             INA of int | INP of block * string list * (string * value) list | INPR of block * string * string list * (string * value) list |
             INB of int * int



let lookup x env =
  try List.assoc x env with 
  Not_found -> failwith "pas dans l'environnement\n"

let rec get_mem a  (m: (int * (value ref)) list) =
    (match m with
        | (a1,v1)::cs ->if(a == a1) then v1 else get_mem a cs
        | _ -> assert false)

let rec get_m a  (m: (int * value) list) =
    (match m with
        | (a1,v1)::cs ->if(a == a1) then v1 else get_m a cs
        | _ -> assert false)

let index_mem = ref 0

let alloc m =
    let i = (!index_mem, (!index_mem, ref(INZ(-1)))::m ) in
    index_mem := (!index_mem + 1);
    i

let allocTab m size =
    if (size <= 0) then failwith "Taille impossible !\n"
    else
        let address = !index_mem in
            let rec ajout_mem new_mem n =
            match n with
                0 -> new_mem
            | _ -> (
                let tmp = !index_mem in
                index_mem := (!index_mem + 1); 
                    ajout_mem ((tmp, ref(INZ(-1)))::new_mem) (n - 1)
                ) in
            (address, (ajout_mem m size))
      
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
                    | ASTBool(true)-> (INZ(1),m)
                    | ASTBool(false) -> (INZ(0),m)
                    | ASTNum(n) -> (INZ(n),m)
                    | ASTId(x)-> (Printf.printf "id\n"; match (lookup x env) with
                                INA(a)-> (!(get_mem a m),m)
                                | v -> (v,m))
                    | ASTIf(e1,e2,e3)-> (match (eval_expr env e1 m) with
                                        (INZ(1),m1)-> eval_expr env e2 m1
                                        | (INZ(0),m1) -> eval_expr env e3 m1)
                    |ASTFun(args, e) ->let xs = List.map (fun (ASTArg(x,t)) -> x) args in
                                        (INF(e,xs,env),m)
                    | ASTApp(e,es) ->Printf.printf "app\n"; ((match (eval_expr env e m) with
                                        (INF(e1, xs, env1),m1) ->
                                            let env2 = List.fold_right2
                                              (fun x elem env_acc -> let (res,_)=eval_expr env elem m1 in env_acc@[x,res])
                                              xs es env1 in
                                            eval_expr env2 e1 m1
                                        | (INFR(e1,fonc,xs,env1),m1) -> 
                                            let env2 = (List.fold_right2 (fun x elem env_acc -> let (res,_)=eval_expr env elem m1 in env_acc@[x,res]) xs es env1) in 
                                            eval_expr (env2@[fonc,INFR(e1,fonc,xs,env1)]) e1 m1)                                        
                                    )
                    | ASTNot(el)-> let (n,m1) = eval_expr env el m in if(n=INZ(1)) then (INZ(0),m1) 
                                        else if(n=INZ(0)) then (INZ(1),m1)
                                        else failwith "error not"

                    |ASTBinary(op,e1,e2) -> let (elem1,m1) = eval_expr env e1 m in
                                            let (elem2,m2) = eval_expr env e2 m1
                                    in (eval_ops op elem1 elem2,m2)
                        
                    | ASTAlloc(e) -> (match (eval_expr env e m) with
                                    (INZ(n),m1)-> if(n>0) then 
                                        let (a, m2) = allocTab m n
                                                                in (INB(a, n),m2)
                                                    else failwith "error alloc, n'est négatif\n"
                                    |_->failwith "error alloc\n")

                    | ASTLen(e1) -> (match (eval_expr env e1 m) with
                                   (INB(a,n),m1)-> (INZ(n),m1)
                                   |_->failwith "error len\n")

                    | ASTnth(vec,n)-> (let (v, mem1) = (eval_expr env vec m) in
                                        let (i, mem2) = (eval_expr env n mem1) in
                                            match i with
                                            INZ(x) -> ( match v with
                                                        INB(a, size) -> ( if x < size then (!(get_mem (a+x) mem2), mem2) else failwith "Evaluation of nth: index out of range")
                                                        | v -> (failwith "Evaluation of nth: not a vec"))
                                                                
                                            | v -> failwith "Evaluation of nth: not an integer")

                    | ASTvset(vec,n,p)-> (let (v, mem1) = (eval_expr env vec m) in
                                            let (i, mem2) = (eval_expr env n mem1) in
                                            let (j,mem3) = (eval_expr env p mem2) in 
                                                match i with
                                                INZ(x) -> ( match v with
                                                            INB(a, size) -> ( if x < size then let INZ(k)= j in
                                                            let INZ(ent)= !(get_mem (a+x) mem2) in 
                                                                (INB(a, size),(ent, ref(INZ(k)))::mem3) 
                                                        else failwith "Evaluation of vset: index out of range")
                                                            | v -> (failwith "Evaluation of vset: not a vec"))
                                                                    
                                                | v -> failwith "Evaluation of vset: not an integer")
                        

and eval_expar env m e = match e with 
                        ASTExprpAdr(ASTId(x))->Printf.printf "expar adr \n"; (lookup x env,m)
                        | ASTExprpExpr(v) -> Printf.printf "expar v\n"; eval_expr env v m
                        |_ -> failwith "error expar"

and eval_stat env w s m = match s with
                    | ASTEcho(e) -> (match (eval_expr env e m) with 
                                    (INZ(n),m1)-> (m1,n::w) 
                                    |_ -> failwith "error echo pas INZ")
                    | ASTSet(x,e) -> (Printf.printf "set \n";
                        match (eval_lvalue env w x m) with
                            | (a,m1) -> 
                                let v= (get_mem a m1) and (res,m2) = eval_expr env e m
                                in v := res;
                                (m2,w)
                            | _ -> failwith ("address not in memory")
                        ) 
                    | ASTIfStat(e, b1, b2) -> Printf.printf "set\n"; (match (eval_expr env e m) with
                        | (INZ(1),m1) -> eval_block env w b1 m1
                        | (INZ(0),m1) -> eval_block env w b2 m1
                        |_-> assert false)
                    | ASTWhile(e, b) -> (match (eval_expr env e m) with
                        | (INZ(1),mbis) -> let (m1, w1) = (eval_block env w b mbis) in
                            eval_stat env w1 (ASTWhile(e, b)) m1
                        |(INZ(0),mbis) -> (mbis, w))
                    | ASTCall(x, exprs) -> Printf.printf " call\n";
                                         match (eval_expr env x m) with
                                        (INP(bk,xs,env1),m1) -> let env2=(List.fold_right2 
                                        (fun x e env -> let (res,_)=eval_expar env m1 e in env@[x,res]) 
                                        xs exprs env) 
                                        in eval_block env2 w bk m1
                                        | (INPR(bk,x,xs,env1),m1) -> Printf.printf "call INPR\n";  let env2=(List.fold_right2 (fun x e env -> let (res,_)=eval_expar env m1 e in [x,res]@env) xs exprs env) 
                                    in eval_block (env2@[x,INPR(bk,x,xs,env1)]) w bk m1

and eval_lvalue env w lv m = match lv with
                            ASTvalueId(x) -> (match (eval_expr env (ASTId(x)) m) with
                                            (INA(a),m1) -> (a,m1)
                                            |_->failwith "error lvalue id\n")
                           | ASTValue(valeur,e) -> (match valeur with
                                                    ASTvalueId(x) -> (match (eval_expr env (ASTId(x)) m) with
                                                                    (INB(a1,n1),m1) -> let (INZ(i),m2)= eval_expr env e m1 in (a1+i,m2)
                                                                    |_-> failwith "error lvalue value\n")
                                                    | ASTValue(x1,e1) -> (let (a1,m1)= eval_lvalue env w x1 m in 
                                                                         let (e2,m2) = eval_expr env e m1
                                                                            in (match e2 with
                                                                                INZ(x2) -> (match !(get_mem a1 m1) with
                                                                                            INB(a2,n) -> (a2+x2,m2)
                                                                                            | _ ->failwith "error lvalue value\n")
                                                                                |_->failwith "error lvalue value\n"))
                
                                                    |_-> failwith "error lvalue value\n")


and eval_def env d m = match d with
                    | ASTDefConst(x,_,e)-> let (v,m2)=eval_expr env e m in (env@[x,v],m2)
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


    

