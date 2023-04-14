
open Ast

type value = INZ of int | INF of expr * string list * (string * value) list | INFR of expr * string * string list * (string * value) list |
             INA of int | INP of block * string list * (string * value) list | INPR of value



let lookup x env =
  try List.assoc x env with 
  Not_found -> failwith "lookup: variable not found in environment"


(*renvoie la valeur de la variable stocké à l'adresse a dans la memoire m*)
let rec get_mem a m =
    match m with
        | (a1, v1)::cs -> if(a == a1) then v1 else get_mem a cs
        | _ -> assert false

(*indice du tableau qui représente la mémoire ou on doit écrire la nouvelle valeur*)
let index_mem = ref 0

(*
    case mémoire -> (adresse, valeur)
    la variable est égale à -1 si aucune valeur ne lui a été affecté
*)
let alloc m =
   let i = (!index_mem, ref(INZ(-1))) in
        index_mem := (!index_mem + 1);
        i::m

let eval_ops op e1 e2 =
    match op with
    | Eq -> let INZ(n1)=e1 in let INZ(n2)=e2 in if(n1==n2)then INZ(1) 
                            else INZ(0)
    | Lt ->let INZ(n1)=e1 in let INZ(n2)=e2 in if(n1<n2)then INZ(1) else INZ(0)
    | Add ->  let INZ(n1)=e1 in let INZ(n2)=e2 in INZ(n1 + n2)
    | Sub -> let INZ(n1)=e1 in let INZ(n2)=e2 in INZ(n1 - n2)
    | Mul -> let INZ(n1)=e1 in let INZ(n2)=e2 in INZ(n1 * n2)
    | Div -> let INZ(n1)=e1 in let INZ(n2)=e2 in INZ(n1 / n2)

let rec eval_expr env e m = match e with
                    | ASTBool(true)-> INZ(1)
                    | ASTBool(false) -> INZ(0)
                    | ASTNum(n) -> INZ(n)
                    | ASTId(x)-> (match (lookup x env) with
                                INA(a)-> get_mem a m 
                                | v -> v)
                    | ASTAnd(e1,e2) -> (match (eval_expr env e1 m) with
                                        INZ(1)-> eval_expr env e2 m
                                        | INZ(0) as v -> v 
                                        | _-> assert false) 
                    | ASTOr(e1,e2) -> (match (eval_expr env e1 m) with
                                        INZ(0)-> eval_expr env e2 m
                                        | INZ(1) as v -> v 
                                        | _-> assert false)
                    | ASTIf(e1,e2,e3)-> (match (eval_expr env e1 m) with
                                        INZ(1)-> eval_expr env e2 m
                                        | INZ(0) -> eval_expr env e3 m
                                        | _-> assert false)
                    |ASTFun(args, e) ->let xs = List.map (fun (ASTArg(x,t)) -> x) args in
                                        INF(e,xs,env) 
                    | ASTApp(e,es) ->((match (eval_expr env e m) with
                                        INF(e1, xs, env1) ->
                                            let env2 = List.fold_right2
                                              (fun x elem env_acc -> env_acc@[x,(eval_expr env elem m)])
                                              xs es env1 in
                                            eval_expr env2 e1 m
                                        | INFR(e1,fonc,xs,env1) -> 
                                            let env2 = (List.fold_right2 (fun x elem env_acc -> env_acc@[x,(eval_expr env elem m)]) xs es env1) in 
                                            eval_expr (env2@[fonc,INFR(e1,fonc,xs,env1)]) e1 m)                                        
                                    )
                    | ASTNot(el)-> let n = eval_expr env el m in if(n==INZ(1)) then INZ(0) 
                                        else if(n==INZ(0)) then INZ(1)
                                        else failwith "error not"

                    |ASTBinary(op,e1,e2) -> let elem1 = eval_expr env e1 m in
                                            let elem2 = eval_expr env e2 m
                                    in eval_ops op elem1 elem2

and eval_expar env m e = match e with 
                        ASTExprpAdr(x)-> eval_expr env x m
                        | ASTExprpExpr(v) -> eval_expr env v m
                        |_ -> failwith "error expar"

and eval_stat env w s m = match s with
                    | ASTEcho(e) -> (match (eval_expr env e m) with 
                                    INZ(n)-> (m,n::w) 
                                    |_ -> failwith "error echo pas INZ")
                    | ASTSet(x,e) -> (
                        match (lookup x env) with
                            | INA(a) -> 
                                let v = (get_mem a m) and res = eval_expr env e m
                                in v := res;
                                (m,w)
                            | _ -> failwith ("address not in memory")
                        )
                    | ASTIfStat(e, b1, b2) -> (match (eval_expr env e m) with
                        | INZ(1) -> eval_block env w b1 m
                        | INZ(0) -> eval_block env w b2 m
                        |_-> assert false)
                    | ASTWhile(e, b) -> (match (eval_expr env e m) with
                        | INZ(1) -> let (env1, m1) = (eval_block env w b m) in
                            eval_stat env1 w s m1
                        | INZ(0) -> (env, m))
                    | ASTCall(x, exprs) -> match (eval_expr env x m) with
                                        INP(bk,xs,env1) -> let env2=(List.fold_right2 (fun (x,e) env -> Env.add x (eval_expar env m e) env) env xs es) in eval_block env2 w bk m
                                        | INPR(bk,x,xs,env1) ->  let env2=(List.fold_right2 (fun (x,e) env -> Env.add x (eval_expar env e m) env) env xs es) in eval_block (Env.add fonc INFR(e1,fonc,xs,env1) env2) w bk m


and eval_def env d m = match d with
                    | ASTDefConst(x,_,e)-> let v = eval_expr env e m
                        in (Env.add x v env, m)
                    | ASTDefFun(x,_,args,e)-> let xs =
                        List.map (fun (ASTArg (x,t))-> x) args in
                        let f = INF(e,xs,env) in (Env.add x f env, m)
                    | ASTDefFunRec(x,_,args,e)-> let xs =
                        List.map (fun (ASTArg (x,t))-> x) args in
                        let f = INFR(e,x,xs,env) in (Env.add x f env, m)
                    | ASTDefVar(x,_) -> let (a, m1) = alloc(m)
                        in (Env.add x INA(a) env, m1)
                    | ASTDefProc(x,_,args, b) -> let xs  =
                        List.map (fun (ASTArg (x,t))-> x) args in
                        let f = INP(b, xs, env) in (Env.add x f env, m)
                    | ASTDefProcRec(x,_,args, b) -> let xs  =
                        List.map (fun (ASTArg (x,t))-> x) args in
                        let f = INPR(b, xs, env) in (Env.add x f env, m)


and eval_cmds env w cmds m = match cmds with
                        | (ASTDef(d)::cs) -> let (env1, m1) = eval_def env d m in
                            eval_cmds env1 w cs m1
                        | (ASTStat(s)::cs) -> let (env1, m1) = eval_stat env w s m in
                            eval_cmds env1 w cs m1
                        | ASTStat(s) -> eval_stat env w s m

and eval_block env w block m = eval_cmds env w block m

and eval_prog prog = eval_block Env.empty "" prog []


    

