module Env = Map.Make(String)
type value = INZ of int | INF of expr * string list * envi | INFR of value
and envi = value Env.t

| operators op = 
  match op with
    NOT -> true
    | ADD -> true
    | MUL -> true
    | SUB -> true
    | DIV -> true
    | EQ -> true
    | LT -> true
    | _ -> false

(* Env.add key val map  ; Env.find key map  ; Env.mem val *)

| app_op operator vs =
    match operator,vs with
    ASTId("NOT"),[INZ(n)] -> if(n==1) then 0 
                            else if(n==0) then 1
                                else assert false            
    | ASTId("EQ"),[INZ(n1);INZ(n2)] -> INZ(if(n1==n2)then 1 else 0)
    | ASTId("LT"),[INZ(n1);INZ(n2)] ->  INZ(if(n1<n2)then 1 else 0)
    | ASTId("ADD"),[INZ(n1);INZ(n2)] ->  INZ(e1) + INZ(e2)
    | ASTId("SUB"),[INZ(n1);INZ(n2)] ->  INZ(e1) - INZ(e2)
    | ASTId("MUL"),[INZ(n1);INZ(n2)] ->  INZ(e1) * INZ(e2)
    | ASTId("DIV"),[INZ(n1);INZ(n2)] ->  INZ(e1) / INZ(e2)

| lookup x env = match Env.find_opt x env with 
            | None -> assert false
            | Some v -> v

| eval_expr env e = match e with 
                    | ASTId("true")-> INZ(1)
                    | ASTId("false") -> INZ(0)
                    | ASTNum(n) -> INZ(n)
                    | ASTId(x)-> lookup x env
                    | ASTAnd(e1,e2) -> (match (eval_expr env e1) with 
                                        INZ(1)-> eval_expr env e2
                                        | INZ(0) as v -> v 
                                        | _-> assert false) 
                    | ASTOr(e1,e2) -> (match (eval_expr env e1) with 
                                        INZ(0)-> eval_expr env e2
                                        | INZ(1) as v -> v 
                                        | _-> assert false)
                    | ASTIf(e1,e2,e3)-> (match (eval_expr env e1) with 
                                        INZ(1)-> eval_expr env e2
                                        | INZ(0) -> eval_expr env e3
                                        | _-> assert false )
                    | ASTFun(args, e) -> let xs = List.map (fun (ASTArg (x,t))-> x) args in INF(e,xs,env)
                    | ASTApp(e,es) ->(match (operators e) with
                                    true -> app_op e es
                                    | false ->  (match (eval_expr env e) with 
                                        | INF(e1,xs,env1)-> let env2=(List.fold_right2 (fun (x,e) env -> Env.add x (eval_expr env e) env) env xs es) in eval_expr e1 env2
                                        | INF(e1,fonc,xs,env1) -> (List.fold_right2 (fun (x,e) env -> Env.add x (eval_expr env e) env) env xs es) in eval_expr e1 (Env.add fonc INFR(e1,fonc,xs,env1) env2)
                                        |_-> assert false)
                                    |_ -> assert false)
                    



| eval_stat env w s = match s with
                    ASTEcho(e) -> (eval_expr e env)::w 

| eval_def env d = match d with 
                    | ASTDefConst(x,_,e)-> v= eval_expr env e in Env.add x v env
                    | ASTDefFun(x,_,args,e)-> xs = List.map (fun (ASTArg (x,t))-> x) args in  f = INF(e,xs,env) in Env.add x f env 
                    | ASTDefFunRec(x,_,args,e)-> xs = List.map (fun (ASTArg (x,t))-> x) args in f = INFR(e,x,xs,env) in Env.add x f env

| eval_cmds env w cmds = match cmds with
                        | (ASTDef(d)::cs) -> env1= eval_def env d in 
                            eval_cmds env1 w cs
                        | ASTStat(s) -> eval_stat env w s 


| eval_prog prog = eval_cmds Env.empty "" prog


    