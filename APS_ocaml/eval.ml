module Env = Map.Make(String)
type value = INZ of int | INF of expr * string list * envi | INFR of value
and envi = value Env.t

(* Env.add key val map  ; Env.find key map  ; Env.mem val *)

let integer i = 
    match i with
    INZ(i) -> true
    | _ -> false

let eval_unary operator e1 = 
    match operator with
    Not -> if (integer(e1) && e1==0) then 1
        else if (integer(e1) && e1==1) then 0
            else "ERROR PAS LES BONS ARGUMEMENTS !"

let eval_binary operator e1 e2 =
    match operator with
    EQ -> if (integer(e1) && integer(e2) && e1==e2) then 1 
        else 0
    | LT -> if (integer(e1) && integer(e2) && e1<e2) then 1
            else 0
    | ADD -> (integer(e1) && integer(e2)) then INZ(e1) + INZ(e2)
    | SUB ->  (integer(e1) && integer(e2)) then INZ(e1) - INZ(e2)
    | MUL ->  (integer(e1) && integer(e2)) then INZ(e1) * INZ(e2)
    | DIV ->  (integer(e1) && integer(e2)) then INZ(e1) / INZ(e2)
    | AND ->  (integer(e1) && integer(e2) && INZ(e1)==INZ(1)) then INZ(e2)

let boolto i = 
    match i with
    INZ(i) -> true
    | _ -> false

let app_op operator vs =
    match operator,vs with
    | EQ,INZ(n1),INZ(n2) -> INZ(if(n1==n2)then )


let lookup x env = macth Env.find_opt x env with 
            | None -> assert false
            | Some v -> v

let eval_expr env e = match e with 
                    | ASTId("true")-> INZ(1)
                    | ASTId("false") -> INZ(0)
                    | ASTNum(n) -> INZ(n)
                    | ASTId(x)-> lookup x env
                    | 



let eval_stat env w s = match s with
                    ASTEcho(e) -> (eval_expr e env)::w 

let eval_def env d = match d with 
                    | ASTDefConst(x,_,e)-> let v= eval_expr env e in Env.add x v env
                    | ASTDefFun(x,_,args,e)-> let xs = List.map (fun (ASTArg (x,t))-> x) args in let f = INF(e,xs,env) in Env.add x f env 
                    | ASTDefFunRec(x,_,args,e)-> let xs = List.map (fun (ASTArg (x,t))-> x) args in let f = INFR(e,x,xs,env) in Env.add x f env

let eval_cmds env w cmds = match cmds with
                        | (ASTDef(d)::cs) -> let env1= eval_def env d in 
                            eval_cmds env1 w cs
                        | ASTStat(s) -> eval_stat env w s 


let eval_prog prog = eval_cmds Env.empty "" prog


    