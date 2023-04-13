<<<<<<< HEAD
(* Env.add key val map  ; Env.find key map  ; Env.mem val *)

open Ast

module Env = Map.Make(String)
type value = INZ of int | INF of expr * string list * envi | INFR of value |
             INA of int | INP of block * string list * envi | INPR of value
and envi = value Env.t

(*Renvoie vrai si op est une opération unaire ou binaire*)
let operators op = 
  match op with
    ASTId("NOT") -> true
    | ASTId("ADD") -> true
    | ASTId("MUL") -> true
    | ASTId("SUB") -> true
    | ASTId("DIV") -> true
    | ASTId("EQ") -> true
    | ASTId("LT") -> true
    | _ -> false
=======
open Ast

module Env = Map.Make(
    struct 
      type t = string    
      let compare = compare 
    end     
)

type value = INZ of int | INF of expr * string list * envi | INFR of expr * string * string list * envi |
             INA of int | INP of block * string list * envi | INPR of value
and envi = value Env.t

>>>>>>> 3d36c87 (correction eval APS1a)

let lookup x env =
    match Env.find_opt x env with 
        | None -> assert false
        | Some v -> v

<<<<<<< HEAD
(*renvoie le resultat de l'opération unaire ou binaire*)
let app_op operator vs =
    match operator,vs with
        ASTId("NOT"),[INZ(n)] -> if(n==1) then INZ(0) 
                                else if(n==0) then INZ(1)
                                    else assert false            
        | ASTId("EQ"),[INZ(n1);INZ(n2)] -> INZ(if(n1==n2)then 1 else 0)
        | ASTId("LT"),[INZ(n1);INZ(n2)] ->  INZ(if(n1<n2)then 1 else 0)
        | ASTId("ADD"),[INZ(n1);INZ(n2)] ->  INZ(n1 + n2)
        | ASTId("SUB"),[INZ(n1);INZ(n2)] ->  INZ(n1 - n2)
        | ASTId("MUL"),[INZ(n1);INZ(n2)] ->  INZ(n1 * n2)
        | ASTId("DIV"),[INZ(n1);INZ(n2)] ->  INZ(n1 / n2)
        | _ -> failwith "erreur, l'opération n'est pas correct"

(*convertit l'expression en int et erreur sinon*)
let rec expr_of_int e = 
    match e with 
        INZ(n) -> n
        | _ -> failwith "erreur, l'expression n'est pas un entier" 

and eval_expr env e m = match e with
                | ASTId("true")-> INZ(1)
                | ASTId("false") -> INZ(0)
                | ASTNum(n) -> INZ(n)
                | ASTId(x)-> lookup x env
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
                                    | _-> assert false )
                | ASTFun(args, e) -> let xs = List.map (fun (ASTArg (x,t))-> x) args in INF(e,xs,env)
                | ASTApp(e,es) ->
                    (match (operators e) with
                        | true -> (match es with 
                                      [x] -> app_op e [expr_of_int env x m]
                                    | [x1; x2] -> app_op e [expr_of_int env x1 m; expr_of_int env x2 m]
                                    | _ -> failwith "erreur, l'opération n'est pas correct")

                        | false -> (match (eval_expr env e m) with
                                    | INF(e1,xs,env1) ->
                                    let env2=(List.fold_right2 (fun (x,e) env -> Env.add x (eval_expr env e m) env) env xs es) in 
                                    eval_expr e1 env2 m
                                    | INF(e1,fonc,xs,env1) ->
                                    let env2=(List.fold_right2 (fun (x,e) env -> Env.add x (eval_expr env e m) env) env xs es) in 
                                    eval_expr e1 (Env.add fonc INFR(e1,fonc,xs,env1) env2) m
                                    | _ -> assert false
                                    )
                    )

(*renvoie la valeur de la variable stocké à l'adresse a dans la memoire m*)
and get_mem a m =
=======
let app_op operator vs =
    match operator,vs with
    ASTId("NOT"),[INZ(n)] -> if(n==1) then INZ(0) 
                            else if(n==0) then INZ(1)
                                else assert false            
    | ASTId("EQ"),[INZ(n1);INZ(n2)] -> if(n1==n2)then INZ(1) else INZ(0)
    | ASTId("LT"),[INZ(n1);INZ(n2)] ->  if(n1<n2)then INZ(1) else INZ(0)
    | ASTId("ADD"),[INZ(n1);INZ(n2)] ->  INZ(n1 + n2)
    | ASTId("SUB"),[INZ(n1);INZ(n2)] ->  INZ(n1 - n2)
    | ASTId("MUL"),[INZ(n1);INZ(n2)] ->  INZ(n1 * n2)
    | ASTId("DIV"),[INZ(n1);INZ(n2)] ->  INZ(n1 / n2)
    |_-> raise Not_found

(*renvoie la valeur de la variable stocké à l'adresse a dans la memoire m*)
let rec get_mem a m =
>>>>>>> 3d36c87 (correction eval APS1a)
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
<<<<<<< HEAD
    let i = (!index_mem, ref(INZ(-1))) in
        !index_mem = (!index_mem + 1);
        i::m

let rec eval_stat env w s m = match s with
                    | ASTEcho(e) -> ((eval_expr e env m)::w, m)
                    | ASTSet(x,e) -> (match lookup x env with
                        | INA(a) -> let v = (get_mem a m) and res = (eval_expr env e m) in
                                v := res;
                                (env, m)
                        | _ -> assert false)
                    | ASTIfStat(e, b1, b2) -> (match (eval_expr env e m) with
                        | INZ(1) -> eval_block env w b1 m
                        | INZ(0) -> eval_block env w b2 m
                        | _ -> assert false)
                    | ASTWhile(e, b) -> (match (eval_expr env e m) with
                                            | INZ(1) -> let (env1, m1) = (eval_block env w b m) in
                                                eval_stat env1 w s m1
                                            | INZ(0) -> (env, m))
                    (*| ASTCall(x, exprs) -> *)



let eval_def env d m = match d with
                    | ASTDefConst(x,_,e)-> let v = eval_expr env e m in 
                                            (Env.add x v env, m)
                    | ASTDefFun(x,_,args,e)-> let xs = List.map (fun (ASTArg (x,t))-> x) args in
                                                let f = INF(e,xs,env) in 
                                                    (Env.add x f env, m)
                    | ASTDefFunRec(x,_,args,e)-> let xs = List.map (fun (ASTArg (x,t))-> x) args in
                                                    let f = INFR(e,x,xs,env) in 
                                                        (Env.add x f env, m)
                    | ASTDefVar(x, _) -> let a, m1 = alloc(m)
                                            in (Env.add x INA(a) env, m1)
                    | ASTDefProc(x, args, b) -> let xs  = List.map (fun (ASTArg (x,t))-> x) args in
                                                    let f = INP(b, args, env) in 
                                                        (Env.add x f env, m)
                    | ASTDefProcRec(x, args, b) -> let xs  = List.map (fun (ASTArg (x,t))-> x) args in
                                                                let f = INPR(INP(b, args, env)) in 
                                                                    (Env.add x f env, m)
=======
   let i = (!index_mem, ref(INZ(-1))) in
        index_mem := (!index_mem + 1);
        i::m

let lookup x env = match Env.find_opt x env with 
            | None -> assert false
            | Some v -> v


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
                    | ASTFun(args, e) -> let xs = List.map (fun (ASTArg (x,t))-> x) args in INF(e,xs,env)
                    | ASTApp(e,es) ->( let suite = List.map (fun x -> eval_expr env x m) es in 
                                     try (app_op e suite) with
                                    | Not_found -> (match (eval_expr env e m) with
                                        |INF(e1, xs, env1) ->
                                            let env2 = List.fold_right2
                                              (fun x e env_acc -> Env.add x (eval_expr env e m) env_acc)
                                              xs es env1 in
                                            eval_expr e1 env2 m
                                        | INFR(e1,fonc,xs,env1) -> 
                                            let env2 = (List.fold_right2 (fun x e env1_acc -> Env.add x e env1_acc) xs es env1) in 
                                            eval_expr e1 (Env.add fonc (INFR(e1,fonc,xs,env1)) env2) m)                                        
                                    )
                    

let eval_expar env m e = match e with 
                        adr(x) -> eval_expr env x m
                        | v -> eval_expr env v m
                        |_ -> failwith "error expar"

let rec eval_stat env w s m = match s with
                    | ASTEcho(e) -> (match (eval_expr e env m) with 
                                    (INZ(n),m2)-> (m2,n::w) 
                                    |_ -> failwith "error echo pas INZ")
                    | ASTSet(x,e) -> (match (lookup x env) with
                                    INA(a) -> let v = (get_mem a m) and res = (eval_expr env e m) in v := res;
                                            (m,w)
                                    |_-> assert false)
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


let eval_def env d m = match d with
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
>>>>>>> 3d36c87 (correction eval APS1a)


let rec eval_cmds env w cmds m = match cmds with
                        | (ASTDef(d)::cs) -> let (env1, m1) = eval_def env d m in
<<<<<<< HEAD
                                                eval_cmds env1 w cs m1
                        | (ASTStat(s)::cs) -> let (env1, m1) = eval_stat env w s m in
                                                eval_cmds env1 w cs m1
=======
                            eval_cmds env1 w cs m1
                        | (ASTStat(s)::cs) -> let (env1, m1) = eval_stat env w s m in
                            eval_cmds env1 w cs m1
>>>>>>> 3d36c87 (correction eval APS1a)
                        | ASTStat(s) -> eval_stat env w s m

let eval_block env w block m = eval_cmds env w block m

<<<<<<< HEAD
let eval_prog prog = eval_cmds Env.empty "" prog []    
=======
let eval_prog prog = eval_block Env.empty "" prog []


    
>>>>>>> 3d36c87 (correction eval APS1a)
