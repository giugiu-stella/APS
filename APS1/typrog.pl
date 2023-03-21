/* main */
main:- read(user_input,AST),check(AST).
check(AST):-g0(G),type_expr(G,AST,T),write("ok!").
check(AST):-write("nop").
/*type_prog(AST,void).*/

/* prog */
type_prog(prog(X),void):- g0(G),type_cmds(G,X,void).

/* cmds -> defs */
type_cmds(G,[D|X],void):-type_def(G,D,G2),type_cmds(G2,X,void).

/* cmds -> end */
type_cmds(G,stat(S),void):-type_stat(G,S,void).

/* echo */
type_stat(G,echo(E),void):-type_expr(G,E,int).

/* def -> const */
type_def(G,const(X,T,E),[(X,T)|G]):-type_expr(G,E,T).
/* def -> fun */
type_def(G,fun(X,T,ListArg,E),[X,fleche((ListT,T)|G)]):-arg_type(ListArg,ListT),env_extend(G,ListArg,G2),type_expr(G2,E,T).
/* def -> fun rec */
type_def(G,funrec(X,T,ListArg,E),[X,fleche((ListT,T)|G)]):-arg_type(ListArg,ListT),env_extend(G,ListArg,G2),type_expr([(X,fleche(ListT,T))|G2],E,T).

/* contexte initial */
g0([(true,bool),(false,bool),(not,fleche([bool],bool)),(eq,fleche([int,int],bool)),
(lt,fleche([int,int],bool)),(add,fleche([int,int],int)),(sub,fleche([int,int],int)),
(mul,fleche([int,int],int)),(div,fleche([int,int],int))]).

/* expr -> num */
type_expr(G,N,int):-integer(N).
/* expr -> id */
type_expr([(X,T)|G],id(X),T).
type_expr([(X1,T1)|G],id(X),T):-type_expr(G,id(X),T).
/* expr -> if */
type_expr(G,if(A,B,C),T):-type_expr(G,A,bool),type_expr(G,B,T),type_expr(G,C,T).
/* expr -> or */
type_expr(G,or(A,B),bool):-type_expr(G,A,bool),type_expr(G,B,bool).
/* expr -> and */
type_expr(G,and(A,B),bool):-type_expr(G,A,bool),type_expr(G,B,bool).
/* expr -> abs */
type_expr(G,abs(ListArg,E),fleche(ListT,T2)):-arg_type(ListArg,ListT),env_extend(G,ListArg,G2),type_expr(G2,E,T2).
env_extend(G,[],G).
env_extend(G,[DL|RL],Gnew):-env_extend([DL|G],RL,Gnew).
arg_type([],ListT).
arg_type([(X,T)|RL],ListT):-arg_type(RL,[T|ListT]).
/* expr -> app */
type_expr(G,app(E,ListE),T):-type_expr(G,E,fleche(ListT,T)),type_exprlist(G,ListE,ListT).
type_exprlist(G,[],[]).
type_exprlist(G,[L1|RL],[T1|RT]):-type_expr(G,L1,T1),type_exprlist(G,RL,RT).
