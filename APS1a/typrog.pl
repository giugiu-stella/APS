/* contexte initial */
g0([(true,bool),(false,bool),(not,fleche([bool],bool)),(eq,fleche([int,int],bool)),
(lt,fleche([int,int],bool)),(add,fleche([int,int],int)),(addi,fleche([int,int],int)),(sub,fleche([int,int],int)),
(mul,fleche([int,int],int)),(div,fleche([int,int],int))]).

/* main */
/*main:-read_term(AST,_),check(AST).
check(AST):-type_stat(g0,AST),write("ok!").
check(AST):-write("nop "), write("AST: "), write(AST).*/

main:-read(AST),check(AST).
check(AST):-type_prog(AST),write("ok!").
check(AST):-write("nop "), write("AST: "), write(AST).


/* prog */
type_prog(prog(X)):- write("prog\n"),g0(G),type_block(G,X,void).

/* block */
type_block(G,block(X),void):- write("block \n"),write(G),write("\n"),type_cmds(G,X,void).

/* cmds -> end */
type_cmds(G,[],void).
/* cmds -> defs */
type_cmds(G,[D|X],void):-write("cmd def \n"),type_def(G,D,G2),type_cmds(G2,X,void).
/* cmds -> stat */
type_cmds(G,[stat(S)|CS],void):-write("cmd stat \n"),write(S),write("\n"),type_stat(G,S,void),type_cmds(G,CS,void).

/* stat-> echo */
type_stat(G,echo(E),void):-write("echo-> \n"),write(G),write("\n"),type_expr(G,E,int).
/* stat -> set */
type_stat(G,set(X,E),void):-write("set \n"),write(G),type_expr(G,id(X),ref(T)),write("set etapege 2\n"),write(E),type_expr(G,E,T),write("fin set\n").
/* stat-> if */ 
type_stat(G,if(E,BK1,BK2),void):-write("if\n"),write(G),type_expr(G,E,bool),write("\n"),write("if etape2\n"),type_block(G,BK1,void),write("if etape3\n"),type_block(G,BK2,void),write("fin if\n").
/* stat -> while */
type_stat(G,while(E,BK),void):-type_expr(G,E,bool),type_block(G,BK,void).
/* stat -> call */
type_stat(G,call(X,EPS),void):-write("call \n"),write(G),write("\n"),write(X),write("\n"),type_expr(G,id(X),fleche(ListT,void)),write(" call etape2 \n"),write(EPS),write("\n"),write(ListT),write("\n"),type_exparlist(G,EPS,ListT).
type_exparlist(G,[],[]).
type_exparlist(G,[L1|RL],[T1|RT]):-write("exparlist \n"),write(L1),write(T1),write("\n"),type_expar(G,L1,T1),type_exparlist(G,RL,RT).
/* def -> const */
type_def(G,const(X,T,E),[(X,T)|G]):-type_expr(G,E,T).
/* def -> fun */
type_def(G,fun(X,T,ListArg,E),[(X,fleche(ListT,T))|G]):-write("fun\n"),write(G),write("\n ListArg:"),write(ListArg),
write("\n"),write(ListT),recup_type(ListArg,ListT),write("\n ListT : "),write(ListT),write("\n"),write(L),
write("\n"),env_extend(G,L,G2),write(G2),type_expr(G2,E,T),write("fin fun\n").

tab(Tab) :-Tab = [].
recup_type([], TypeL) :-write("Liste des types :\n"),write(TypeL),write("\n"),tab(TypeL).
recup_type([(X, T) | RL], [T | TypeL]) :-recup_type(RL, TypeL).
recup_type([var(X, T) | RL], [ref(T) | TypeL]) :-recup_type(RL, TypeL).
/* def -> fun rec */
type_def(G,funrec(X,T,ListArg,E),[(X,fleche(ListT,T))|G]):-recup_type(ListArg,ListT),env_extend(G,ListArg,G2),type_expr([(X,fleche(ListT,T))|G2],E,T).
/* def -> var */
type_def(G,var(X1,T),[(X1,ref(T))|G]):-write("aled\n").
/* def -> proc */
type_def(G,proc(X,ListArg,BK),[(X,fleche(L,void))|G]):-
write("proc \n"),write(G),recup_liste_typ(ListT,ListArg,L),write("\n"),write(ListT),
write("\n proc etape2 \n"),write(L),write("\n"),
env_extend(G,ListT,G2),write(G2),write("\n"),write(" etape4 \n"),type_block(G2,BK,void),write("fin proc\n").

recup_liste_typ(Liste,[],TypeL):-write("TypeL\n"),write(TypeL),write("\n"),tab(TypeL),tab(Liste).
recup_liste_typ([(X,T)|Liste],[(X,T)|RL],[T|TypeL]):-write("recup_liste_typ\n"),write(TypeL),recup_liste_typ(Liste,RL,TypeL).
recup_liste_typ([(X,ref(T))|Liste],[var(X,T)|RL],[ref(T)|TypeL]):-write("recup_liste_typ\n"),recup_liste_typ(Liste,RL,TypeL).
/* def -> proc rec */
type_def(G,procrec(X,ListArg,BK),[(X,fleche(L,void))|G]):-write("proc rec\n"),write(G),
recup_liste_typ(ListT,ListArg,L),write("\n ListT : "),write(ListT),write("\n ListArg : "),write(L),
write("\n proc rec etape2 \n"),write(L),write("\n"),env_extend(G,ListT,G2),
write(G2),write("\n"),write(" proc rec etape4 \n"),type_block([(X,fleche(L,void))|G2],BK,void),write("fin proc rec\n").

/* exprp -> adr , ref */
type_expar(G,adr(X),ref(T)):-write("expar adr \n"),write(E),write(T),type_expr(X,id(X),ref(T)).
/* exprp -> val */
type_expar(G,E,T):-write("expar val \n"),write(E),write(T),write("\n"),type_expr(G,E,T),write("fin expar val\n").

/* expr -> num */
type_expr(G,N,int):-write("num int \n"),write(N),write("\n"),integer(N),write("fin num int\n").
type_expr(G,N,ref(int)):-write("num ref \n"),write(N),write("\n"),integer(N).
/* expr -> bool */
type_expr(G,false,bool).
type_expr(G,true,bool).
/* expr -> id ref*/
type_expr([(X,ref(T))|G],id(X),T).
/* expr -> id */
type_expr([(X,T)|G],id(X),T):-write(X),write(" "),write(T).
type_expr([(X1,T1)|G],id(X),T):-write("whattt\n"),write(T1),write("\n"),write(G),write("oh\n"),type_expr(G,id(X),T).
/* expr -> if */
type_expr(G,if(A,B,C),T):-write("if expr\n"),type_expr(G,A,bool),write("if expr etape2\n"),type_expr(G,B,T),write("if expr etape3\n"),type_expr(G,C,T),write("fin if expr\n").
/* expr -> or */
type_expr(G,or(A,B),bool):-type_expr(G,A,bool),type_expr(G,B,bool).
/* expr -> and */
type_expr(G,and(A,B),bool):-type_expr(G,A,bool),type_expr(G,B,bool).
/*expr -> not*/
type_expr(G,not(A),bool):-type_expr(G,A,bool).
/* expr -> binary*/
type_expr(G,op(nom,A,B),T):-type_expr(G,A,TA),type_expr(G,B,TB),type_expr(G,nom,fleche([TA,TB],T)).
/* expr -> abs */
type_expr(G,abs(ListArg,E),fleche(ListT,T2)):-write("abs\n"),arg_type(ListArg,ListT),write("abs etape2\n"),env_extend(G,ListArg,G2),write("abs etape3\n"),type_expr(G2,E,T2).
env_extend(G,[],G).
env_extend(G,[DL|RL],Gnew):-env_extend([DL|G],RL,Gnew).
arg_type([],ListT):-write("arg type\n"),write(ListT),tab(ListT).
arg_type([(X,T)|RL],[T|ListT]):-write("deb arg typ\n"),arg_type(RL,ListT),write(LT),write("\n").
/* expr -> app */
type_expr(G,app(E,ListE),T):-write("app\n"),write(ListE),write("\n"),type_expr(G,E,fleche(ListT,T)),write("app etape2\n"),write(G),write("\n erreur : "),write(ListT),write("\n"),type_exprlist(G,ListE,ListT),write("fin app\n").
type_exprlist(G,[],[]).
type_exprlist(G,[L1|RL],[T1|RT]):-write(L1),write(T1),write("\n"),type_expr(G,L1,T1),type_exprlist(G,RL,RT).
