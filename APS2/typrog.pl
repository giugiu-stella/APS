/* contexte initial */
g0([(true,bool),(false,bool),(not,fleche([bool],bool)),(eq,fleche([int,int],bool)),
(lt,fleche([int,int],bool)),(add,fleche([int,int],int)),(sub,fleche([int,int],int)),
(mul,fleche([int,int],int)),(div,fleche([int,int],int)),(or,fleche([bool,bool],bool)),(and,fleche([bool,bool],bool))]).

/* main */
main:-read(AST),check(AST).
check(AST):-type_prog(AST),write("ok!").
check(AST):-write("nop "), write("AST: "), write(AST).


/* prog */
type_prog(prog(X)):-g0(G),type_block(G,X,void).

/* block */
type_block(G,block(X),void):-type_cmds(G,X,void).

/* cmds -> end */
type_cmds(G,[],void).
/* cmds -> defs */
type_cmds(G,[def(D)|X],void):-type_def(G,D,G2),type_cmds(G2,X,void).
/* cmds -> stat */
type_cmds(G,[stat(S)|CS],void):-type_stat(G,S,void),type_cmds(G,CS,void).

/* stat-> echo */
type_stat(G,echo(E),void):-type_expr(G,E,int).
/* stat -> set */
type_stat(G,set(X,E),void):-type_lvalue(G,X,T),type_expr(G,E,T).
/* stat-> if */ 
type_stat(G,ifbk(E,BK1,BK2),void):-type_expr(G,E,bool),type_block(G,BK1,void),type_block(G,BK2,void).
/* stat -> while */
type_stat(G,while(E,BK),void):-type_expr(G,E,bool),type_block(G,BK,void).
/* stat -> call */
type_stat(G,call(X,EPS),void):-type_expr(G,X,fleche(ListT,void)),type_exparlist(G,EPS,ListT).
type_exparlist(G,[],[]).
type_exparlist(G,[L1|RL],[T1|RT]):-type_expar(G,L1,T1),type_exparlist(G,RL,RT).
/* def -> const */
type_def(G,const(X,T,E),[(X,T)|G]):-type_expr(G,E,T).
/* def -> fun */
type_def(G,fun(X,T,ListArg,E),[(X,fleche(ListT,T))|G]):-recup_type(ListArg,ListT),env_extend(G,L,G2),type_expr(G2,E,T).

tab(Tab) :-Tab = [].
recup_type([], TypeL) :-tab(TypeL).
recup_type([(X, T) | RL], [T | TypeL]) :-recup_type(RL, TypeL).
recup_type([var(X, T) | RL], [ref(T) | TypeL]) :-recup_type(RL, TypeL).
/* def -> fun rec */
type_def(G,funrec(X,T,ListArg,E),[(X,fleche(ListT,T))|G]):-recup_type(ListArg,ListT),env_extend(G,ListArg,G2),type_expr([(X,fleche(ListT,T))|G2],E,T).
/* def -> var */
type_def(G,var(X1,bool),[(X1,ref(bool))|G]).
type_def(G,var(X1,int),[(X1,ref(int))|G]).
/* def -> proc */
type_def(G,proc(X,ListArg,BK),[(X,fleche(L,void))|G]):-recup_liste_typ(ListT,ListArg,L),env_extend(G,ListT,G2),type_block(G2,BK,void).

recup_liste_typ(Liste,[],TypeL):-tab(TypeL),tab(Liste).
recup_liste_typ([(X,T)|Liste],[(X,T)|RL],[T|TypeL]):-recup_liste_typ(Liste,RL,TypeL).
recup_liste_typ([(X,ref(T))|Liste],[var(X,T)|RL],[ref(T)|TypeL]):-recup_liste_typ(Liste,RL,TypeL).
/* def -> proc rec */
type_def(G,procrec(X,ListArg,BK),[(X,fleche(L,void))|G]):-recup_liste_typ(ListT,ListArg,L),env_extend(G,ListT,G2),type_block([(X,fleche(L,void))|G2],BK,void).

/* exprp -> adr , ref */
type_expar(G,adr(X),ref(T)):-type_expr(X,id(X),ref(T)).
/* exprp -> val */
type_expar(G,E,T):-type_expr(G,E,T).


/* expr -> num */
type_expr(G,N,int):-integer(N).
/* expr -> bool */
type_expr(G,false,bool).
type_expr(G,true,bool).
/* expr -> id ref*/
type_expr([(X,ref(T))|G],id(X),T).
/* expr -> id */
type_expr([(X,ref(T))|G],arg(X),ref(T)).
type_expr([(X1,T1)|G],arg(X),ref(T)):-type_expr(G,arg(X),ref(T)).
type_expr([(X,T)|G],id(X),T).
type_expr([(X1,T1)|G],id(X),T):-type_expr(G,id(X),T).
/* expr -> if */
type_expr(G,if(A,B,C),T):-type_expr(G,A,bool),type_expr(G,B,T),type_expr(G,C,T).

/* expr -> abs */
type_expr(G,abs(ListArg,E),fleche(ListT,T2)):-arg_type(ListArg,ListT),env_extend(G,ListArg,G2),type_expr(G2,E,T2).
env_extend(G,[],G).
env_extend(G,[DL|RL],Gnew):-env_extend([DL|G],RL,Gnew).
arg_type([],ListT):-tab(ListT).
arg_type([(X,T)|RL],[T|ListT]):-arg_type(RL,ListT).
/* expr -> app */
type_expr(G,app(E,ListE),T):-type_expr(G,E,fleche(ListT,T)),type_exprlist(G,ListE,ListT).
type_exprlist(G,[],[]).
type_exprlist(G,[L1|RL],[T1|RT]):-type_expr(G,L1,T1),type_exprlist(G,RL,RT).
/* expr -> alloc */
type_expr(G,alloc(E),vec(T)):-type_expr(G,E,int).
/* expr -> len */
type_expr(G,len(E),int):-type_expr(G,E,vec(T)).
/* expr -> nth */
type_expr(G,nth(E1,E2),LT):-type_expr(G,E1,vec(T)),type_expr(G,E2,int),recup_type_vec(T,LT).
/* expr -> vset */
type_expr(G,vset(E1,E2,E3),T):-type_expr(G,E1,vec(T)),type_expr(G,E2,int),type_expr(G,E3,T).

/* lvalue */
type_lvalue(G,ident(ID),T):-type_expr(G,id(ID),T).
type_lvalue(G,nth(X,E),LT):-type_lvalue(G,X,T),type_expr(G,E,int),recup_type_vec(T,LT).


recup_type_vec(vec(T),LT):-recup_type_vec(T,LT).
recup_type_vec(T,T).