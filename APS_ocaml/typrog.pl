type_prog();

type_expr(true,bool).
type_expr(false,bool).

type_expr(G,true,bool).
type_expr([(X,T)|G],id(X),T).
type_expr(G,if(A,B,C),T):-type_expr(A,bool),type_expr(B,T),type(C,T).
type_expr(G,or(A,B),bool):-type_expr(A,bool),type_expr(B,bool).
type_expr(G,and(A,B),bool):-type_expr(A,bool),type_expr(B,bool).
type_expr([(X1,T1)|G],id(X),T):-type_expr(G,id(X),T).
type_expr(G,num(N),int).

type_expr(G,A,List,fleche(T1,T2)):-type_expr(G,A,fleche(T1,T2)),type_exprs(G,List,T1).
type_exprs(G,[],[]).
type_exprs(G,[L1|RL],[T1|RT]):-type_expr(G,L1,T1),type_exprs(G,RL,RT).