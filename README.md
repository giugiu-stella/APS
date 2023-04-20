# RAPPORT APS

Une courte description du projet et de ses objectifs.

## Table des matières

- [APS0](#APS0)
- [APS1a](#APS1a)
- [APS2](#APS2)
- [Compilation](#Compilation)
- [Exécution](#Exécution)


## APS0
- APS0 implémente les éléments de bases pour le typeur et l'évaluateur.
- On s'est rendu compte lors du codage d'APS1a que le code du typeur et de l'évaluateur comportait beaucoup d'erreurs.
- choix d'implémentation : l'environnement.
En effet, pour le typeur, nous avons fait le choix d'avoir un environnement initial comportant les opérations primitives :
```
g0([(true,bool),(false,bool),(not,fleche([bool],bool)),(eq,fleche([int,int],bool)),
(lt,fleche([int,int],bool)),(add,fleche([int,int],int)),(or,fleche([bool,bool],bool)),(and,fleche([bool,bool],bool)),(sub,fleche([int,int],int)),
(mul,fleche([int,int],int)),(div,fleche([int,int],int))]).
```
Pour l'évaluateur, nous sommes partis sur un module déjà existant, module Env = Map.Make(String).
Cela nous permettait d'optimiser notre code.
```
| lookup x env = match Env.find_opt x env with 
            | None -> assert false
            | Some v -> v
	
 Env.add x (eval_expr env e) env
 
 Env.empty
 ```

## APS1a
- extension : nous avons rajouté un printer dans l'évaluateur qui nous permet de visualiser le résultat et nous assurer que notre évaluateur est correct.
```
and  print_output output =
    List.iter (function x -> Printf.printf "%d\n" x) output
```
Par exemple, prenons le test.aps suivant :
```
[
VAR x int;
SET x 0;
ECHO x
SET x 2;
ECHO x
]
```
dans le terminal nous obtiendrons : 
```
2
0
```
- Nous avons modifié quelques lignes de grammaire.
```
ast du formulaire : 
 ASTSet of ident * expr
 ASTCall of ident * exprp list
 ASTExprpAdr of ident
 
 ast de notre code:
 ASTSet of expr * expr
 ASTCall of expr * exprp list
 ASTExprpAdr of expr
 ```
 Dans notre prologTerm, afin d'alléger le code ASTBinary qui correspond à la règle PRIM2,AND0,AND1,OR1 et OR0 ainsi que le code ASTNot correspondant à PRIM1 nous les avons convertit en règle app, qui permettra avec id(op) de rechercher les opérations primitives dans l'environnement :
 
 ```
 ASTBinary(op, e1, e2) ->(
      Printf.printf "app(id(";
      Printf.printf "%s" (op_to_string op);
      Printf.printf "),";
      Printf.printf "[";
      print_expr e1;
      Printf.printf ",";
      print_expr e2;
      Printf.printf "]";
      Printf.printf ")";
    )
  | ASTNot(e) -> (
      Printf.printf "app(" ;
      Printf.printf "id(not), [";
      print_expr e;
      Printf.printf "])";
    )
```
Enfin, dans le typeur nous avons rajouté une règle liéé à set qui nous permet de renvoyer ref(T) au lieu de simplement T, comme le fait la règle id : 
```
type_expr([(X,ref(T))|G],arg(X),ref(T)).
type_expr([(X1,T1)|G],arg(X),ref(T)):-type_expr(G,arg(X),ref(T)).

type_expr([(X,T)|G],id(X),T).
type_expr([(X1,T1)|G],id(X),T):-type_expr(G,id(X),T).
```
- implémentation de l'environnement : 
Nous avons modifié l'environnement car le module nous faisait perdre du temps, compréhension exact du module plus ma version d'Ocaml n'étant pas à jour, je ne pouvais pas avoir accès à certaines options. N'arrivant pas à mettre à jour ocaml sur mon macbook, nous avons modifié l'environnement en tant que (string * value) list.

- implémentation de la mémoire : 
Chaque fois que la fonction "alloc" est appelée, elle crée une nouvelle paire contenant l'identifiant de l'allocation et une référence à une valeur initialisée à -1, et ajoute cette paire à une liste, incrémente le compteur d'allocations et retourne la paire nouvellement créée. Cela permet de suivre les allocations de mémoire effectuées par la fonction et de référencer chaque nouvelle allocation par un identifiant unique.

```
let rec get_mem a  (m: (int * (value ref)) list) =
    (match m with
        | (a1,v1)::cs ->if(a == a1) then v1 else get_mem a cs
        | _ -> assert false)

let alloc m =
    let i = (!index_mem, (!index_mem, ref(INZ(-1)))::m ) in
    index_mem := (!index_mem + 1);
    i
```


## APS2

- APS2 n'est pas fini. 
Son typeur compile et exécute correctement cependant, son évaluateur comporte encore des erreurs, il compile mais s'exécute avec une erreur (à noter que les tests: test.aps, test2.aps et test3.aps s'exécute bien et renvoie le bon résulat). L'erreur se trouve dans eval_lvalue pour ASTvalueId, en effet lors du match evaluation de l'expression ne devrait renvoyer que INA mais il renvoie des INB et des INZ. Par manque de temps , je n'ai pas réussi à fixer ce problème.
- implémentation du tableau :
La fonction allocTab alloue un tableau en mémoire en prenant en entrée une liste représentant la mémoire disponible et la taille du tableau à allouer. La fonction vérifie que la taille est supérieure à zéro, puis elle ajoute un nombre déterminé d'éléments vides à la liste de mémoire en incrémentant l'adresse mémoire à chaque ajout. La fonction renvoie finalement un couple contenant l'adresse de début du tableau et la liste de mémoire modifiée.
 
 ```
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
 ```


## Compilation

Makefile :
```
LEX_ML = ocamllex
YACC_ML = /usr/local/bin/ocamlyacc
OCAMLC = ocamlc

all: prologTerm eval

eval: parser eval.ml
	$(OCAMLC) -o eval ast.cmo lexer.cmo parser.cmo eval.ml

prologTerm: parser prologTerm.ml
	$(OCAMLC) -o prologTerm ast.cmo lexer.cmo parser.cmo prologTerm.ml

parser: ast.ml lexer.mll parser.mly
	$(OCAMLC) -c ast.ml
	$(LEX_ML) -o lexer.ml lexer.mll
	$(YACC_ML) -b parser parser.mly
	$(OCAMLC) -c parser.mli
	$(OCAMLC) -c lexer.ml
	$(OCAMLC) -c parser.ml

clean:
	rm -f *.cmo
	rm -f *.cmi
	rm -f prologTerm
	rm -f eval
	rm -f lexer.ml
	rm -f parser.mli
	rm -f parser.ml
```

Grâce au Makefile pour compiler, il nous suffit d'écrire make dans le terminal.


## Exécution 

- pour prologTerm : ./prologTerm test.aps
- pour le typeur : ./prologTerm test.aps | swipl -l typrog.pl -g main
```
main:-read(AST),check(AST).
check(AST):-type_prog(AST),write("ok!").
check(AST):-write("nop "), write("AST: "), write(AST).
```
Cela nous permet d'utiliser directement le résultat de prologTerm et de s'avoir si le test passe, ok! dans le terminal ou nop : avec le prologTerm du test s'il y a une erreur.
- pour l'évaluateur : ./eval test.aps
