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
- On s'est rendu compte lors du codage d'APS1a, que le code du typeur et de l'évaluateur comportait beaucoup d'erreur.
- choix d'implémentation : l'environnement.
En effet, pour le typeur, nous avons fait le choix d'avoir un environnement initial comportant les opérations de base :
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
- choix d'implémentation : ident = expr, mémoire
- 


## APS2
- choix d'implémentation : tableau


## Compilation

Makefile :
```
LEX_ML = ocamllex
YACC_ML = /usr/local/bin/ocamlyacc
YACC_ML= ocamlyacc
OCAMLC = ocamlc -g
 
all: prologTerm eval

eval: parser
	$(OCAMLC) -o eval ast.cmo lexer.cmo parser.cmo
	
prologTerm: parser prologTerm.ml
	$(OCAMLC) -o prologTerm ast.cmo lexer.cmo parser.cmo prologTerm.ml

parser: ast.ml lexer.mll parser.mly 
	$(OCAMLC) -c ast.ml
	$(LEX_ML) -o lexer.ml lexer.mll
	$(YACC_ML) -b parser parser.mly
	$(OCAMLC) -c parser.mli
	$(OCAMLC) -c lexer.ml
	$(OCAMLC) -c parser.ml
	$(OCAMLC) -c eval.ml

clean:
	rm -f *.cmo
	rm -f *.cmi
	rm -f prologTerm
	rm -f eval
	rm -f lexer.ml
	rm -f parser.mli
	rm -f parser.ml
	rm *~
```

Grâce au Makefile pour compiler, il nous suffit d'écrire make dans le terminal.


## Exécution 

- pour prologTerm : ./prologTerm test.aps
- pour le typeur : ./prologTerm test.aps | swipl -l typrog.pl -g main
- pour l'évaluateur : ./eval test.aps
