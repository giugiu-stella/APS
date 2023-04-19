# RAPPORT APS

Une courte description du projet et de ses objectifs.

## Table des matières

- [APS0](#APS0)
- [APS1a](#APS1a)
- [APS2](#APS2)
- [Compilation](#Compilation)
- [Exécution](#Exécution)


## APS0
- mal codé, on s'en est rendu compte après 


## APS1a
- extension : print 
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


## Exécution 

- pour prologTerm : ./prologTerm test.aps
- pour le typeur : ./prologTerm test.aps | swipl -l typrog.pl -g main
- pour l'évaluateur : ./eval test.aps
