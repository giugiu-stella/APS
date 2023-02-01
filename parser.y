
%{

  import java.io.*;
  import java.util.ArrayList;

%}
 //HELLLLLLOOOO 
%token <ival> NUM            
%token <sval> IDENT
%token ECHO
%token LPAR RPAR             
%token LBRA RBRA
%token PVIR
%token DP
%token VIR
%token ETOILE
%token FLECHE
%token FUN 
%token REC
%token BOOL
%token CONST
%token INT
%token IF
%token AND 
%token OR


%type <obj> expr
%type <obj> exprs
%type <obj> stat
%type <obj> cmds
%type <obj> prog

%start prog      
%%


prog:  LBRA cmds RBRA   { $$ = new AstProg((ArrayList<Ast>)$2); }
;

cmds:
stat                    { ArrayList<Ast>r = new ArrayList<Ast>();
                          r.add((Ast)$1);
                          $$ = r;
                        }
| DEF PVIR CMDS 	{ { (ArrayList<AstDef>)$3).add((AstDef)$1); $$ = $3;}
;


Type:
BOOL	{  $$ = new AstBool($1);  }
| INT 	{  $$ = new AstInt($1);  }
| LPAR TYPES FLECHE TYPE RPAR  {$$ = new AstTypefun($2,$4);} 

Types: 
TYPE	{ $$= new AstType($1) ;}
| TYPE ETOILE TYPES	{(ArrayList<AstType>)$3).add((AstType)$1); $$ = $3 };

Arg:
IDENT DP TYPE { $$= new AstArg($1,$3);}

Args:
ARG	{$$=$1;}
| ARG VIR ARGS {(ArrayList<AstArg>)$3).add((AstArg)$1); $$ = $3 };

Def:
CONST IDENT TYPE EXPR {$$= new AstConst($2,$3,$4);}
| FUN IDENT TYPE LBAR ARGS RBAR EXPR {$$= new AstFun($2,$3,$5,$7);}
| FUN REC IDENT TYPE LBAR ARGS RBAR EXPR {$$= new AstFunRec($3,$4,$6,$8);}



stat:
ECHO expr               { $$ = new AstEcho((Ast)$2); }
;

expr:
  NUM                   { $$ = new AstNum($1); }
| IDENT                 { $$ = new AstId($1); }
| LPAR IF EXPR EXPR EXPR RPAR 	{ $$ = new AstIf($3,$4,$5);}
| LPAR AND EXPR EXPR RPAR	{ $$ = new AstAnd($3,$4);}
| LPAR OR EXPR EXPR RPAR	{ $$ = new AstOr($3,$4);}
| LBAR ARGS RBAR EXPR		{ $$ = new ASTFun($2,$3);}
| LPAR expr exprs RPAR  { $$ = new AstApp((Ast)$2,(ArrayList<Ast>)$3); }
;

exprs:
  expr                  { ArrayList<Ast> r = new ArrayList<Ast>();
                          r.add((Ast)$1);
			  $$ = r;}
| expr exprs            { ((ArrayList<Ast>)$2).add((Ast)$1); $$ = $2; }
;
%%

  public Ast prog;
  
  private Yylex lexer;


  private int yylex () {
    int yyl_return = -1;
    try {
      yylval = new ParserVal(0);
      yyl_return = lexer.yylex();
    }
    catch (IOException e) {
      System.err.println("IO error :"+e);
    }
    return yyl_return;
  }


  public void yyerror (String error) {
    System.err.println ("Error: " + error);
  }


  public Parser(Reader r) {
    lexer = new Yylex(r, this);
  }