%%

%byaccj

%{
  private Parser yyparser;

  public Yylex(java.io.Reader r, Parser yyparser) {
    this(r);
    this.yyparser = yyparser;
  }
%}

nls  = \n | \r | \r\n
nums = -?[0-9]+
ident = [a-z][a-zA-Z0-9]*

%%

{nls}   { return 0; }
[ \t]+ { }

"["  { return Parser.LBRA; }
"]"  { return Parser.RBRA; }
"("  { return Parser.LPAR; }
")"  { return Parser.RPAR; }
";"  { return Parser.PVIR; }
":"  { return Parser.DP; }
","  { return Parser.VIR; }
"*"  { return Parser.ETOILE; }
"->" { return Parser.FLECHE; }

"ECHO" { return Parser.ECHO; }
"FUN" { return Parser.FUN; }
"REC" { return Parser.REC; }
"BOOL" { return Parser.BOOL; }
"CONST" { return Parser.CONST; }
"INT" { return Parser.INT; }
"IF" { return Parser.IF; }
"AND" { return Parser.AND; }
"OR" { return Parser.OR; }

{nums}  { yyparser.yylval = new ParserVal(Integer.parseInt(yytext()));
         return Parser.NUM; }

{ident} { yyparser.yylval = new ParserVal(yytext());
  return Parser.IDENT;
}

\b     { System.err.println("Sorry, backspace doesn't work"); }

/* error fallback */
[^]    { System.err.println("Error: unexpected character '"+yytext()+"'"); return -1; }