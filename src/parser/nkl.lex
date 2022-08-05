%name NKLLex;

%defs (
  structure T = NKLTokens
  type lex_result = T.token
  fun eof () = T.EOF
);

%let ws = [ \t\n\v\f\r];
%let digit = [0-9];
%let alpha = [a-zA-Z];

{ws}+	=> (skip());

"bool"  => (T.KW_bool);
"datatype" => (T.KW_datatype);
"else"	=> (T.KW_else);
"end"   => (T.KW_end);
"false"	=> (T.KW_false);
"float" => (T.KW_float);
"fun"	=> (T.KW_fun);
"if"	=> (T.KW_if);
"in"	=> (T.KW_in);
"int"   => (T.KW_int);
"let"	=> (T.KW_let);
"then"	=> (T.KW_then);
"true"	=> (T.KW_true);

{alpha}({alpha}|{digit}|_)*	=> (T.ID(Atom.atom yytext));
{digit}+			=> (T.INT(valOf(IntInf.fromString yytext)));

"="	=> (T.EQ);
","	=> (T.COMMA);
":"	=> (T.COLON);
"("	=> (T.LP);
")"	=> (T.RP);
"["	=> (T.LB);
"]"	=> (T.RB);
"{"	=> (T.LCB);
"}"	=> (T.RCB);
"+"	=> (T.PLUS);
"-"	=> (T.MINUS);
"*"	=> (T.STAR);
"/"	=> (T.SLASH);
"++"	=> (T.CONCAT);
"!"	=> (T.BANG);
"<"	=> (T.LT);
"<="	=> (T.LTE);
"=="	=> (T.EQEQ);
">="	=> (T.GTE);
">"	=> (T.GT);
"#"	=> (T.HASH);
"."     => (T.DOT);
.	=> (skip());


