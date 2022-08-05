(* vcode.lex
 *
 *)

%name VCodeLex;

%defs(

    structure T = VCodeTokens

  (* error function for lexers *)
    fun lexErr (lnum, msg) = Error.say [
	    "Error [", !Error.sourceFile, ":", Int.toString lnum, "]: ", msg, "\n"
	  ]

  (* some type lex_result is necessitated by ml-ulex *)
    type lex_result = T.token

  (* eof : unit -> lex_result *)
  (* ml-ulex requires this as well *)
    fun eof () = T.EOF

  (* keyword lookup table *)
    local
      val find =
	  let val tbl = AtomTable.mkTable (103, Fail "keywords")
	      fun ins (id, tok) = AtomTable.insert tbl (Atom.atom id, tok)
	  in
	      app ins [
                 ("+", T.PLUS),
                 ("-", T.MINUS),
                 ("*", T.TIMES),
                 ("/", T.DIV),
                 ("%", T.MOD),
                 ("<", T.LT),
                 ("<=", T.LEQ),
                 (">", T.GT),
                 (">=", T.GEQ),
                 ("=", T.EQ),
                 ("!=", T.NEQ),
                 ("LSHIFT", T.LSHIFT),
                 ("RSHIFT", T.RSHIFT),
                 ("NOT", T.NOT),
                 ("AND", T.AND),
                 ("OR", T.OR),
                 ("XOR", T.XOR),
                 ("SELECT", T.SELECT),
                 ("RAND", T.RAND),
                 ("FLOOR", T.FLOOR),
                 ("CEIL", T.CEIL),
                 ("TRUNC", T.TRUNC),
                 ("ROUND", T.ROUND),
                 ("LOG", T.LOG),
                 ("SQRT", T.SQRT),
                 ("EXP", T.EXP),
                 ("SIN", T.SIN),
                 ("COS", T.COS),
                 ("TAN", T.TAN),
                 ("ASIN", T.ASIN),
                 ("ACOS", T.ACOS),
                 ("ATAN", T.ATAN),
                 ("SINH", T.SINH),
                 ("COSH", T.COSH),
                 ("TANH", T.TANH),
                 ("I_TO_F", T.I_TO_F),
                 ("I_TO_B", T.I_TO_B),
                 ("B_TO_I", T.B_TO_I),
                 ("+_SCAN", T.PLUS_SCAN),
                 ("*_SCAN", T.MULT_SCAN),
                 ("MAX_SCAN", T.MAX_SCAN),
                 ("MIN_SCAN", T.MIN_SCAN),
                 ("AND_SCAN", T.AND_SCAN),
                 ("OR_SCAN", T.OR_SCAN),
                 ("XOR_SCAN", T.XOR_SCAN),
                 ("+_REDUCE", T.PLUS_REDUCE),
                 ("*_REDUCE", T.MULT_REDUCE),
                 ("MAX_REDUCE", T.MAX_REDUCE),
                 ("MIN_REDUCE", T.MIN_REDUCE),
                 ("AND_REDUCE", T.AND_REDUCE),
                 ("OR_REDUCE", T.OR_REDUCE),
                 ("XOR_REDUCE", T.XOR_REDUCE),
                 ("PERMUTE", T.PERMUTE),
                 ("DPERMUTE", T.DPERMUTE),
                 ("FPERMUTE", T.FPERMUTE),
                 ("BPERMUTE", T.BPERMUTE),
                 ("BFPERMUTE", T.BFPERMUTE),
                 ("DFPERMUTE", T.DFPERMUTE),
                 ("EXTRACT", T.EXTRACT),
                 ("REPLACE", T.REPLACE),
                 ("DIST", T.DIST),
                 ("INDEX", T.INDEX),
                 ("RANK_UP", T.RANK_UP),
                 ("RANK_DOWN", T.RANK_DOWN),
                 ("PACK", T.PACK),
                 ("LENGTH", T.LENGTH),
                 ("MAKE_SEGDES", T.MAKE_SEGDES),
                 ("LENGTHS", T.LENGTHS),
                 ("COPY", T.COPY),
                 ("POP", T.POP),
                 ("CPOP", T.CPOP),
                 ("PAIR", T.PAIR),
                 ("UNPAIR", T.UNPAIR),
                 ("CALL", T.CALL),
                 ("RET", T.RET),
                 ("FUNC", T.FUNC),
                 ("IF", T.IF),
                 ("ELSE", T.ELSE),
                 ("ENDIF", T.ENDIF),
                 ("CONST", T.CONST),
                 ("EXIT", T.EXIT),
                 ("START_TIMER", T.START_TIMER),
                 ("STOP_TIMER", T.STOP_TIMER),
                 ("READ", T.READ),
                 ("WRITE", T.WRITE),
                 ("FOPEN", T.FOPEN),
                 ("FCLOSE", T.FCLOSE),
                 ("FREAD", T.FREAD),
                 ("FREAD_CHAR", T.FREAD_CHAR),
                 ("FWRITE", T.FWRITE),
                 ("SPAWN", T.SPAWN),
                 ("SRAND", T.SRAND),
                 ("INT", T.INT),
                 ("BOOL", T.BOOL),
                 ("FLOAT", T.FLOAT),
                 ("SEGDES", T.SEGDES),
                 ("CHAR", T.CHAR),
                 ("T", T.V_TRUE),
                 ("F", T.V_FALSE),
                 ("NULL_STREAM", T.NULL_STREAM),
                 ("STDIN", T.STDIN),
                 ("STDOUT", T.STDOUT),
                 ("STDERR", T.STDERR)
	      ];
	      AtomTable.find tbl
	  end
    in
  (* return either a keyword token or a NAME token *)
    fun idToken id =
	let val ida = Atom.atom id
	in
	    case find ida
	     of NONE => T.NAME ida
	      | SOME kw => kw
	end
    end
);

%states INITIAL COMMENT;

%let digit =	[0-9];
%let integer =	[-+]?{digit}+;
%let float =	[-+]?{digit}+\.({digit}+)?([Ee][-+]?{digit}+)?;
%let identifier =	[-+*/%<>=!._A-Za-z0-9]+;
%let comment =	"{"[^}]*"}";
%let string =	\"[^"]*\";
%let whitespace =	[ \t];
%let newline =\n;
%let begin_vector =	"(";
%let end_vector =	")";
%let input_info =	I;
%let output_info = O;

<INITIAL>{integer}	=> (T.INTEGER(valOf (IntInf.fromString yytext)));

<INITIAL>{float}	=> (T.REAL(valOf (Real.fromString yytext)));

<INITIAL>{input_info} => (T.INPUT_INFO);

<INITIAL>{output_info} => (T.OUTPUT_INFO);

<INITIAL>{identifier}	=> (idToken yytext);

<INITIAL>{newline} => (continue());

<INITIAL> "{"	=> (YYBEGIN COMMENT; continue());
<COMMENT> [^"}"\n]* => (continue());
<COMMENT> \n	=> (continue());
<COMMENT> "}" => (YYBEGIN INITIAL; continue());

<INITIAL>{string}	=> (T.STRING(yytext));

<INITIAL>{begin_vector} => (T.BEGIN_VECTOR);
<INITIAL>{end_vector}	=> (T.END_VECTOR);

<INITIAL>{comment} 	=> (continue ());

<INITIAL>{whitespace}	=> (continue ());
<INITIAL> . => (
	lexErr(!yylineno, concat["bad character `", String.toString yytext, "'"]);
	continue());
