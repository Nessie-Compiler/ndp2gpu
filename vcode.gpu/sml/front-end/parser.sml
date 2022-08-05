(* parser.sml
 *
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * CMSC 22610 Sample code (Winter 2007)
 *)

structure Parser : sig

  (* parse a file; return NONE if there are syntax errors *)
    val parseFile : string -> ParseTree.program option
    val parseString : string -> ParseTree.program option
    val getTokens : string -> VCodeLex.UserDeclarations.lex_result list;
  end = struct

  (* glue together the lexer and parser *)
    structure VCodeParser = VCodeParseFn(VCodeLex)

  (* global flag to record the existance of errors *)
    val anyErrors = Error.anyErrors

  (* error function for parsers *)
    fun parseErr (filename, srcMap) = let
	  val errToStr = AntlrRepair.repairToString VCodeTokens.toString srcMap
	  in
	    fn err => Error.say ["Error [", filename, "] ", errToStr err]
	  end

  (* parse a file, returning a parse tree *)
    fun parseFile filename = let
	  val _ = (anyErrors := false; Error.sourceFile := filename)
	  val file = TextIO.openIn filename
	  fun get () = TextIO.input file
	  val srcMap = AntlrStreamPos.mkSourcemap()
	  val _ = Error.sourceMap := srcMap
	  val lexer = VCodeLex.lex srcMap
	  in
	    case VCodeParser.parse lexer (VCodeLex.streamify get)
	     of (SOME pt, _, []) => (TextIO.closeIn file; SOME pt)
	      | (_, _, errs) => (
		  TextIO.closeIn file;
		  List.app (parseErr (filename, srcMap)) errs;
		  NONE)
	    (* end case *)
	  end

  (* parse a string, returning a parse tree *)
    fun parseString code = let
	val _ = (anyErrors := false; Error.sourceFile := "interactive")
	val srcMap = AntlrStreamPos.mkSourcemap()
	val _ = Error.sourceMap := srcMap
	val lexer = VCodeLex.lex srcMap
        val get = let
            val first = ref true
        in
         fn () => if (!first) then (first := false; code)
                     else ""
        end
    in
	case VCodeParser.parse lexer (VCodeLex.streamify get)
	 of (SOME pt, _, []) => (SOME pt)
	  | (_, _, errs) => (
	    List.app (parseErr ("interactive", srcMap)) errs;
	    NONE)
    (* end case *)
    end

    (*string -> UserDeclarations.lex_result list;*)
    fun getTokens code = let
        val map = AntlrStreamPos.mkSourcemap();
        val lexer = VCodeLex.lex map;
        val get = let
            val first = ref true
        in
            fn () => if (!first) then (first := false; code)
                     else ""
        end
        val stream = VCodeLex.streamify get;
        fun buildStream ((VCodeLex.UserDeclarations.T.EOF, _, stm), n) = []
          | buildStream ((tok, _, stm), 0) = (tok :: [])
          | buildStream ((tok, _, stm), n) = (tok :: buildStream ((lexer stm), n-1))
    in
        buildStream(lexer stream, 10)
    end
  end
