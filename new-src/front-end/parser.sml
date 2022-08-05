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

  end = struct

  (* glue together the lexer and parser *)
    structure MinMLParser = MinMLParseFn(MinMLLex)

  (* global flag to record the existance of errors *)
    val anyErrors = Error.anyErrors

  (* error function for parsers *)
    fun parseErr (filename, srcMap) = let
	  val errToStr = AntlrRepair.repairToString MinMLTokens.toString srcMap
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
	  val lexer = MinMLLex.lex srcMap
	  in
	    case MinMLParser.parse lexer (MinMLLex.streamify get)
	     of (SOME pt, _, []) => (TextIO.closeIn file; SOME pt)
	      | (_, _, errs) => (
		  TextIO.closeIn file;
		  List.app (parseErr (filename, srcMap)) errs;
		  NONE)
	    (* end case *)
	  end

  end
