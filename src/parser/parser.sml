structure Parser : sig

    val parseFile : string -> NKL.program

  end = struct

    structure NKLParser = NKLParseFn(NKLLex)

  (* global flag to record the existance of errors *)
    val anyErrors = Error.anyErrors

  (* error function for parsers *)
    fun parseErr (filename, srcMap) = let
          val errToStr = AntlrRepair.repairToString NKLTokens.toString srcMap
          in
            fn err => Error.say ["Error [", filename, "] ", errToStr err]
          end

    fun parseFile filename = let
	  val inS = TextIO.openIn filename
	  fun get () = TextIO.input inS
	  val srcMap = AntlrStreamPos.mkSourcemap()
          val _ = Error.sourceMap := srcMap
	  val lexer = NKLLex.lex srcMap
	  in
	    case NKLParser.parse lexer (NKLLex.streamify get)
	     of (SOME pt, _, []) => (
		  TextIO.closeIn inS;
		  Normalize.xform pt)
	      | (_, _, errs) => (
                  List.app (parseErr (filename, srcMap)) errs;
		  raise Fail "parse error")
	    (* end case *)
	  end

  end

