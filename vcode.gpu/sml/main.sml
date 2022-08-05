structure Main : sig

    val main : (string * string list) -> OS.Process.status

  end = struct

    fun main (_, [file]) = (let
          val pt = Parser.parseFile file
          val compiledName = concat[file, ".cc"]
	  in
	    if !Error.anyErrors
	      then OS.Process.failure
	      else (case pt of
                        SOME(program) => let
                            val outStream = TextIO.openOut compiledName
                        in
                            EmitPT.output (outStream,program) ;
                            TextIO.closeOut outStream;
                            OS.Process.success
                        end
                      | NONE => OS.Process.failure)
	  end
	  handle ex => (
	    Error.say [
		"uncaught exception ", General.exnName ex,
		" [", exnMessage ex, "]\n"
	      ];
	    List.app (fn s => Error.say ["  raised at ", s, "\n"]) (SMLofNJ.exnHistory ex);
	    OS.Process.failure))
      | main (prog, _) = (
	  TextIO.output(TextIO.stdErr, concat["usage: ", prog, " file\n"]);
	  OS.Process.failure)

  end

