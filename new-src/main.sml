(* main.sml
 *
 * COPYRIGHT (c) 2008 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Main : sig

    val toAST  : string -> AST.program option
    val toMono : string -> AST.program option

    val main : (string * string list) -> OS.Process.status

  end = struct

  (* parse and typecheck a file *)
    val toAST = FrontEnd.checkFile

    val toMono = Option.map Mono.transform o toAST

    fun main (_, [file]) = (let
	  val ast = toAST file
	  val ast = Option.map Mono.transform ast
	  in
	    if !Error.anyErrors
	      then OS.Process.failure
	      else OS.Process.success
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

