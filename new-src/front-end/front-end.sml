(* front-end.sml
 *
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * CMSC 22610 Sample code (Winter 2007)
 *)

structure FrontEnd : sig

    val checkFile : string -> AST.program option

  end = struct

  (* parse and typecheck a file *)
    fun checkFile file = (case Parser.parseFile file
	   of SOME pt => Typechecker.check pt
	    | NONE => NONE
	  (* end case *))

  end

