(* pvector.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parallel vector operations implemented using lists.
 *)

signature PVEC =
  sig

    type 'a vec

    val ## : 'a vec -> int			(* length *)
    val F : 'a vec vec -> 'a vec		(* flatten *)
    val S : 'a vec vec -> int vec		(* construct segment descriptor *)
    val P : ('a vec * int vec) -> 'a vec vec	(* partition *)

  end

structure PVec :> PVEC =
  struct

    type 'a vec = 'a list

    val ## = List.length
    val F = List.concat

    fun S l = List.map ## l

    fun P (l, s) = let
	  fun split (0, pre, rest) = (List.rev pre, rest)
	    | split (i, pre, x::rest) = split (i-1, x::pre, rest)
	    | split _ = raise Fail "runtime error"
	  fun part (i, (rest, result)) = let
		val (l, rest) = split (i, [], rest)
		in
		  (rest, l::result)
		end
	  in
	    List.rev (#2(List.foldl part (l, []) s))
	  end

  end
