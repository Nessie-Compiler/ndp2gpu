(* basis-names.sml
 *
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * CMSC 22610 Sample code (Winter 2007)
 *
 * Names of identifiers and operators bound in the MinML basis.
 *)

structure BasisNames =
  struct

    nonfix mod div

  (* predefined type names *)
    val bool =		Atom.atom "bool"
    val int =		Atom.atom "int"
    val list =		Atom.atom "list"
    val string =	Atom.atom "string"
    val unit =		Atom.atom "unit"
    val parray =	Atom.atom "parray"

  (* operators *)
    val eq =		Atom.atom "=="
    val neq =		Atom.atom "!="
    val lte =		Atom.atom "<="
    val lt =		Atom.atom "<"
    val gte =		Atom.atom ">="
    val gt =		Atom.atom ">"
    val append =	Atom.atom "@"
    val plus =		Atom.atom "+"
    val minus =		Atom.atom "-"
    val times =		Atom.atom "*"
    val div =		Atom.atom "div"
    val mod =		Atom.atom "mod"
    val uMinus =	Atom.atom "~"
    val subscript =	Atom.atom "!"
    val length =	Atom.atom "#"
    val arrayConcat =	Atom.atom "++"

  (* pre-defined data constructors *)
    val boolTrue =	Atom.atom "true"
    val boolFalse =	Atom.atom "false"
    val listCons =	Atom.atom "::"
    val listNil =	Atom.atom "nil"

  (* predefined variables *)
    val print =		Atom.atom "print"
    val fail =		Atom.atom "fail"

  end
