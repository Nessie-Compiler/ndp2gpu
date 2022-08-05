(* kl.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure KL =
  struct

    datatype var = V of (string * word)

    datatype funid = FId of (string * bool)	(* true means lifted *)

    datatype program = Prog of (fundef list * exp)

    and fundef = FDef of (funid * var list * exp)

    and exp
      = Const of string
      | Var of var
      | Tuple of exp list
      | App of (funid * exp list)
      | Let of (var * exp * exp)
      | If of (exp * exp * exp)
      | Vector of exp list
      | ForEach of (exp * generator list)
      | Lambda of (var list * exp)
      | Compose of exp * exp

    withtype generator = (var * exp)

  end

structure Var =
  struct

    datatype var = datatype KL.var

    local
      val cnt = ref 0w0
    in
    fun new s = let val id = !cnt in cnt := id+0w1; V(s, id) end
    end

    fun fresh (V(s, _)) = new s

    fun compare (V(_, id1), V(_, id2)) = Word.compare(id1, id2)

  end
