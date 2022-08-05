structure Prim =
  struct

  (* primitive operators are indexed by type, so we have a generic version (represented as
   * a variable) plus instances.
   *)
    datatype primop = POp of {
	name : Var.var,		(* identified by generic version *)
	tys : Ty.ty list	(* type arguments of specific instance *)
      }

  (* selection operators *)
    local
      val selOps = Vector.tabulate (10, fn i => Var.prim("SEL" ^ Int.toString(i+1)))
    in
    fun select i = Vector.sub(selOps, i-1)
    end

    local
      val a' = Ty.VarTy 0
      val intTy = Ty.BaseTy Ty.IntTy
      val boolTy = Ty.BaseTy Ty.BoolTy
      val a'Array = Ty.ArrayTy a'
      val boolArray = Ty.ArrayTy boolTy
    in
    val ##		= Var.prim("#",		1, [a'], intTy)
    val dist		= Var.prim("dist",	1, [a', intTy], a'Array)
    val P		= Var.prim("P",		1, [a'Array, Ty.ArrayTy intTy], Ty.ArrayTy(a'Array))
    val F		= Var.prim("F",		1, [Ty.ArrayTy(a'Array)], a'Array)
    val S		= Var.prim("S",		1, [Ty.ArrayTy(a'Array)], Ty.ArrayTy intTy)
    val empty		= Var.prim("empty",	1, [a'Array], boolTy)
    val combine		= Var.prim("combine",	1, [a'Array, a'Array, boolArray], a'Array)
    val pack		= Var.prim("pack",	1, [a'Array, boolArray], a'Array)
    val not		= Var.prim("not",	0, [boolTy], [boolTy])
    val not' = Var.lift not
    val sum = Var.prim "sum"
    val upto = Var.prim "upto"
    val default_permute = Var.prim "default_permute"

    val op + = Var.prim "+"
    val op - = Var.prim "-"
    val op * = Var.prim "*"
    val op / = Var.prim "/"
    val ++ = Var.prim "++"
    val op < = Var.prim "<"
    val op <= = Var.prim "<="
    val == = Var.prim "=="
    val op >= = Var.prim ">="
    val op > = Var.prim ">"
    val ! = Var.prim "!"

  end

