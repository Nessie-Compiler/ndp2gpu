(* fkl-opt.sml
 *
 * Some simple optimizations of FKL code.  These are mostly to cleanup the output of flattening.
 *)

structure FKLOpt : sig

    val optimize : FKL.program -> FKL.program

  end = struct

    structure F = FKL
    structure VMap = Var.Map

  (* tests for specific primitive operators *)
    fun isPrim p f = Var.same(p, f)
    val isEmptyPrim = isPrim Prim.empty
    val isLengthPrim = isPrim Prim.##
    val isDistPrim = isPrim Prim.dist

  (* approximate aritys of arrays *)
    datatype arity = AnyArity | NonEmptyArity | FixedArity of int

  (* approximate values *)
    datatype value
      = ConstVal of Literal.literal
      | ArrayVal of arity
      | UnknownVal

    fun valueOf (env, x) = (case VMap.find(env, x)
	   of NONE => UnknownVal
	    | SOME v => v
	  (* end case *))

  (* evaluate primops on approximate values *)
    fun evalPrim (env, f, [v]) =
	  if isEmptyPrim f
	    then (case v
	       of ArrayVal NonEmptyArity => ConstVal(Literal.Bool false)
		| ArrayVal(FixedArity 0) => ConstVal(Literal.Bool true)
		| ArrayVal(FixedArity _) => ConstVal(Literal.Bool false)
		| _ => UnknownVal
	      (* end case *))
	  else if isLengthPrim f
	    then (case v
	       of ArrayVal(FixedArity i) => ConstVal(Literal.int i)
		| _ => UnknownVal
	      (* end case *))
	    else UnknownVal
      | evalPrim (env, f, [v1, v2]) =
	  if isDistPrim f
	    then (case v2
	       of ConstVal(Literal.Int i) => ArrayVal(FixedArity(IntInf.toInt i))
		| _ => UnknownVal
	      (* end case *))
	    else UnknownVal
      | evalPrim (_, f, args) = UnknownVal

    fun optExp (env, e) = (case e
	   of F.Const lit => (ConstVal lit, e)
	    | F.Var x => (valueOf(env, x), e)
	    | F.Tuple es => (UnknownVal, F.Tuple(optExps(env, es)))
	    | F.App(f, es) => let
    		val (vs, es) = ListPair.unzip (List.map (fn e => optExp(env, e)) es)
    		in
    		  if Var.isPrim f
    		    then (case evalPrim (env, f, vs)
		       of (v as ConstVal lit) => (v, F.Const lit)
			| v => (v, F.App(f, es))
		      (* end case *))
    		    else (UnknownVal, F.App(f, es))
    		end
	    | F.Let(x, e1, e2) => let
    		val (v1, e1) = optExp(env, e1)
    		val (v2, e2) = optExp(VMap.insert(env, x, v1), e2)
    		in
    		  case (v1, v2)
    		   of (_, ConstVal lit) => (v2, F.Const lit)
    		    | (ConstVal lit, _) => (v2, e2)
    		    | _ => (v2, F.Let(x, e1, e2))
    		  (* end case *)
    		end
	    | F.If(e1, e2, e3) => (case optExp(env, e1)
    		 of (ConstVal(Literal.Bool true), _) => optExp(env, e2)
    		  | (ConstVal(Literal.Bool false), _) => optExp(env, e3)
    		  | (UnknownVal, e1 as F.App(f, [F.Var x])) =>
    		      if isEmptyPrim f
    			then let (* specialize the value of x in e2 and e3 *)
    			  val (_, e2) = optExp(VMap.insert(env, x, ArrayVal(FixedArity 0)), e2)
    			  val (_, e3) = optExp(VMap.insert(env, x, ArrayVal NonEmptyArity), e3)
    			  in
    			    (UnknownVal, F.If(e1, e2, e3))
    			  end
    			else (UnknownVal, F.If(e1, #2(optExp(env, e2)), #2(optExp(env, e3))))
    		  | (UnknownVal, e1) => (UnknownVal, F.If(e1, #2(optExp(env, e2)), #2(optExp(env, e3))))
    		  | _ => raise Fail "bogus argument to If"
    		(* end case *))
	    | F.PArray[] => (ArrayVal(FixedArity 0), F.PArray[])
    	    | F.PArray es => (ArrayVal(FixedArity(length es)), F.PArray(optExps(env, es)))
    	  (* end case *))

    and optExps (env, es) = List.map (fn e => #2(optExp(env, e))) es

    fun optimize (F.Prog(fds, e)) = let
	  fun optFun (F.Fun(f, params, e)) = F.Fun(f, params, #2(optExp(VMap.empty, e)))
	  val e = #2(optExp(VMap.empty, e))
	  in
	    F.Prog(List.map optFun fds, e)
	  end

  end
