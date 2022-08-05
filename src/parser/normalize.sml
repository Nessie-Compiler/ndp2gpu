(* normalize.sml
 *
 * Convert the parse-tree representation of NKL to the
 * normalized representation.
 *)

structure Normalize : sig

    val xform : ParseTree.program -> NKL.program

  end = struct

    structure Env = AtomMap
    structure PT = ParseTree

    fun fresh (env, x) = let
	  val x' = Var.new (Atom.toString x)
	  in
	    (Env.insert (env, x, x'), x')
	  end

    fun xform (PT.Prog(fds, e)) = let
	  fun bindFun (PT.Fun(f, _, _), env) = #1 (fresh (env, f))
	  val topEnv = List.foldl bindFun Env.empty fds
	  fun xformFun (PT.Fun(f, params, e)) = let
		val f' = Env.lookup (topEnv, f)
		fun doArg ((x, ty), (env, xs)) = let
		      val (env', x') = fresh (env, x)
		      in
			(env', x'::xs)
		      end
		val (env', params') = List.foldr doArg (topEnv, []) params
		in
		  NKL.Fun(f', params', xformExp(env', e))
		end
	  and xformExp (env, e) = (case e
	       of PT.Const lit => NKL.Const lit
		| PT.Var x => (case Env.find (env, x)
		     of NONE => raise Fail("unbound var " ^ Atom.toString x)
		      | SOME x' => NKL.Var x'
		    (* end case *))
		| PT.Tuple es => NKL.Tuple(xformExps(env, es))
		| PT.App(f, args) => (case Env.find (env, f)
		     of NONE => raise Fail("unbound function " ^ Atom.toString f)
		      | SOME f' => NKL.App(f', xformExps(env, args))
		    (* end case *))
		| PT.Prim(p, args) => NKL.App(p, xformExps(env, args))
		| PT.Let((x, ty), e1, e2) => let
		    val e1' = xformExp (env, e1)
		    val (env, x') = fresh (env, x)
		    in
		      NKL.Let(x', e1', xformExp(env, e2))
		    end
		| PT.If(e1, e2, e3) =>
		    NKL.If(xformExp(env, e1),
		      xformExp(env, e2),
		      xformExp(env, e3))
		| PT.PArray es => NKL.PArray(xformExps(env, es))
		| PT.ForEach(e, gens, optCond) => let
		  (* first, we simplify the generators to have the form
		   * "x in xs", where "xs" is a variable.
		   *)
		    fun xformGen (PT.Gen(x, e), (env', binds, gens)) = let
			  val (env', x') = fresh(env', x)
			  in
			    case xformExp (env, e)
			     of NKL.Var xs =>
				  (env', binds, NKL.Gen(x', xs)::gens)
			      | e' => let
				  val xs = Var.new(Atom.toString x ^ "s")
				  val b = (xs, e')
				  val g = NKL.Gen(x', xs)
				  in
				    (env', b::binds, g::gens)
				  end
			    (* end case *)
			  end
		    val (env', binds, gens) =
			  List.foldr xformGen (env, [], []) gens
		    val e' = xformExp (env', e)
		    fun mk ([], e', gens) = NKL.ForEach(e', gens)
		      | mk ((x, e)::r, e', gens) =
			  NKL.Let(x, e, mk(r, e', gens))
		    in
		      case optCond
		       of NONE => mk (binds, e', gens)
			| SOME b => let
			  (* c.f. Keller 4.1.1 *)
			    val b' = xformExp (env', b)
			    val fs = Var.new "fs"
			    val (packs, gens') = let
				  fun f (NKL.Gen(x, xs), (ps, gs)) = let
					val xs' = Var.new(Var.name xs ^ "'")
					val pack = (xs', NKL.App(Prim.pack, [NKL.Var xs, NKL.Var fs]))
					val gen = NKL.Gen(x, xs')
					in
					  (pack::ps, gen::gs)
					end
				  in
				    List.foldr f ([], []) gens
				  end
			    in
			      mk (binds @ (fs, NKL.ForEach(b', gens)) :: packs,
				e', gens')
			    end
		      (* end case *)
		    end
	      (* end case *))
	  and xformExps (env, es) = List.map (fn e => xformExp(env, e)) es
	  in
	    NKL.Prog(List.map xformFun fds, xformExp (topEnv, e))
	  end


  end

