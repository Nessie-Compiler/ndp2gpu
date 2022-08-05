(* flatten.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Flatten : sig

    val xform : NKL.program -> FKL.program

  end = struct

    structure N = NKL	(* source language *)
    structure F = FKL	(* target language *)

    structure VSet = Var.Set
    structure VMap = Var.Map

    (* return true if an expression does not refer to a variable in the
     * given set.
     *)
      fun doesNotContain (e, vs) = let
	    fun look (F.Const _) = true
	      | look (F.Var x) = not(VSet.member(vs, x))
	      | look (F.Tuple es) = List.all look es
	      | look (F.App(_, es)) = List.all look es
	      | look (F.Let(_, e1, e2)) = look e1 andalso look e2
	      | look (F.If(e1, e2, e3)) = look e1 andalso look e2 andalso look e3
	      | look (F.PArray es) = List.all look es
	    in
	      look e
	    end

  (* add lifted versions of functions that are used in parallel contexts *)
    fun liftFunctions (N.Prog(fds, e)) = let
	(* create a mapping from function names to their definitions *)
	  val defMap = List.foldl (fn ((fd as N.Fun(f, _, _)), m) => VMap.insert(m, f, fd)) VMap.empty fds
	(* the set of functions to be lifted *)
	  val liftedFuns = ref VSet.empty
	  fun doFun (N.Fun(_, _, e)) = doExp (false, e)
	  and doExp (inPar, e) = (case e
		 of N.Const _ => ()
		  | N.Var _ => () (* assuming first-order language *)
		  | N.Tuple es => doExps (inPar, es)
		  | N.App(f, args) => (
		      if inPar then lift f else ();
		      doExps (inPar, args))
		  | N.Let(x, e1, e2) => (doExp(inPar, e1); doExp(inPar, e2))
		  | N.If(e1, e2, e3) => doExps(inPar, [e1, e2, e3])
		  | N.PArray es => doExps (inPar, es)
		  | N.ForEach(e, gens) => doExp (true, e)
		(* end case *))
	  and doExps (inPar, es) = List.app (fn e => doExp(inPar, e)) es
	  and lift f = if Var.isPrim f orelse VSet.member(!liftedFuns, f)
		then () (* nothing to do *)
		else let
		  val SOME(N.Fun(_, _, e)) = VMap.find(defMap, f)
		  in
		    liftedFuns := VSet.add(!liftedFuns, f);
		    doExp (true, e)
		  end
	(* find the functions to lift *)
	  val _ = (List.app doFun fds; doExp(false, e))
	(* generate definitions for the lifted functions *)
	  fun mkLiftedFun (f, fds) = let
		val SOME(N.Fun(_, args, e)) = VMap.find(defMap, f)
(* FIXME: we should create an alpha converted version of args and e *)
		val (args', gens) = List.foldr
		      (fn (x, (xss, gs)) => let
			  val xs = Var.new(Var.name x ^ "s")
			  in (xs::xss, N.Gen(x, xs)::gs) end
			) ([], []) args
		in
		  N.Fun(Var.lift f, args', N.ForEach(e, gens))::fds
		end
	  in
	    List.app doFun fds;
	    N.Prog (VSet.foldl mkLiftedFun fds (!liftedFuns), e)
	  end

  (* apply builtin functions *)
    fun prim (p, args) = F.App(p, args)

  (* indexed variable *)
    fun vari (s, i) = Var.new(s ^ Int.toString i)

    fun dist (c, n) = prim(Prim.dist, [c, n])

    fun ## e = prim(Prim.##, [e])

    fun appP (x, y) = prim(Prim.P, [x, y])
    fun appF x = prim(Prim.F, [x])
    fun appS x = prim(Prim.S, [x])

    fun mul (a, b) = prim(Prim.*, [a, b])

    fun empty x = prim(Prim.empty, [x])
    fun combine (a, b, c) = prim(Prim.combine, [a, b, c])
    fun pack (a, b) = prim(Prim.pack, [a, b])
    fun zip l = prim(Var.new("zip" ^ Int.toString(length l)), l)
    fun notLifted e = prim(Prim.not', [e])
    fun upto (a, b, c) = prim(Prim.upto, [a, b, c])
    fun default_permute (a, b, c) = prim(Prim.default_permute, [a, b, c])

    fun mkLet ([], e) = e
      | mkLet ((x, e)::bs, e') = F.Let(x, e, mkLet(bs, e'))

      fun flatten (N.Prog(fds, e)) = let
	    fun liftApply (f, es) = if Var.lifted f
		(* 4.8 *)
		  then appP(F.App(f, List.map appF es), appS(hd es))
		  else let
		    val f' = Var.lift f
		    in
		      F.App(f', es)
		    end
	    fun xform (N.Const c) = F.Const c
	      | xform (N.Var v) = F.Var v
	      | xform (N.App(f, args)) = F.App(f, List.map xform args)
	      | xform (N.Let(x, e1, e2)) = F.Let(x, xform e1, xform e2)
	      | xform (N.If(e1, e2, e3)) = F.If(xform e1, xform e2, xform e3)
	      | xform (N.Tuple es) = F.Tuple(List.map xform es)
	      | xform (N.PArray es) = F.PArray(List.map xform es)
	      | xform (N.ForEach(e, gens)) = 
		  xformForEach (xform e, List.map (fn (N.Gen(x, xs)) => (x, xs)) gens)
	  (* transform a parallel comprehension after having transformed the body
	   * and generators.  Each case corresponds to a subsection in Keller's
	   * dissertation; section numbers are given as comments.
	   *)
	    and xformForEach (e, gens as ((x, xs)::_)) =
		  if doesNotContain(e, VSet.fromList(List.map #1 gens))
		    then dist(e, ##(F.Var xs)) (* 4.1 and 4.3.7 *)
		    else (case e
		    (* 4.1 *)
		     of F.Const _ => raise Fail "unexpected const" (* covered above *)
		    (* 4.2 *)
		      | F.Var x => (case List.find (fn (y, _) => Var.same(x, y)) gens
			   of SOME(y, ys) => F.Var ys
			    | NONE => raise Fail "unexpected const" (* covered above *)
			  (* end case *))
		    (* 4.3 *)
		      | F.App(f, args) =>
			(* let's assume that primitive operators handle the empty array case correctly *)
			  if Var.isPrim f
			    then liftApply (f, List.map (fn e => xformForEach(e, gens)) args)
			    else F.If(empty(F.Var xs),
			      F.PArray[],
			      liftApply (f, List.map (fn e => xformForEach(e, gens)) args))
		    (* 4.4 *)
		      | F.Let(x, e1, e2) => let
			  val v = Var.new "v"
			  val vs = Var.new "vs"
			  in
			    F.Let(vs,
			      xformForEach (e1, gens),
			      xformForEach (e2, (v, vs)::gens))
			  end
		    (* 4.5 *)
		      | F.If(cond, e1, e2) => let
			  val fs = Var.new "fs"
			  val nfs = Var.new "nfs"
			  fun mkPack (t, b) ((x, xs), (i, gens, binds)) = let
				val tmp = vari(t, i)
				in
				  (i+1, (x, tmp)::gens, (tmp, pack(F.Var xs, F.Var b))::binds)
				end
			(* values for else branch *)
			  val (_, elseGens, binds) = List.foldl (mkPack ("h", nfs)) (0, [], []) gens
			(* values for then branch *)
			  val (_, thenGens, binds) = List.foldl (mkPack ("g", fs)) (0, [], binds) gens
			(* result variables *)
			  val thenRes = Var.new "g"
			  val elseRes = Var.new "h"
			  in
			    mkLet(
			      (fs, xformForEach(cond, gens))
				:: (nfs, notLifted(F.Var fs))
				:: binds @ [
				  (thenRes, xformForEach(e1, thenGens)),
				  (elseRes, xformForEach(e2, elseGens))
				],
			      combine(F.Var thenRes, F.Var elseRes, F.Var fs))
			  end
		    (* 4.1 *)
		      | (e as F.Tuple[]) => raise Fail "unexpected const" (* covered above *)
		      | F.Tuple[e] => xformForEach(e, gens)
		    (* 4.6 *)
		      | F.Tuple es => let
			  fun f (e, (i, ess, bs)) = let
				  val es = vari("es", i)
				  in
				    (i+1, F.Var es::ess, (es, xformForEach(e, gens))::bs)
				  end
			  val (_, ess, bs) = List.foldl f (1, [], []) es
			  in
			    mkLet(List.rev bs, zip ess)
			  end
		    (* 4.7 *)
		      | F.PArray[] => raise Fail "unexpected const" (* covered above *)
		      | F.PArray es => let
			  val k = F.Const(Literal.int(length es))
			  fun f (e, (i, ebs, ibs)) = let
				val esi = vari("es", i)
				val isi = vari("is", i)
				val eb = (esi, xformForEach (e, gens))
				val ib = (isi, upto(F.Const(Literal.int i), k, mul(k, ##(F.Var xs))))
				in
				  (i+1, eb::ebs, ib::ibs)
				end
			  val (_, ebs, ibs) = List.foldl f (0, [], []) es
(* FIXME: need to complete this code *)
			  in
			    mkLet (
			      List.revAppend(ebs, List.revAppend(ibs, [])),
			      appP (F.Var(Var.new "tsk"), F.Var(Var.new "s")))
			  end
		  (* case end *))
	    fun flattenFDef (N.Fun(f, args, e)) = F.Fun(f, args, xform e)
	    in
	      F.Prog(List.map flattenFDef fds, xform e)
	    end (* flatten *)

    fun xform prog = flatten (liftFunctions prog)

  end
