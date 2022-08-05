(* xform.sml
 *
 * COPYRIGHT (c) 2007 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure XForm =
  struct

    local
      open KL
      structure VMap = RedBlackMapFn (
	struct
	  type ord_key = var
	  val compare = Var.compare
	end)
      structure VSet = RedBlackSetFn (
	struct
	  type ord_key = var
	  val compare = Var.compare
	end)
    (* return the set of variables bound in a list of generators *)
      fun boundVars [] = VSet.empty
	| boundVars ((vs, _)::r) = VSet.addList(boundVars r, vs)
    (* return the free variables of an expression *)
      fun freeVars (Const _) = VSet.empty
	| freeVars (Var x) = VSet.singleton x
	| freeVars (Tuple es) =
	    List.foldl
	     (fn (e, fv) => VSet.union(freeVars e, fv))
		VSet.empty es
	| freeVars (App(f, es)) =
	    List.foldl
	     (fn (e, fv) => VSet.union(freeVars e, fv))
		VSet.empty es
	| freeVars (Let(x, e1, e2)) =
	    VSet.union(freeVars e1,
	      VSet.difference(freeVars e2, VSet.singleton x))
	| freeVars (If(e1, e2, e3)) =
	    VSet.union(freeVars e1, VSet.union(freeVars e2, freeVars e3))
	| freeVars (Vector es) =
	    List.foldl
	     (fn (e, fv) => VSet.union(freeVars e, fv))
		VSet.empty es
	| freeVars (ForEach of (exp * generator list)
	| freeVars (Lambda of (var list * exp)
	| freeVars (Compose e1, e2) = VSet.union(freeVars e1, freeVars e2)
    (* apply builtin functions *)
      fun prim (p, args) = App(FId(p, false), args)
    (* indexed variable *)
      fun vari (s, i) = V.new(s ^ Int.toString i)
    in

      fun dist (c, n) = prim("dist", [c, n])

      fun ## e = prim("#", [e])

      fun appP (x, y) = prim("P", [x, y])
      fun appF x = prim("F", [x])
      fun appS x = prim("S", [x])

      fun empty x = prim("empty", x)
      fun combine (a, b, c) = prim("combine", [a, b, c])
      fun pack (a, b) = prim("pack", [a, b])
      fun zip l = prim("zip" ^ Int.toString(length l), l)
      fun not' e = App(FId("not", true), [e])

      fun mkLet ([], e) = e
	| mkLet (b::bs, e) = Let(b, mkLet(bs, e))

      fun freshForEach (e, g) = ??

      fun flatten (Proc(fds, e)) = let
	    val workList = ref fds
	    fun liftApply ((f, false), es) = (
(* FIXME: add f to worklist *)
		  App((f, true), es))
	      | liftApply (f, es) =
		(* 4.8 *)
		  appP(App(f, List.map appF es), appS(hd es))
	    fun flattenFDef (FDef(f, params, e)) = ??
	    fun xform (e as Const _) = e
	      | xform (e as Var _) = e
	      | xform (App(f, args)) = App(f, List.map xform args)
	      | xform (Let(x, e1, e2)) = Let(x, xform e1, xform e2)
	      | xform (If(e1, e2, e2)) = If(xform e1, xform e2, xform e3)
	      | xform (Tuple es) = Tuple(List.map xform es)
	      | xform (Vector es) = Vector(List.map xform es)
	      | xform (ForEach(e, binds) = let
		  val binds as (x, xs)::_) =
			List.map (fn (x, xs) => (x, xform xs)) binds
		  in
		    case xform e
		    (* 4.1 *)
		     of Const c => dist(Const c, ## xs)
		    (* 4.2 *)
		      | Var x => (case List.find (fn (y, _) => (x = y)) binds
			   of SOME(y, e) => e
			    | NONE => dist(Var x, ##xs)
			  (* end case *))
		    (* 4.3 *)
		      | App(f, args) =>
			  If(empty(xs),
			    Vector[],
			    liftApply (f, List.map fn e => ForEach(e, binds)))
		    (* 4.4 *)
		      | Let(x, e1, e2) => let
			  val y = V.new "y"
			  val ys = V.new "ys"
			  in
			    Let(ys, ForEach(e1, g), ForEach(e2, ([y], ys)::binds))
			  end
		    (* 4.5 *)
		      | If(e1, e2, e3) => let
			  val fs = V.new "fs"
			  fun mkPack (x, xs) = 
			  in
			    mkLet([
			      (fs, freshForEach(e1, binds)),
				??
			      ], combine(Var g, Var h, Var fs))
			  end
		      | Tuple[] => dist(Tuple[], ## xs)
		    (* 4.6 *)
		      | Tuple es => let
			  fun f (e, (i, ess, bs)) = let
			          val es = vari("es", i)
				  in
				    (i+1, Var es::ess, (es, freshForEach(e, binds))::bs)
				  end
			  val (_, ess, bs) = List.foldl f (1, [], []) es
			  in
			    mkLet(List.rev bs, zip ess)
			  end			
		      | Vector es =>
		      | ForEach _ => raise Fail "unexpected nested ForEach"
		    (* end case *)
		  end
	    val e' = xform (VMap.empty, e)
	    fun iterate fdefs = (case !workList
		   of [] => Prog(List.rev fdefs, e')
		    | (fd::r) => (
			workList := r;
			iterate (flattenFDef fd :: fdefs))
		  (* end case *))
	    in
	      iterate []
	    end (* flatten *)

    end (* local open KL *)

  end
