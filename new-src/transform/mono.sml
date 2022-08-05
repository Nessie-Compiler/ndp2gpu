(* mono.sml
 *
 * COPYRIGHT (c) 2008 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Convert the polymorphic program to a monomorphic program.
 *)

structure Mono : sig

    val transform : AST.program -> AST.program

    val verbose : bool ref

  end = struct

    structure VarTbl = Var.Tbl
    structure VarSet = Var.Set
    structure TU = TypeUtil

    val verbose = ref false

    fun pr msg = if !verbose
	  then print(concat msg)
	  else ()

  (* A momomorphic instance of a datatype or variable *)
    type 'a instance = (AST.ty list * 'a)

  (* search a list of instances for a match *)
    fun findInstance (tys : AST.ty list, instances) =
	  List.find (fn (tys', _, _) => ListPair.all TU.same (tys, tys')) instances

  (* environment for rewriting *)
    datatype env = E of {
	tyMap : AST.ty TyVar.Map.map,	(* the type-variable to type mapping *)
	varMap : AST.var Var.Map.map	(* the variable renaming map *)
      }

    val emptyEnv = E{tyMap=TyVar.Map.empty, varMap=Var.Map.empty}

    fun insertTy (E{tyMap, varMap}, tv, ty) = E{
	    tyMap = TyVar.Map.insert(tyMap, tv, ty),
	    varMap = varMap
	  }

    fun insertTys (E{tyMap, varMap}, tvs, tys) = E{
	    tyMap = ListPair.foldlEq
		(fn (tv, ty, m) => TyVar.Map.insert(m, tv, ty))
		  tyMap (tvs, tys),
	    varMap = varMap
  	  }

    fun insertVar (E{tyMap, varMap}, x, x') = E{
	    tyMap = tyMap,
	    varMap = Var.Map.insert(varMap, x, x')
	  }

    fun renameVar (E{varMap, ...}, x) = (case Var.Map.find(varMap, x)
	   of NONE => x
	    | SOME x' => x'
	  (* end case *))

    fun cvtTy (E{tyMap, ...}, ty) = TU.applySubst(tyMap, ty)
    fun cvtTys (env, tys) = List.map (fn ty => cvtTy(env, ty)) tys

  (* create a copy of a variable that has a converted type; add a mapping from
   * the old variable to the new one to the environment.
   *)
    fun copyVar (env, x) = let
	  val x' = Var.new(Var.nameOf x, cvtTy(env, Var.monoTypeOf x))
	  in
	    (x', insertVar(env, x, x'))
	  end

  (* is a variable polymorphic? *)
    fun isPoly x = (case Var.typeOf x
	   of AST.TyScheme([], _) => false
	    | _ => true
	  (* end case *))

    fun transform prog = let
	(* hash table for tracking instances polymorphic variables. *)
	  val varTbl = VarTbl.mkTable (128, Fail "varTbl")
	(* specialize an instance of a polymorphic variable *)
	  fun specializeVar (x, tys) = let
		val instances = VarTbl.lookup varTbl x
		in
		  case findInstance (tys, instances)
		   of NONE => let
			val newTy = TU.applyScheme(Var.typeOf x, tys)
			val x' = Var.new(Var.nameOf x, newTy)
			in
			  pr [
			      "specializeVar ", Var.toString x', " = ",
			      Var.toString x, "[",
			      String.concatWith ", " (List.map TU.toString tys),
			      "]\n"
			    ];
			  VarTbl.insert varTbl (x, (tys, x', ref false)::instances);
			  x'
			end
		    | SOME(_, x', _) => x'
		  (* end case *)
		end
	(* create a copy of an expression, while converting the types of variables
	 * as needed.  We assume that the expression has already been transformed
	 * so that any nested polymorphic bindings have been expanded.
	 *)
	  fun copyExp (env, e) = (case e
		 of (AST.LetExp(pat, e1, e2)) => let
		      val (pat, env) = copyPat (env, pat)
		      in
			AST.LetExp(pat, copyExp(env, e1), copyExp(env, e2))
		      end
		  | (AST.FunExp(fbs, scope)) => let
		    (* we are copying this expression, so it has already had
		     * its polymorphic bindings instantiated.
		     *)
		      (* first: rename the functions *)
			fun renameFn (AST.FB(f, _, _), env) = #2(copyVar(env, f))
			val env = List.foldl renameFn env fbs
		      (* now copy the function bindings *)
			fun copyFB (AST.FB(f, x, body)) = let
			      val (x, env) = copyVar(env, x)
			      in
				AST.FB(renameVar(env, f), x, copyExp(env, e))
			      end
			in
			  AST.FunExp(List.map copyFB fbs, copyExp(env, scope))
			end
		  | (AST.IfExp(e1, e2, e3)) =>
		      AST.IfExp(copyExp(env, e1), copyExp(env, e2), copyExp(env, e3))
		  | (AST.CaseExp(e, rules)) => let
		      fun copyRule (pat, e) = let
			    val (pat, env) = copyPat(env, pat)
			    val e = copyExp(env, e)
			    in
			      (pat, e)
			    end
		      in
			AST.CaseExp (copyExp(env, e), List.map copyRule rules)
		      end
		  | (AST.ApplyExp(e1, e2)) =>
		      AST.ApplyExp(copyExp(env, e1), copyExp(env, e2))
		  | (AST.TupleExp es) =>
		      AST.TupleExp(copyExps (env, es))
		  | (AST.PArrayExp es) =>
		      AST.PArrayExp(copyExps (env, es))
		  | (AST.PForallExp(e1, pat, e2, optPred)) => let
		      val e2 = copyExp (env, e2)
		      val (pat, env) = copyPat(env, pat)
		      val e1 = copyExp(env, e1)
		      val optPred = Option.map (fn e => copyExp(env, e)) optPred
		      in
			AST.PForallExp(e1, pat, e2, optPred)
		      end
		  | (AST.DConExp(dc, tys)) => AST.DConExp(dc, cvtTys(env, tys))
		  | (AST.VarExp(x, [])) => AST.VarExp(renameVar(env, x), [])
		  | (AST.VarExp(x, tys)) =>
		      AST.VarExp(specializeVar (renameVar(env, x), cvtTys (env, tys)), [])
		  | (e as AST.ConstExp _) => e
		  | (AST.SeqExp(e1, e2)) => AST.SeqExp(copyExp(env, e1), copyExp(env, e2))
		(* end case *))
	  and copyExps (env, es) = List.map (fn e => copyExp(env, e)) es
	  and copyPat (env, pat) = (case pat
		 of AST.ConPat(dc, tys, pat) => let
		      val (pat, env) = (case pat
			     of SOME p => let
				  val (p, env) = copyPat (env, p)
				  in
				    (SOME p, env)
				  end
			      | NONE => (NONE, env)
			    (* end case *))
		      in
			(AST.ConPat(dc, cvtTys(env, tys), pat), env)
		      end
		  | AST.TuplePat pats => let
		      val (pats, env) = copyPats (env, pats)
		      in
			(AST.TuplePat pats, env)
		      end
		  | AST.VarPat x => let
		      val (x', env) = copyVar(env, x)
		      in
			(AST.VarPat x', env)
		      end
		  | AST.ConstPat _ => (pat, env)
		(* end case *))
	  and copyPats (env, pats) =  let
		fun cpy (p, (ps, env)) = let
		      val (p, env) = copyPat(env, p)
		      in
			(p::ps, env)
		      end
		in
		  List.foldr cpy ([], env) pats
		end
	(* transform an expression by instantiating polymorphic variables and
	 * data constructors to their type arguments and creating new monomorphic
	 * instances of these variables and datatypes.
	 *)
	  fun xformExp e = (case e
		 of (AST.LetExp(pat, e1, e2)) =>
(* NOTE: we don't have polymorphic let bindings yet! *)
		      AST.LetExp(pat, xformExp e1, xformExp e2)
		  | (AST.FunExp(fbs, scope)) => let
		      fun addEntry (AST.FB(f, _, _)) =
			    if isPoly f
			      then (VarTbl.insert varTbl (f, []); true)
			      else false
		      val (polyFuns, monoFuns) = List.partition addEntry fbs
		      val scope = xformExp scope
		      fun xformFB (AST.FB(f, x, body)) = AST.FB(f, x, xformExp body)
		      val monoFuns = List.map xformFB monoFuns
		      val polyFuns = List.map xformFB polyFuns
		      in
			case polyFuns
			 of [] => AST.FunExp(monoFuns, scope)
			  | _ => let
			      val fbs = ref []
			      fun doFun (AST.FB(f, x, body)) = let
				    val AST.TyScheme(tyParams, funTy) = Var.typeOf f
				    fun doInstance (_, _, ref true) = true
				      | doInstance (tys, f', done) = let
					(* instantiate the type parameters *)
					  val env = insertTys(emptyEnv, tyParams, tys)
					  val (x', env) = copyVar (env, x)
					  in
					    fbs := AST.FB(f', x', copyExp(env, body)) :: !fbs;
					    done := true;
					    false
					  end
				    in
				    (* instantiate any unexpanded instances; return true
				     * if we are done
				     *)
				      List.all doInstance (VarTbl.lookup varTbl f)
				    end
			    (* create instances of the polymorphic functions until they
			     * are all done.
			     *)
			      fun lp () = if List.all doFun polyFuns
				    then ()
				    else lp()
			      in
				lp();
				AST.FunExp(List.revAppend(!fbs, monoFuns), scope)
			      end
			(* end case *)
		      end
		  | (AST.IfExp(e1, e2, e3)) =>
		      AST.IfExp(xformExp e1, xformExp e2, xformExp e3)
		  | (AST.CaseExp(e, rules)) => let
		      fun xformRule (pat, e) = (pat, xformExp e)
		      in
			AST.CaseExp (xformExp e, List.map xformRule rules)
		      end
		  | (AST.ApplyExp(e1, e2)) => AST.ApplyExp(xformExp e1, xformExp e2)
		  | (AST.TupleExp es) => AST.TupleExp(xformExps es)
		  | (AST.PArrayExp es) => AST.PArrayExp(xformExps es)
		  | (AST.PForallExp(e1, pat, e2, optPred)) => let
		      val e2 = xformExp e2
		      val e1 = xformExp e1
		      val optPred = Option.map (fn e => xformExp e) optPred
		      in
			AST.PForallExp(e1, pat, e2, optPred)
		      end
		  | (e as AST.DConExp _) => e
		  | (e as AST.VarExp(x, [])) => e
		  | (AST.VarExp(x, tys)) => AST.VarExp(specializeVar (x, tys), [])
		  | (e as AST.ConstExp _) => e
		  | (AST.SeqExp(e1, e2)) => AST.SeqExp(xformExp e1, xformExp e2)
		(* end case *))
	  and xformExps es = List.map xformExp es
	  in
	    xformExp prog
	  end

  end
