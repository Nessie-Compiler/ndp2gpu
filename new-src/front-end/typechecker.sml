(* typechecker.sml
 *
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * CMSC 22610 Sample code (Winter 2007)
 *)

structure Typechecker : sig

    val check : ParseTree.program -> AST.program option

  end = struct

    structure PT = ParseTree
    structure E = Env
    structure U = Unify
    structure TU = TypeUtil

    val atos = Atom.toString

    fun error (lnum, msg) = Error.say (
	  "Error [" :: !Error.sourceFile :: ":" :: Int.toString lnum :: "] "
	    :: (msg @ ["\n"]))

    fun mismatch (lnum, msg, ty1, ty2) = error (lnum, [
	    "type mismatch in ", msg, "\n",
	    "    Expected: ", TU.toString ty1, "\n",
	    "    Found:    ", TU.toString ty2
    ])

  (* create a single-parameter lambda from one that has a list of parameters *)
    fun mkLambda (f, AST.VarPat x, e) = AST.FB(f, x, e)
      | mkLambda (f, pat, e) = let
	  val AST.TyScheme(_, AST.FunTy(argTy, _)) = Var.typeOf f
	  val arg = Var.new (Atom.atom "arg", argTy)
	  in
	    AST.FB(f, arg, AST.CaseExp(AST.VarExp(arg, []), [(pat, e)]))
	  end

  (* "smart" tuple type constructor *)
    fun mkTupleTy [] = Basis.unitTy
      | mkTupleTy [ty] = ty
      | mkTupleTy tys = AST.TupleTy tys

  (* "smart" tuple pattern constructor *)
    fun mkTuplePat [p] = p
      | mkTuplePat pats = AST.TuplePat pats

  (* a type expression for when there is an error *)
    val bogusTy = AST.TupleTy[]

  (* an expression/type pair for when there is an error *)
    val bogusExp = (AST.TupleExp[], bogusTy)

  (* a pattern for when there is an error *)
    val bogusPat = AST.TuplePat[]

  (* an atomic pattern for when there is an error *)
    val bogusAPat = AST.ConstPat(Literal.int 0)

  (* typecehck a literal *)
    fun chkLit (loc, Literal.Int i) =
	  if ((i < ~0x40000000) orelse (0x3FFFFFFF < i))
	    then (
	      error(loc, ["integer literal ", IntInf.toString i, " out of range"]);
	      (Literal.int 0, Basis.intTy))
	    else (Literal.Int i, Basis.intTy)
      | chkLit (_, Literal.Str s) = (Literal.Str s, Basis.stringTy)

  (* typecheck value declarations as described in Section 6.6 *)
    fun chkValDcl (loc, depth, ve, decl, next) = (case decl
	   of PT.MarkVDecl{lnum, tree} => chkValDcl (lnum, depth, ve, tree, next)
	    | PT.ValVDecl(apats, e) => let
		val (apats', ve', lhsTy) = chkAtomicPats(loc, depth, ve, apats)
		val (e', rhsTy) = chkExp (loc, depth, ve, e)
		val (e'', resTy) = next ve'
		in
		  if not(U.unify(lhsTy, rhsTy))
		    then mismatch (loc, "val binding", lhsTy, rhsTy)
		    else ();
		  (AST.LetExp(apats', e', e''), resTy)
		end
	    | PT.FunVDecl fbs => let
		val depth' = depth+1
	      (* create variable bindings for the functions *)
		fun bindFun (fb, (fs, names)) = (case fb
		       of PT.MarkFunct{lnum, tree} => bindFun (tree, (fs, names))
			| PT.Funct(f, _, _) => let
			    val f' = Var.new(f,
				  AST.FunTy(
				    AST.MetaTy(MetaVar.new depth'),
				    AST.MetaTy(MetaVar.new depth')))
			    in
			      if AtomSet.member(names, f)
				then (
				  error(loc, [
				      "duplicate name ", Atom.toString f,
				      "in function binding"
				    ]);
				  (f'::fs, names))
				else (f'::fs, AtomSet.add(names, f))
			    end
		      (* end case *))
		val (fs, _) = List.foldr bindFun ([], AtomSet.empty) fbs
	      (* insert the function variables into an environment for checking
	       * the function bodies.
	       *)
	        val ve' = List.foldl
		      (fn (f, ve) => E.insert(ve, Var.nameOf f, E.Var f))
			ve fs
	      (* typecheck the functions *)
		fun chkFun loc (fb, fbs) = (case fb
		       of PT.MarkFunct{lnum, tree} => chkFun lnum (tree, fbs)
			| PT.Funct(f, params, body) => let
			    val SOME(E.Var f') = E.find(ve', f)
			    val AST.TyScheme(_, funTy) = Var.typeOf f'
			    val (params', ve'', paramTy) =
				  chkAtomicPats (loc, depth', ve', params)
			    val (body', bodyTy) = chkExp (loc, depth', ve'', body)
			    in
			      if not(U.unify(funTy, AST.FunTy(paramTy, bodyTy)))
				then error(loc, ["type mismatch in function ", Atom.toString f])
				else ();
			      mkLambda(f', params', body') :: fbs
			    end
		      (* end case *))
		val fbs' = List.foldr (chkFun loc) [] fbs
	      (* close over the types of the functions and build an environment
	       * for checking the scope of the declaration.
	       *)
		fun close (f, ve) = (
		      Var.closeTypeOf (depth, f);
		      E.insert(ve, Var.nameOf f, E.Var f))
	        val ve' = List.foldl close ve fs
		val (e', resTy) = next ve'
		in
		  (AST.FunExp(fbs', e'), resTy)
		end
	  (* end case *))

  (* typecheck expressions as described in Section 6.8 *)
    and chkExp (loc, depth, ve, exp) = (case exp
	   of PT.MarkExp{lnum, tree} => chkExp (lnum, depth, ve, tree)
	    | PT.LetExp(valDcls, exp) => let
		fun chkDcls ([], ve) = chkExp (loc, depth, ve, exp)
		  | chkDcls (vd::vds, ve) =
		      chkValDcl (loc, depth, ve, vd, fn ve' => chkDcls(vds, ve'))
		in
		  chkDcls (valDcls, ve)
		end
	    | PT.IfExp(e1, e2, e3) => let
		val (e1', ty1) = chkExp (loc, depth, ve, e1)
		val (e2', ty2) = chkExp (loc, depth, ve, e2)
		val (e3', ty3) = chkExp (loc, depth, ve, e3)
		in
		  if not(U.unify(ty1, Basis.boolTy))
		    then error(loc, ["type of conditional not bool"])
		    else ();
		  if not(U.unify(ty2, ty3))
		    then (
		      error(loc, ["types of then and else clauses must match"]);
		      bogusExp)
		    else (AST.IfExp(e1', e2', e3'), ty2)
		end
	    | PT.CaseExp(e, cases) => let
		val (e', argTy) = chkExp (loc, depth, ve, e)
		val resTy = AST.MetaTy(MetaVar.new depth)
		val matches = List.map
		      (fn m => chkMatch(loc, depth, ve, argTy, resTy, m))
			cases
		in
		  (AST.CaseExp(e', matches), resTy)
		end
	    | PT.AndAlsoExp(e1, e2) => let
		val (e1', ty1) = chkExp (loc, depth, ve, e1)
		val (e2', ty2) = chkExp (loc, depth, ve, e2)
		in
		  if not(U.unify(ty1, Basis.boolTy) andalso U.unify(ty2, Basis.boolTy))
		    then error(loc, ["arguments of andalso must have type bool"])
		    else ();
		  (AST.IfExp(e1', e2', AST.DConExp(Basis.boolFalse, [])), Basis.boolTy)
		end
	    | PT.OrElseExp(e1, e2) => let
		val (e1', ty1) = chkExp (loc, depth, ve, e1)
		val (e2', ty2) = chkExp (loc, depth, ve, e2)
		in
		  if not(U.unify(ty1, Basis.boolTy) andalso U.unify(ty2, Basis.boolTy))
		    then error(loc, ["arguments of orelse must have type bool"])
		    else ();
		    (AST.IfExp(e1', AST.DConExp(Basis.boolTrue, []), e2'), Basis.boolTy)
		end
	    | PT.BinaryExp(e1, bop, e2) => let
		val (e1', ty1) = chkExp (loc, depth, ve, e1)
		val (e2', ty2) = chkExp (loc, depth, ve, e2)
		fun mkApp rator = AST.ApplyExp(AST.VarExp(rator, []), AST.TupleExp[e1', e2'])
		fun chkApp (mkRator, tyScheme) = let
		      val (tys, AST.FunTy(argTy, resTy)) = TU.instantiate (depth, tyScheme)
		      in
			if not(U.unify(argTy, AST.TupleTy[ty1, ty2]))
			  then error(loc, ["type mismatch for operator ", Atom.toString bop])
			  else ();
			(AST.ApplyExp(mkRator tys, AST.TupleExp[e1', e2']), resTy)
		      end
		in
(* FIXME: handle neq too and postpone fixing type *)
		  if Atom.same(bop, BasisNames.eq)
		    then if not(U.unify(ty1, ty2))
		      then (
			error(loc, ["type mismatch for operator ="]);
			(AST.TupleExp[], Basis.boolTy))
		      else if TU.same(ty1, Basis.boolTy)
			then (mkApp Basis.boolEq, Basis.boolTy)
		      else if TU.same(ty1, Basis.intTy)
			then (mkApp Basis.intEq, Basis.boolTy)
		      else if TU.same(ty1, Basis.stringTy)
			then (mkApp Basis.stringEq, Basis.boolTy)
			else (
			  error(loc, ["not an equality type"]);
			  (AST.TupleExp[], Basis.boolTy))
		  else if Atom.same(bop, BasisNames.listCons)
		    then chkApp(
		      fn tys => AST.DConExp(Basis.listCons, tys),
		      DataCon.typeOf Basis.listCons)
		    else let
		      val rator = Basis.lookupOp bop
		      in
			chkApp(
			  fn tys => AST.VarExp(rator, tys),
			  Var.typeOf rator)
		      end
		end
	    | PT.ApplyExp(e1, e2) => let
		val (e1', ty1) = chkExp (loc, depth, ve, e1)
		val (e2', ty2) = chkExp (loc, depth, ve, e2)
		val resTy = AST.MetaTy(MetaVar.new depth)
		in
		  if not(U.unify(ty1, AST.FunTy(ty2, resTy)))
		    then error(loc, ["type mismatch in application"])
		    else ();
		  (AST.ApplyExp(e1', e2'), resTy)
		end
	    | PT.ConstExp const => let
		val (const', ty) = chkLit (loc, const)
		in
		  (AST.ConstExp const', ty)
		end
	    | PT.TupleExp[] => (AST.TupleExp[], Basis.unitTy)
	    | PT.TupleExp es => let
		fun chk (e, (es, tys)) = let
		      val (e', ty) = chkExp(loc, depth, ve, e)
		      in
			(e'::es, ty::tys)
		      end
		val (es', tys) = List.foldr chk ([], []) es
		in
		  (AST.TupleExp es', mkTupleTy tys)
		end
	    | PT.PArrayExp es => let
		val elemTy = AST.MetaTy(MetaVar.new depth)
		fun chk e = let
		      val (e', ty) = chkExp(loc, depth, ve, e)
		      in
			if not(U.unify(elemTy, ty))
			  then error (loc, ["type mismatch in parallel array"])
			  else ();
			e'
		      end
		in
		  (AST.PArrayExp(List.map chk es), Basis.parrayTy elemTy)
		end	    
	    | PT.PForallExp(e1, pat, e2, pred) => let
		val (e2', ty2) = chkExp (loc, depth, ve, e2)
		val (pat', ve', patTy) = chkPat(loc, depth, ve, pat)
		val (e1', ty1) = chkExp (loc, depth, ve', e1)
		val pred' = (case pred
		       of NONE => NONE
			| SOME p => let
			    val (p', pTy) = chkExp (loc, depth, ve', p)
			    in
			      if not(U.unify(pTy, Basis.boolTy))
				then error(loc, ["predicate must have bool type"])
				else ();
			      SOME p'
			    end
		      (* end case *))
		in
		  if not(U.unify(Basis.parrayTy patTy, ty2))
		    then (
		      mismatch (loc, "forall", patTy, ty2);
		      bogusExp)
		    else (AST.PForallExp(e1', pat', e2', pred'), Basis.parrayTy ty1)
		end
	    | PT.SeqExp es => let
		fun chk [e] = chkExp(loc, depth, ve, e)
		  | chk (e::r) = let
		      val (e', _) = chkExp (loc, depth, ve, e)
		      val (e'', ty) = chk r
		      in
			(AST.SeqExp(e', e''), ty)
		      end
		in
		  chk es
		end
	    | PT.IdExp x => (case E.find(ve, x)
		 of SOME(E.Con dc) => let
		      val (tys, ty) = TU.instantiate (depth, DataCon.typeOf dc)
		      in
			(AST.DConExp(dc, tys), ty)
		      end
		  | SOME(E.Var x') => let
		      val (mvs, ty) = TU.instantiate (depth, Var.typeOf x')
		      in
			(AST.VarExp(x', mvs), ty)
		      end
		  | NONE => (
		      error(loc, ["undefined identifier ", Atom.toString x]);
		      bogusExp)
		(* end case *))
	  (* end case *))

    and chkMatch (loc, depth, ve, argTy, resTy, match) = (case match
	   of PT.MarkMatch{lnum, tree} => chkMatch(lnum, depth, ve, argTy, resTy, tree)
	    | PT.Match(pat, exp) => let
		val (pat', ve', argTy') = chkPat(loc, depth, ve, pat)
		val (exp', resTy') = chkExp(loc, depth, ve', exp)
		in
		  if not(U.unify(argTy, argTy'))
		    then error(loc, ["type mismatch in case pattern"])
		    else ();
		  if not(U.unify(resTy, resTy'))
		    then error(loc, ["type mismatch in case"])
		    else ();
		  (pat', exp')
		end
	  (* end case *))

    and chkPat (loc, depth, ve, pat) = (case pat
	   of PT.MarkPat{lnum, tree} => chkPat(lnum, depth, ve, tree)
	    | PT.ConPat(conid, pats) => let
		val (pat, ve', ty) = chkAtomicPats (loc, depth, ve, pats)
		in
		  case E.find(ve, conid)
		   of SOME(E.Con dc) => (case TU.instantiate (depth, DataCon.typeOf dc)
			 of (tys, AST.FunTy(argTy, resTy)) => (
			      if not(U.unify(argTy, ty))
				then error(loc, ["type mismatch in constructor pattern"])
				else ();
			      (AST.ConPat(dc, tys, SOME pat), ve', resTy))
			  | _ => (
			      error(loc, [
				  "application of nullary constructor ",
				  Atom.toString conid
				]);
			      (bogusPat, ve', bogusTy))
			(* end case *))
		    | _ => (
			error(loc, ["unbound data constructor ", Atom.toString conid]);
			(bogusPat, ve', bogusTy))
		  (* end case *)
		end
	    | PT.TuplePat pats => let
		val (pat, ve', ty) = chkAtomicPats (loc, depth, ve, pats)
		in
		  (pat, ve', ty)
		end
	    | PT.AtomicPat pat => let
		val (pat', ve', ty) = chkAtomicPat(loc, depth, ve, pat)
		in
		  (pat', ve', ty)
		end
	  (* end case *))

    and chkAtomicPats (loc, depth, ve, apats) = let
	  fun chk (apat, (ve, ps, tys)) = let
		val (apat', ve', ty) = chkAtomicPat (loc, depth, ve, apat)
		in
		  (ve', apat'::ps, ty::tys)
		end
	  val (ve', apats', tys) = List.foldr chk (ve, [], []) apats
	  in
	    (mkTuplePat apats', ve', mkTupleTy tys)
	  end

    and chkAtomicPat (loc, depth, ve, apat) = (case apat
	   of PT.MarkAPat{lnum, tree} => chkAtomicPat (lnum, depth, ve, tree)
	    | PT.WildPat => let
		val ty = AST.MetaTy(MetaVar.new depth)
		val x' = Var.new(Atom.atom "_", ty)
		in
		  (AST.VarPat x', ve, ty)
		end
	    | PT.IdPat x => (case E.find(ve, x)
		 of SOME(E.Con dc) => (case TU.instantiate (depth, DataCon.typeOf dc)
			 of (_, AST.FunTy _) => (
			      error(loc, [
				  "missing arguments to data constructor ",
				  Atom.toString x
				]);
			      (bogusAPat, ve, bogusTy))
			  | (tys, ty) => (AST.ConPat(dc, tys, NONE), ve, ty)
			(* end case *))
		  | _ => let
		      val ty = AST.MetaTy(MetaVar.new depth)
		      val x' = Var.new(x, ty)
		      in
			(AST.VarPat x', E.insert(ve, x, E.Var x'), ty)
		      end
		(* end case *))
	    | PT.ConstPat const => let
		val (const', ty) = chkLit (loc, const)
		in
		  (AST.ConstPat const', ve, ty)
		end
	  (* end case *))

  (* typecheck type expressions as described in Section 6.4 *)
    fun chkTy (loc, te, tve, ty) = (case ty
	   of PT.MarkTy{lnum, tree} => chkTy(lnum, te, tve, tree)
	    | PT.NamedTy(tyArgs, id) => let
		val tyArgs' = List.map (fn ty => chkTy(loc, te, tve, ty)) tyArgs
		in
		  case Env.find(te, id)
		   of SOME(E.TyDef(AST.TyScheme(tvs, ty))) =>
			if (List.length tvs <> List.length tyArgs')
			  then (
			    error(loc, ["arity mismatch for ", atos id]);
			    bogusTy)
			  else TU.substitute (ty, ListPair.zip(tvs, tyArgs'))
		    | SOME(E.TyCon tyc') =>
			if (TyCon.arityOf tyc' <> List.length tyArgs')
			  then (
			    error(loc, ["arity mismatch for ", atos id]);
			    bogusTy)
			  else AST.ConTy(tyArgs', tyc')
		    | NONE => (
			error(loc, ["undefined type constructor ", atos id]);
			bogusTy)
		  (* end case *)
		end
	    | PT.VarTy tv => (case Env.find(tve, tv)
		 of SOME tv' => AST.VarTy tv'
		  | NONE => (error(loc, ["unbound type variable ", atos tv]); bogusTy)
		(* end case *))
	    | PT.TupleTy tys =>
		mkTupleTy(List.map (fn ty => chkTy(loc, te, tve, ty)) tys)
	    | PT.FunTy(ty1, ty2) =>
		AST.FunTy(chkTy(loc, te, tve, ty1), chkTy(loc, te, tve, ty2))
	  (* end case *))

  (* check a list of type variables *)
    fun chkTyVars (loc, tvs) = let
	  fun chk ([], tve, tvs) = (tve, List.rev tvs)
	    | chk (tv::rest, tve, tvs) = let
		val tv' = TyVar.new tv
		in
		  if E.inDomain(tve, tv)
		    then (
		      error (loc, ["duplicate type variable ", Atom.toString tv]);
		      chk (rest, tve, tv'::tvs))
		    else chk (rest, E.insert(tve, tv, tv'), tv'::tvs)
		end
	  in
	    chk (tvs, E.empty, [])
	  end

  (* typecheck top-level declarations as described in Sections 6.2 and 6.3 *)
    fun chkTopDcl (loc, te, ve, decl, next) = (case decl
	   of PT.MarkDecl{lnum, tree} => chkTopDcl (lnum, te, ve, tree, next)
	    | PT.TyDecl(tvs, id, ty) => let
		val (tve, tvs') = chkTyVars (loc, tvs)
		val ty' = chkTy (loc, te, tve, ty)
		in
		  next(E.insert(te, id, E.TyDef(AST.TyScheme(tvs', ty'))), ve)
		end
	    | PT.DataDecl(tvs, id, cons) => let
		val (tve, tvs') = chkTyVars (loc, tvs)
		val tyc = TyCon.newDataTyc(id, tvs')
	      (* update the type environment before checking the constructors so that
	       * recursive types work.
	      *)
		val te = E.insert(te, id, E.TyCon tyc)
		val newCon = DataCon.new tyc
		fun chkCons (_, ids, [], ve, cons) = (ve, List.rev cons)
		  | chkCons (loc', ids, con::rest, ve, cons) = (case con
		       of PT.MarkConDecl{lnum, tree} =>
			    chkCons (lnum, ids, tree::rest, ve, cons)
			| PT.ConDecl(conid, optTy) =>
			    if AtomSet.member(ids, conid)
			      then (
				error (loc', [
				    "duplicate constructor ", Atom.toString conid,
				    " in datatype ", Atom.toString id
				  ]);
				chkCons (loc, ids, rest, ve, cons))
			      else let
				val optTy' = Option.map
				      (fn ty => chkTy(loc, te, tve, ty)) optTy
				val con' = newCon(conid, optTy')
				in
				  chkCons (loc,
				    AtomSet.add(ids, conid), rest,
				    E.insert (ve, conid, E.Con con'), con'::cons)
				end
		      (* end case *))
		val (ve', cons') = chkCons (loc, AtomSet.empty, cons, ve, [])
		in
		  next(te, ve')
		end
	    | PT.ValueDecl valDcl =>
		chkValDcl(loc, 0, ve, valDcl, fn ve' => next(te, ve'))
	  (* end case *))

    fun check (dcls, exp) = let
	  fun chkDcls (te, ve, []) = chkExp(1, 0, ve, exp)
	    | chkDcls (te, ve, d::ds) =
		chkTopDcl (1, te, ve, d, fn (te, ve) => chkDcls (te, ve, ds))
	  in
	    SOME (#1 (chkDcls (Basis.te0, Basis.ve0, dcls)))
	  end

  end
