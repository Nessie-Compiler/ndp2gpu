(* type-util.sml
 *
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * CMSC 22610 Sample code (Winter 2007)
 *
 * Various utility functions for manipulating types.
 *)

structure TypeUtil : sig

    val toString : AST.ty -> string

  (* return the "head-normal form" by pruning an instantiated meta
   * variables.
   *)
    val prune : AST.ty -> AST.ty

    val substitution : (AST.tyvar list * AST.ty list) -> AST.ty TyVar.Map.map

  (* apply a type variable to type substitution to a type.  The substitution
   * is represented as a list of type variable/type pairs.
   *)
    val substitute : (AST.ty * (AST.tyvar * AST.ty) list) -> AST.ty

    val applySubst : AST.ty TyVar.Map.map * AST.ty -> AST.ty

  (* instantiate a type scheme at the given lambda-nesting depth *)
    val instantiate : (int * AST.ty_scheme) -> (AST.ty list * AST.ty)

  (* substitute types for the bound variables of a type scheme *)
    val applyScheme : (AST.ty_scheme * AST.ty list) -> AST.ty

  (* close a type w.r.t. to a set of non-generic variables (i.e., those
   * variables whose depth is less than or equal to the given depth).
   *)
    val closeTy : (int * AST.ty) -> AST.ty_scheme

  (* return true if two types are equal.  Note that this function does not
   * do unification or alpha renaming of meta variables, but it does chase
   * instantiated meta-variable types.
   *)
    val same : (AST.ty * AST.ty) -> bool

  end = struct

    structure MV = MetaVar
    structure TVMap = TyVar.Map
    structure MVMap = MetaVar.Map

    fun toString ty = let
	  fun toS (ty, l) = (case ty
		 of (AST.MetaTy(AST.MVar{stamp, info})) => (case !info
		       of AST.INSTANCE ty' => toS(ty', l)
			| _ => ("'M" ^ Stamp.toString stamp) :: l
		      (* end case *))
		  | (AST.VarTy(AST.TVar{name, ...})) => Atom.toString name :: l
		  | (AST.ConTy([], tyc)) => Atom.toString(TyCon.nameOf tyc) :: l
		  | (AST.ConTy([ty], tyc)) =>
			toSP (ty, " " :: Atom.toString(TyCon.nameOf tyc) :: l)
		  | (AST.ConTy(ty::tys, tyc)) => let
			fun f (ty, l) = ", " :: toS(ty, l)
			in
			  "(" :: toS(ty,
			    List.foldr f
			      (") " :: Atom.toString(TyCon.nameOf tyc) :: l) tys)
			end
		  | (AST.FunTy(ty1 as AST.FunTy _, ty2)) =>
			"(" :: toS(ty1, ") -> " :: toS(ty2, l))
		  | (AST.FunTy(ty1, ty2)) =>
			toS(ty1, " -> " :: toS(ty2, l))
		  | (AST.TupleTy[]) => "unit" :: l
		  | (AST.TupleTy(ty::tys)) => let
			fun f (ty, l) = " * " :: toS(ty, l)
			in
			  "(" :: toS(ty, List.foldr f (")" :: l) tys)
			end
		(* end case *))
	    and toSP (ty as AST.FunTy _, l) = "(" :: toS(ty, ")" :: l)
	      | toSP (ty, l) = toS (ty, l)
	    in
	      concat (toS (ty, []))
	    end

  (* return a string representation of a type (for debugging) *)
    fun toDebugString (AST.MetaTy(AST.MVar{stamp, info})) = (case !info
	   of AST.UNIV d => concat["$", Stamp.toString stamp, "@", Int.toString d]
	    | AST.INSTANCE ty => (
		info := AST.UNIV ~1;
		concat["$", Stamp.toString stamp, " == ", toDebugString ty]
		  before info := AST.INSTANCE ty)
	  (* end case *))
      | toDebugString (AST.VarTy(AST.TVar{name, ...})) = Atom.toString name
      | toDebugString (AST.ConTy([], tyc)) = Atom.toString(TyCon.nameOf tyc)
      | toDebugString (AST.ConTy([ty], tyc)) = concat[
	    toDebugString ty, " ", Atom.toString(TyCon.nameOf tyc)
	  ]
      | toDebugString (AST.FunTy(ty1, ty2)) = concat[toDebugString ty1, " -> ", toDebugString ty2]
      | toDebugString (AST.TupleTy tys) = "<tuplety>"

  (* return the "head-normal form" by pruning an instantiated meta
   * variables.
   *)
    fun prune (AST.MetaTy(AST.MVar{info as ref(AST.INSTANCE ty), ...})) = let
	  val _ = (info := AST.UNIV ~1) (* black hole *)
	  val ty = prune ty
	  in
	    info := AST.INSTANCE ty;	(* path compression *)
	    ty
	  end
      | prune ty = ty

  (* apply a type variable to type substitution to a type *)
    fun applySubst (subst, ty) = let
	  fun inst ty = (case prune ty
		 of AST.MetaTy _ => raise Fail "unexpected meta variable"
		  | AST.VarTy tv => (case TVMap.find(subst, tv)
		       of NONE => AST.VarTy tv
			| SOME ty => ty
		      (* end case *))
		  | AST.ConTy(args, tyc) => AST.ConTy(List.map inst args, tyc)
		  | AST.FunTy(ty1, ty2) => AST.FunTy(inst ty1, inst ty2)
		  | AST.TupleTy tys => AST.TupleTy(List.map inst tys)
		(* end case *))
	  in
	    inst ty
	  end

  (* create a type-variable to type substitution from a list of type params and
   * a list of type variables.
   *)
    fun substitution (tvs, tys) = let
	  fun ins (tv, ty, env) = TVMap.insert(env, tv, ty)
	  in
	    ListPair.foldlEq ins TVMap.empty (tvs, tys)
	  end

  (* apply a type-variable-to-type substitution to a type.  The substitution
   * is represented as a list of type variable/type pairs.
   *)
    fun substitute (ty, []) = ty
      | substitute (ty, s) = applySubst (List.foldl TVMap.insert' TVMap.empty s, ty)

  (* substitute types for the bound variables of a type scheme *)
    fun applyScheme (AST.TyScheme(tvs, ty), tys) = applySubst (substitution (tvs, tys), ty)

  (* instantiate a type scheme at the given lambda-nesting depth *)
    fun instantiate (_, AST.TyScheme([], ty)) = ([], ty)
      | instantiate (depth, AST.TyScheme(tvs, ty)) = let
	(* create a substitution from type variables to fresh meta variables *)
	  val (mvs, subst) = List.foldl
		(fn (tv, (mvs, s)) => let
		  val mv = AST.MetaTy (MV.new depth)
		  in
		    (mv::mvs, TVMap.insert(s, tv, mv))
		  end)
		  ([], TVMap.empty) tvs
	  in
	    (mvs, applySubst (subst, ty))
	  end

  (* close a type w.r.t. to a set of non-generic variables (i.e., those
   * variables whose depth is less than or equal to the given depth).
   *)
    fun closeTy (depth, ty) = let
	  val count = ref 0
	(* generate a fresh type variable *)
	  fun newVar () = let
		val id = !count
		in
		  count := id+1;
		  TyVar.new(Atom.atom("'a" ^ Int.toString id))
		end
	  fun genVars (ty, env) = (case prune ty
		 of ty as AST.MetaTy(mv as AST.MVar{info=ref(AST.UNIV d), ...}) =>
		      if (d > depth)
			then (case MVMap.find(env, mv) (* generic variable *)
			   of SOME tv => (env, AST.VarTy tv)
			    | NONE => let
				val tv = newVar()
				in
				  (MVMap.insert(env, mv, tv), AST.VarTy tv)
				end
			  (* end case *))
			else (env, ty) (* non-generic variable *)
		  | AST.MetaTy _ => raise Fail "impossible"
		  | AST.VarTy _ => raise Fail "unexpected type variable"
		  | AST.ConTy(args, tyc) => let
		      val (env, tys) = genVarsForTys (args, env)
		      in
			(env, AST.ConTy(tys, tyc))
		      end
		  | AST.FunTy(ty1, ty2) => let
		      val (env, ty1) = genVars (ty1, env)
		      val (env, ty2) = genVars (ty2, env)
		      in
			(env, AST.FunTy(ty1, ty2))
		      end
		  | AST.TupleTy tys => let
		      val (env, tys) = genVarsForTys (tys, env)
		      in
			(env, AST.TupleTy tys)
		      end
		(* end case *))
	  and genVarsForTys (tys, env) = let
		fun f (ty, (env, tys)) = let
			val (env', ty') = genVars(ty, env)
			in
			  (env', ty'::tys)
			end
		in
		  List.foldr f (env, []) tys
		end
	  val (tvs, ty) = genVars (ty, MetaVar.Map.empty)
	  in
	  (* instantiate the meta-variables to their corresponding type variables
	   * so that the type information in the AST is complete.
	   *)
	    MVMap.appi
	      (fn (AST.MVar{info, ...}, tv) => info := AST.INSTANCE(AST.VarTy tv))
		tvs;
	    AST.TyScheme(MVMap.listItems tvs, ty)
	  end

  (* return true if two types are equal.  Note that this function does not
   * do unification or alpha renaming of meta variables, but it does chase
   * instantiated meta-variable types.
   *)
    fun same (ty1, ty2) = (case (prune ty1, prune ty2)
	   of (AST.MetaTy mv1, AST.MetaTy mv2) => MV.same(mv1, mv2)
	    | (AST.VarTy tv1, AST.VarTy tv2) => TyVar.same(tv1, tv2)
	    | (AST.ConTy(args1, tyc1), AST.ConTy(args2, tyc2)) =>
		TyCon.same(tyc1, tyc2) andalso ListPair.allEq same (args1, args2)
	    | (AST.FunTy(ty11, ty12), AST.FunTy(ty21, ty22)) =>
		same(ty11, ty21) andalso same(ty21, ty22)
	    | (AST.TupleTy tys1, AST.TupleTy tys2) =>
		ListPair.allEq same (tys1, tys2)
	    | _ => false
	  (* end case *))

  end

