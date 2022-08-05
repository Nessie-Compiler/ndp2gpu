(* basis.sml
 *
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * CMSC 22610 Sample code (Winter 2007)
 *)

structure Basis =
  struct

    nonfix div mod

    local
      structure N = BasisNames
      val --> = AST.FunTy
      fun ** (t1, t2) = AST.TupleTy[t1, t2]
      infix 9 **
      infixr 8 -->
      fun forall mkTy = let
	    val tv = TyVar.new(Atom.atom "'a")
	    in
	      AST.TyScheme([tv], mkTy tv)
	    end
      fun monoVar (name, ty) = Var.new(name, ty)
    in

    val boolTyc = TyCon.newDataTyc (N.bool, [])
    val boolTrue = DataCon.new boolTyc (N.boolTrue, NONE)
    val boolFalse = DataCon.new boolTyc (N.boolFalse, NONE)

    local
      val tv = TyVar.new(Atom.atom "'a")
      val tv' = AST.VarTy tv
    in
    val listTyc = TyCon.newDataTyc (N.list, [tv])
    val listNil = DataCon.new listTyc (N.listNil, NONE)
    val listCons =
	  DataCon.new listTyc
	    (N.listCons, SOME(AST.TupleTy[tv', AST.ConTy([tv'], listTyc)]))
    end (* local *)

    val intTyc = TyCon.newAbsTyc (N.int, 0)
    val stringTyc = TyCon.newAbsTyc (N.string, 0)
    val unitTyc = TyCon.newAbsTyc (N.unit, 0)
    val parrayTyc = TyCon.newAbsTyc (N.parray, 1)

  (* predefined types *)
    val boolTy = AST.ConTy([], boolTyc)
    val intTy = AST.ConTy([], intTyc)
    val stringTy = AST.ConTy([], stringTyc)
    val unitTy = AST.ConTy([], unitTyc)
    fun parrayTy ty = AST.ConTy([ty], parrayTyc)

  (* operator symbols *) 
    val lt =		monoVar(N.lt, intTy ** intTy --> boolTy)
    val lte =		monoVar(N.lte, intTy ** intTy --> boolTy)
    val gte =		monoVar(N.gte, intTy ** intTy --> boolTy)
    val gt =		monoVar(N.gt, intTy ** intTy --> boolTy)
    val append =	Var.newPoly(N.append,
			  forall(fn tv => let
			    val ty = AST.ConTy([AST.VarTy tv], listTyc)
			    in
			      ty ** ty --> ty
			    end))
    val plus =		monoVar(N.plus, intTy ** intTy --> intTy)
    val minus =		monoVar(N.minus, intTy ** intTy --> intTy)
    val times =		monoVar(N.times, intTy ** intTy --> intTy)
    val div =		monoVar(N.div, intTy ** intTy --> intTy)
    val mod =		monoVar(N.mod, intTy ** intTy --> intTy)
    val uMinus =	monoVar(N.uMinus, intTy --> intTy)
    val subscript =	Var.newPoly(N.subscript,
			  forall(fn tv => let
			    val ty = AST.ConTy([AST.VarTy tv], parrayTyc)
			    in
			      ty ** intTy --> AST.VarTy tv
			    end))
    val arrayLength =	Var.newPoly(N.length,
			  forall(fn tv => let
			    val ty = AST.ConTy([AST.VarTy tv], parrayTyc)
			    in
			      ty --> intTy
			    end))
    val arrayConcat =	Var.newPoly(N.arrayConcat,
			  forall(fn tv => let
			    val ty = AST.ConTy([AST.VarTy tv], parrayTyc)
			    in
			      ty ** ty --> ty
			    end))

    val lookupOp = let
	  val tbl = AtomTable.mkTable (16, Fail "lookupOp")
	  val ins = AtomTable.insert tbl
	  in
	    List.app ins [
		(N.lt, lt),
		(N.lte, lte),
		(N.gte, gte),
		(N.gt, gt),
		(N.append, append),
		(N.plus, plus),
		(N.minus, minus),
		(N.times, times),
		(N.div, div),
		(N.mod, mod),
		(N.subscript,	subscript),
		(N.arrayConcat,	arrayConcat)
	      ];
	    AtomTable.lookup tbl
	  end

  (* equality operators *)
    val boolEq =	monoVar(N.eq, boolTy ** boolTy --> boolTy)
    val intEq =		monoVar(N.eq, intTy ** intTy --> boolTy)
    val stringEq =	monoVar(N.eq, stringTy ** stringTy --> boolTy)

  (* predefined functions *)
    val print =		monoVar(N.print, stringTy --> unitTy)
    val fail =		Var.newPoly(N.fail, forall (fn tv => unitTy --> AST.VarTy tv))

  (* the predefined type environment *)
    val te0 = Env.fromList [
	    (N.bool,		Env.TyCon boolTyc),
	    (N.int,		Env.TyCon intTyc),
	    (N.list,		Env.TyCon listTyc),
	    (N.string,		Env.TyCon stringTyc),
	    (N.unit,		Env.TyCon unitTyc),
	    (N.parray,		Env.TyCon parrayTyc)
	  ]

    val ve0 = Env.fromList [
	    (N.boolTrue,	Env.Con boolTrue),
	    (N.boolFalse,	Env.Con boolFalse),
	    (N.listNil,		Env.Con listNil),
	    (N.listCons,	Env.Con listCons),
	    (N.uMinus,		Env.Var uMinus),
	    (N.length,		Env.Var arrayLength),
	    (N.print,		Env.Var print),
	    (N.fail,		Env.Var fail)
	  ]

    end (* local *)

  end

