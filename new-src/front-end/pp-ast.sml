(* pp-ast.sml
 *
 * COPYRIGHT (c) 2008 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)


structure PPAST : sig

    val output : TextIO.outstream * AST.program -> unit
    val print : AST.program -> unit

  end = struct

    open AST

    structure PP = TextIOPP
    fun str (strm, s) = PP.string strm s
    fun sp strm = PP.space strm 1
    fun nl strm = PP.newline strm

    fun ppList (strm, pp) = let
	  fun pp' [] = ()
	    | pp' [x] = pp(strm, x)
	    | pp' (x::r) = (pp(strm, x); str(strm, ","); sp strm; pp' r)
	  in
	    pp'
	  end

    fun ppAtom (strm, a) = str(strm, Atom.toString a)

    fun ppBlock (strm, e) = let
	  fun pp e = (
		PP.openHOVBox strm (PP.Rel 0);
		  str (strm, "let"); sp strm;
		  ppBlk e;
		PP.closeBox strm)
	  and ppBlk (AST.LetExp(pat, e1, e2)) = (
		PP.openHOVBox strm (PP.Rel 4);
		  PP.openHBox strm;
		    str (strm, "val"); sp strm; ppPat(strm, pat);
		    sp strm; str (strm, "=");
		  PP.closeBox strm;
		  sp strm;
		  PP.openHBox strm;
		    ppExp (strm, e1);
		  PP.closeBox strm;
		  nl strm;
		PP.closeBox strm;
		ppBlk e2)
	    | ppBlk (AST.FunExp(fbs, e)) = let
		fun pp (AST.FB(f, x, e), prefix) = (
		      PP.openHOVBox strm (PP.Rel 4);
			PP.openHBox strm;
			  str (strm, prefix); sp strm;
			  ppAtom (strm, Var.nameOf f); sp strm;
			  ppAtom (strm, Var.nameOf x); sp strm;
			  str (strm, "=");
			PP.closeBox strm;
			sp strm;
			PP.openHBox strm;
			  ppExp (strm, e);
			PP.closeBox strm;
			nl strm;
		      PP.closeBox strm;
		      "and")
		in
		  List.foldl pp "fun" fbs; nl strm;
		  ppBlk e
		end
	    | ppBlk e = (
		PP.openVBox strm (PP.Rel 2);
		  str (strm, "in"); nl strm;
		  ppExp (strm, e);
		PP.closeBox strm;
		nl strm;
		str (strm, "end"))
	  in
	    case e
	     of AST.LetExp _ => pp e
	      | AST.FunExp _ => pp e
	      | _ => ppExp (strm, e)
	    (* end case *)
	  end

      and ppExp (strm, e) = (case e
	     of AST.LetExp _ => (
		  PP.openVBox strm (PP.Rel 0);
		    ppBlock (strm, e);
		  PP.closeBox strm;
		  PP.cut strm)
	      | AST.FunExp _ => (
		  PP.openVBox strm (PP.Rel 0);
		    ppBlock (strm, e);
		  PP.closeBox strm;
		  PP.cut strm)
	      | AST.IfExp(e1, e2, e3) => (
		  PP.openHOVBox strm (PP.Rel 2);
		    PP.openHBox strm;
		      str(strm, "if"); sp strm; ppExp(strm, e1);
		    PP.closeBox strm;
		    sp strm;
		    PP.openHBox strm;
		      str(strm, "then"); sp strm; ppExp(strm, e2);
		    PP.closeBox strm;
		    sp strm;
		    PP.openHBox strm;
		      str(strm, "else"); sp strm; ppExp(strm, e3);
		    PP.closeBox strm;
		  PP.closeBox strm)
	      | AST.CaseExp(e, rules) => raise Fail "case"
	      | AST.ApplyExp(f, arg) => (
		  PP.openHBox strm;
		    ppExp (strm, f);
		    sp strm;
		    ppExp (strm, arg);
		  PP.closeBox strm)
	      | (AST.TupleExp es) => (
		  PP.openHBox strm;
		    str(strm, "(");
		    ppList (strm, ppExp) es;
		    str(strm, ")");
		  PP.closeBox strm)
	      | AST.PArrayExp es => (
		  PP.openHBox strm;
		    str(strm, "[");
		    ppList (strm, ppExp) es;
		    str(strm, "]");
		  PP.closeBox strm)
	      | AST.PForallExp(e1, p, e2, optPred) => raise Fail "forall"
	      | (AST.DConExp(dc, tys)) => (
		  PP.openHBox strm;
		    ppAtom (strm, DataCon.nameOf dc);
		    ppTyArgs (strm, tys);
		  PP.closeBox strm)
	      | (AST.VarExp(x, tys)) => (
		  PP.openHBox strm;
		    str(strm, Var.toString x);
		    ppTyArgs (strm, tys);
		  PP.closeBox strm)
	      | (AST.ConstExp lit) => str(strm, Literal.toString lit)
	      | AST.SeqExp _ => raise Fail "unexpected SeqExp"
	    (* end case *))
      and ppFun (strm, AST.FB(f, x, e)) = (
	    PP.openHOVBox strm (PP.Rel 4);
	      PP.openHBox strm;
	        str(strm, "fun"); sp strm; str(strm, Var.toString f); sp strm;
	        str(strm, "("); ppList (strm, fn (strm, x) => str(strm, Var.toString x)); str(strm, ")");
		sp strm; str(strm, "=");
	      PP.closeBox strm;
	      sp strm;
	      PP.openHBox strm;
		ppExp (strm, e);
	      PP.closeBox strm;
	    PP.closeBox strm)
      and ppPat (strm, pat) = (case pat
	     of AST.ConPat(dc, tys, pat) => (
		  PP.openHBox strm;
		    ppAtom (strm, DataCon.nameOf dc);
		    ppTyArgs (strm, tys);
		    case pat
		     of NONE => ()
		      | SOME(p as AST.ConPat _) => (
			  str(strm, "(");
			  ppPat (strm, p);
			  str(strm, ")"))
		      | SOME p => ppPat (strm, p)
		    (* end case *);
		  PP.closeBox strm)
	      | AST.TuplePat pats => (
		  PP.openHOVBox strm (PP.Rel 2);
	            str(strm, "(");
		    ppList (strm, ppPat);
		    str(strm, ")");
		  PP.closeBox strm)
	      | AST.VarPat x => ppAtom (strm, Var.nameOf x)
	      | AST.ConstPat lit => str (strm, Literal.toString lit)
	    (* end case *))
 
      and ppTyArgs (strm, []) = ()
	| ppTyArgs (strm, tys) = (
	    PP.openHBox strm;
	      str(strm, "[");
	      ppList (strm, fn (strm, ty) => str(strm, TypeUtil.toString ty)) tys;
	      str(strm, "]");
	    PP.closeBox strm)

      fun ppProg (strm, e) = (
	    PP.openHBox strm;
	      ppExp (strm, e);
	      nl strm;
	    PP.closeBox strm)

    fun output (outS, prog) = let
	  val dev = SimpleTextIODev.openDev {dst=outS, wid=90}
	  val strm = PP.openStream dev
	  in
	    ppProg (strm, prog);
	    PP.flushStream strm;
	    PP.closeStream strm
	  end

    fun print prog = output (TextIO.stdOut, prog)

  end

