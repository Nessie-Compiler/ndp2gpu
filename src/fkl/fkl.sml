(* fkl.sml
 *
 * The representation of programs after flattening.
 *)

structure FKL =
  struct

    type var = Var.var

    datatype program = Prog of fundef list * exp

    and fundef = Fun of var * var list * exp

    and exp
      = Const of Literal.literal
      | Var of var
      | Tuple of exp list
      | App of var * exp list
      | Let of var * exp * exp
      | If of exp * exp * exp
      | PArray of exp list

    local
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
      fun ppExp (strm, e) = (case e
	     of (Const lit) => str(strm, Literal.toString lit)
	      | (Var x) => str(strm, Var.toString x)
	      | (Tuple es) => (
		  PP.openHBox strm;
		    str(strm, "(");
		    ppList (strm, ppExp) es;
		    str(strm, ")");
		  PP.closeBox strm)
	      | App(f, es) => (
		  PP.openHBox strm;
		    str (strm, Var.toString f);
		    str (strm, "(");
		    PP.openHVBox strm (PP.Rel 2);
		      ppList (strm, ppExp) es;
		      str (strm, ")");
		      PP.cut strm;
		    PP.closeBox strm;
		  PP.closeBox strm)
	      | Let _ => let
		  fun ppBlock (Let(x, e1, e2)) = (
			PP.openHOVBox strm (PP.Rel 4);
			  PP.openHBox strm;
			    str (strm, "let"); sp strm; str(strm, Var.toString x);
			    sp strm; str (strm, "=");
			  PP.closeBox strm;
			  sp strm;
			  PP.openHBox strm;
			    ppExp (strm, e1);
			  PP.closeBox strm;
			PP.closeBox strm;
			nl strm;
			ppBlock e2)
		    | ppBlock e = (
			PP.openVBox strm (PP.Rel 2);
			  str (strm, "in"); nl strm;
			  ppExp (strm, e);
			PP.closeBox strm;
			nl strm;
			str (strm, "end"))
		  in
		    PP.openVBox strm (PP.Rel 0);
		      ppBlock e;
		    PP.closeBox strm;
		    PP.cut strm
		  end
	      | If(e1, e2, e3) => (
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
	      | PArray es => (
		  PP.openHBox strm;
		    str(strm, "[");
		    ppList (strm, ppExp) es;
		    str(strm, "]");
		  PP.closeBox strm)
	    (* end case *))
      fun ppFun (strm, Fun(f, params, e)) = (
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
      fun ppProg (strm, Prog([], e)) = (
	    PP.openHBox strm;
	      ppExp (strm, e);
	      nl strm;
	    PP.closeBox strm)
	| ppProg (strm, Prog(fds, e)) = (
	    PP.openVBox strm (PP.Rel 0);
	      List.app (fn fd => (ppFun(strm, fd); nl strm)) fds;
	      PP.openVBox strm (PP.Rel 2);
		str(strm, "in"); PP.newline strm;
		PP.openHBox strm;
		  ppExp (strm, e);
		PP.closeBox strm;
	      PP.closeBox strm;
	      nl strm;
	      str(strm, "end");
	      nl strm;
	    PP.closeBox strm)
    in
    fun output (outS, prog) = let
	  val dev = SimpleTextIODev.openDev {dst=outS, wid=90}
	  val strm = PP.openStream dev
	  in
	    ppProg (strm, prog);
	    PP.flushStream strm;
	    PP.closeStream strm
	  end
    fun print prog = output (TextIO.stdOut, prog)
    end (* local *)

  end
