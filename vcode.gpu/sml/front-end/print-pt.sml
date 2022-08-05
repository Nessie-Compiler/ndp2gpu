(* print-pt.sml 
 *
 * (c) 2/1/2007 Adam Shaw
 *
 * Functions to convert parse trees to strings.
 *)

(* Alpha version: just barely tested. *)

structure PrintPT : sig

  val pp    : ParseTree.program -> string
  val print : ParseTree.program -> unit
  val out   : ParseTree.program * string -> unit

end = struct

    (* BEGIN utils *)
    fun spaces n = 
	let fun sp (0, acc) = implode acc
	      | sp (n, acc) = sp (n-1, #" " :: acc)
	in
	    if n < 0 then raise Fail "negative spaces?"
	    else sp (n, [])
	end
    fun indent (i, s) = concat [spaces i, s]
    fun par s = concat ["(", s, ")"]
    fun quote s = concat ["\"", s, "\""]
    fun comment s = concat ["(* ", s, " *)"]    
    val catw = String.concatWith
    (* END utils *)

    structure T = ParseTree

    type tyid = Atom.atom
    type tyvar = Atom.atom
    type conid = Atom.atom
    type vid = Atom.atom
    type opid = Atom.atom	(* operator IDs; e.g., "=", "<=", "<", "::", ... *)

    val tyid  = Atom.toString
    val tyvar = Atom.toString
    val conid = Atom.toString
    val vid   = Atom.toString
    val opid  = Atom.toString

   (* a term marked with a line number *)
    type 'a mark = {lnum : int, tree : 'a}

    fun mark (lnum, s) = concat ["(", s, " ", comment (Int.toString lnum), ")"]

   (* top-level declarations *)
    fun decl (i : int) (d : T.decl) : string =
	indent (i, 
		(case d
		  of T.MarkDecl {lnum, tree} => mark (lnum, decl i tree)
		   | T.TyDecl (tvs, ti, t) =>
		     let val tvs' = catw " " (map tyvar tvs)
			 val ti' = tyid ti
			 val t' = ty i t
		     in
			 catw " " ["type",
				   case tvs' of "" => ti'
					      | _ => concat [tvs', " ", ti'],
				   "=", t', ";"]
		     end
		   | T.DataDecl (tvs, ti, cds) =>
		     let val tvs' = catw " " (map tyvar tvs)
			 val ti' = tyid ti
			 val cds' = con_decls (i+2) cds
		     in
			 concat ["datatype ", tvs', " ", ti', 
				 "\n", cds',";"]
		     end
		   | T.ValueDecl v => concat [val_decl i v, ";"]
		(* end case *)))

    and con_decl (i : int) (d : T.con_decl) : string =
	indent (i,
		(case d
		  of T.MarkConDecl {lnum, tree} => mark (lnum, con_decl i tree)
		   | T.ConDecl (cid, optTy) => 
		      (case optTy
			of NONE => conid cid
			 | SOME t => catw " " [conid cid, "of", ty i t]
		      (* end case *))
		(* end case *)))

    and con_decls (i : int) (ds : T.con_decl list) : string =		  
	let fun f sym d = indent (i, concat [sym, " ", con_decl i d])
	in
	    case ds
	     of [] => raise Fail "expected at least one declaration"
	      | cd::cds => catw "\n" ((f "=" cd) :: (map (f "|") cds))
	end		     

    and val_decl (i : int) (d : T.val_decl) : string =
	indent (i,
		(case d
		  of T.MarkVDecl {lnum, tree} => mark (lnum, val_decl i tree)
		   | T.ValVDecl (aps, e) => 
		     catw " " ["val", tuple_pat aps, "=", exp 0 e]		  
		   | T.FunVDecl (fs) =>
		      (case fs
			of [] => raise Fail "empty function list"
			 | func::funcs => catw "\n" ((funct i true func)
						     :: (map (funct i false) funcs))
		      (* end case *))
		(* end case *)))

    and funct (i : int) (isFirst : bool) (f : T.funct) : string =
	indent (i,
		(case f
		  of T.MarkFunct {lnum, tree} => mark (lnum, funct i isFirst tree)
		   | T.Funct (v, aps, e) => 
		     concat [if isFirst then "fun " else "and ", 
			     vid v, " ", tuple_pat aps, " =\n", exp (i+2) e]
		(* end case *)))

    and ty (i : int) (t : T.ty) : string =
      indent (i, 
	      (case t
		of T.MarkTy {lnum, tree} => mark (lnum, ty 0 tree)
		 | T.NamedTy (ts, ti) => catw " " ((map (ty 0) ts) @ [tyid ti])
		 | T.VarTy tv => tyvar tv
		 | T.TupleTy ts => par (catw "," (map (ty 0) ts))
		 | T.FunTy (d, r) => concat [ty 0 d, " -> ", ty 0 r]
	      (* end case *)))

    and exp (i : int) (e : T.exp) : string =
	indent (i,
		(case e
		  of T.MarkExp {lnum, tree} => mark (lnum, exp i tree)
		   | T.LetExp (ds, e) => 
		     let val ds' = catw "\n" (map (val_decl (i+2)) ds)
		     in
			 concat ["let ", ds', "\n", indent (i, "in\n"),
				 exp (i+2) e, "\n", indent (i, "end\n")]
		     end
		   | T.IfExp (c, t, f) =>
		     concat ["if ", exp 0 c, " then ", exp 0 t, "\n",
			      indent (i, "else "), exp 0 f]
		   | T.CaseExp (e, ms) =>
		     let val ms' = (case ms 
				     of [] => raise Fail "branchless case"
				      | ma::mas => 
					catw "\n" ((match (i+2) true ma) ::
						   map (match (i+2) false) mas))
		     in
			 concat ["case ", exp 0 e, "\n", ms']
		     end
		   | T.AndAlsoExp (e1, e2) => 
		     par (concat [exp 0 e1, " andalso ", exp 0 e2])
		   | T.OrElseExp (e1, e2) => 
		     par (concat [exp 0 e1, " orelse ", exp 0 e2])
		   | T.BinaryExp (e1, oi, e2) => 
		     par (concat [exp 0 e1, " ", opid oi, " ", exp 0 e2])
		   | T.ApplyExp (e1, e2) => 
		     par (concat [exp 0 e1, " ", exp 0 e2])
		   | T.ConstExp c => const c
		   | T.TupleExp es => par (catw "," (map (exp 0) es))
		   | T.SeqExp es => par (catw ";" (map (exp 0) es))
		   | T.IdExp v => vid v
		(* end case *)))

    (* match : int -> bool -> match -> string *)
    (* The boolean value "isFirst" indicates whether the branch in question is the *)
    (* first in its group; if so, it starts with "of", otherwise with a bar. *)
    and match (i : int) (isFirst : bool) (m : T.match) : string =
	let val pre = if isFirst then "of " else " | "
	in
	    indent (i,
		    (case m
		      of T.MarkMatch {lnum, tree} => mark (lnum, match i isFirst tree)
		       | T.Match (p, e) => concat [pre, pat 0 p, " => ", exp 0 e]
		   (* end case *)))
	end

    and pat (i : int) (p : T.pat) : string =
	indent (i, 
		(case p
		  of T.MarkPat {lnum, tree} => mark (lnum, pat i tree)
		   | T.ConPat (cid, aps) => concat [conid cid, " ", tuple_pat aps]
		   | T.TuplePat aps => tuple_pat aps
		   | T.AtomicPat ap => atomic_pat ap
		(* end case *)))

    and atomic_pat (ap : T.atomic_pat) : string =
	(case ap
	  of T.MarkAPat {lnum, tree} => mark (lnum, atomic_pat tree)
	   | T.WildPat => "_"
	   | T.IdPat v => vid v
	   | T.ConstPat c => const c
	(* end case *))

    and tuple_pat (aps : T.atomic_pat list) : string = 
	par (catw "," (map atomic_pat aps))

    and const (T.IntLit ii) = IntInf.toString ii
      | const (T.StrLit s) = quote (String.toString s)

    fun program (ds : T.decl list, e : T.exp) : string =
	let val ds' = catw "\n\n" (map (decl 0) ds)
	in
	    concat [ds', "\n\n", exp 0 e]
	end

    val pp : T.program -> string = program

    val print : T.program -> unit = 
	(fn p => (TextIO.print (program p);
		  TextIO.print "\n"))

    fun out (p : T.program, s : string) : unit =
	let val ostr = TextIO.openOut s
	in
	    TextIO.output (ostr, program p);
	    TextIO.output (ostr, "\n");
	    TextIO.closeOut ostr
	end
	handle _ => TextIO.print ("cannot open " ^ s ^ "\n")

end
