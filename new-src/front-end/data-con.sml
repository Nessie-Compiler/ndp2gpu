(* data-con.sml
 *
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * CMSC 22610 Sample code (Winter 2007)
 *)

structure DataCon : sig

  (* create a new data constructor and add it to the list of constructors in its parent. *)
    val new : AST.tycon -> (Atom.atom * AST.ty option) -> AST.dcon

  (* return true if two data constructors are the same (i.e., have the same stamp) *)
    val same : AST.dcon * AST.dcon -> bool

  (* return the name of the data constructor *)
    val nameOf : AST.dcon -> Atom.atom

  (* return the type of the data constructor *)
    val typeOf : AST.dcon -> AST.ty_scheme

  (* return the argument type of the data constructor (if any) *)
    val argTypeOf : AST.dcon -> AST.ty option

  (* return true if the data constructor is nullary *)
    val isNullary : AST.dcon -> bool

  (* hash tables keyed by data constructors *)
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = AST.dcon

   end = struct

    datatype dcon = datatype AST.dcon

    fun new (tyc as AST.DataTyc{cons, ...}) (name, argTy) = let
	  val dcon = DCon{stamp = Stamp.new(), name = name, owner = tyc, argTy = argTy}
	  in
	    cons := !cons @ [dcon];
	    dcon
	  end

    fun same (DCon{stamp=a, ...}, DCon{stamp=b, ...}) = Stamp.same(a, b)

    fun nameOf (DCon{name, ...}) = name

    fun argTypeOf (DCon{argTy, ...}) = argTy

    fun typeOf (DCon{owner as AST.DataTyc{params, ...}, argTy, ...}) = let
	  val ty = AST.ConTy(List.map AST.VarTy params, owner)
	  in
	    case argTy
	     of NONE => AST.TyScheme(params, ty)
	      | SOME ty' => AST.TyScheme(params, AST.FunTy(ty', ty))
	    (* end case *)
	  end

    fun isNullary (DCon{argTy = NONE, ...}) = true
      | isNullary _ = false

    structure Tbl = HashTableFn (
      struct
	type hash_key = dcon
	fun hashVal (DCon{stamp, ...}) = Stamp.hash stamp
	val sameKey = same
      end)

  end
