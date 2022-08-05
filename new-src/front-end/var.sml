(* var.sml
 *
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * CMSC 22610 Sample code (Winter 2007)
 *)

structure Var : sig

    val new : Atom.atom * AST.ty -> AST.var
    val newPoly : Atom.atom * AST.ty_scheme -> AST.var

    val nameOf : AST.var -> Atom.atom
    val toString : AST.var -> string

    val typeOf : AST.var -> AST.ty_scheme

  (* return the type of a variable that is known to be monomorphic *)
    val monoTypeOf : AST.var -> AST.ty

  (* close the type of the variable w.r.t. to the given lambda-nesting depth. *)
    val closeTypeOf : int * AST.var -> unit

    val same : AST.var * AST.var -> bool
    val hash : AST.var -> word

    structure Map : ORD_MAP where type Key.ord_key = AST.var
    structure Set : ORD_SET where type Key.ord_key = AST.var
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = AST.var

  end = struct

    fun newPoly (name, ty) = AST.Var{
	    stamp = Stamp.new(),
	    name = name,
	    ty = ref ty
	  }

    fun new (name, ty) = newPoly (name, AST.TyScheme([], ty))

    fun nameOf (AST.Var{name, ...}) = name

    fun toString (AST.Var{name, stamp, ...}) = Atom.toString name

    fun typeOf (AST.Var{ty, ...}) = !ty

  (* return the type of a variable that is known to be monomorphic *)
    fun monoTypeOf x = (case typeOf x
	   of AST.TyScheme([], ty) => ty
	    | _ => raise Fail(concat["monoTypeOf(", Atom.toString(nameOf x), ")"])
	  (* end case *))

    fun closeTypeOf (depth, AST.Var{ty as ref(AST.TyScheme(_, ty')), ...}) =
	  ty := TypeUtil.closeTy(depth, ty')

    fun compare (AST.Var{stamp=s1, ...}, AST.Var{stamp=s2, ...}) = Stamp.compare(s1, s2)
    fun same (AST.Var{stamp=s1, ...}, AST.Var{stamp=s2, ...}) = Stamp.same(s1, s2)
    fun hash (AST.Var{stamp, ...}) = Stamp.hash stamp

    local
      structure Key = struct
	  type ord_key = AST.var
	  val compare = compare
	end
    in
    structure Map = RedBlackMapFn (Key)
    structure Set = RedBlackSetFn (Key)
    end

    structure Tbl = HashTableFn (
      struct
	type hash_key = AST.var
	val hashVal = hash
	val sameKey = same
      end)

  end
