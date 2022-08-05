(* ast.sml
 *
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * CMSC 22610 Sample code (Winter 2007)
 *)

structure AST =
  struct

    datatype ty_scheme = TyScheme of (tyvar list * ty)

    and ty
      = MetaTy of meta
      | VarTy of tyvar
      | ConTy of (ty list * tycon)
      | FunTy of ty * ty
      | TupleTy of ty list

    and meta = MVar of {
	    stamp : Stamp.stamp,	(* unique stamp *)
	    info : meta_info ref
	  }

    and meta_info
      = UNIV of int
      | INSTANCE of ty

    and tyvar = TVar of {
	    stamp : Stamp.stamp,	(* unique stamp *)
	    name : Atom.atom		(* the varable name *)
	  }

    and tycon
      = AbsTyc of {
	    stamp : Stamp.stamp,	(* unique stamp *)
	    name : Atom.atom,		(* the type name *)
	    arity : int			(* number of type parameters *)
	  }
      | DataTyc of {
	    stamp : Stamp.stamp,	(* unique stamp *)
	    name : Atom.atom,		(* the type name *)
	    params : tyvar list,	(* type parameters *)
	    cons : dcon list ref	(* list of data constructors *)
	  }

    and dcon = DCon of {
	    stamp : Stamp.stamp,	(* unique stamp *)
	    name : Atom.atom,		(* the name of the constructor *)
	    owner : tycon,		(* the datatype for which this is a constructor *)
	    argTy : ty option		(* argument type; NONE for nullary constructors *)
	  }

    datatype var = Var of {
	    stamp : Stamp.stamp,	(* unique stamp *)
	    name : Atom.atom,		(* the name of the variable *)
	    ty : ty_scheme ref		(* type of variable *)
	  }

    datatype exp
      = LetExp of (pat * exp * exp)
      | FunExp of (lambda list * exp)
      | IfExp of (exp * exp * exp)
      | CaseExp of (exp * (pat * exp) list)
      | ApplyExp of exp * exp
      | TupleExp of exp list
      | PArrayExp of exp list
      | PForallExp of (exp * pat * exp * exp option)
      | DConExp of dcon * ty list
      | VarExp of var * ty list
      | ConstExp of Literal.literal
      | SeqExp of (exp * exp)

    and lambda = FB of (var * var * exp)

    and pat
      = ConPat of dcon * ty list * pat option
      | TuplePat of pat list
      | VarPat of var
      | ConstPat of Literal.literal

    type program = exp

  end
