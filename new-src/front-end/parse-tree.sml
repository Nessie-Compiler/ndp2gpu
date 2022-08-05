(* parse-tree.sml
 *
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Parse-tree representation of MinML programs.
 *)

structure ParseTree =
  struct

    type tyid = Atom.atom
    type tyvar = Atom.atom
    type conid = Atom.atom
    type vid = Atom.atom
    type opid = Atom.atom	(* operator IDs; e.g., "=", "<=", "<", "::", ... *)

  (* a term marked with a line number *)
    type 'a mark = {lnum : int, tree : 'a}

  (* top-level declarations *)
    datatype decl
      = MarkDecl of decl mark
      | TyDecl of (tyvar list * tyid * ty)
      | DataDecl of (tyvar list * tyid * con_decl list)
      | ValueDecl of val_decl

  (* data-constructor definitions *)
    and con_decl
      = MarkConDecl of con_decl mark
      | ConDecl of conid * ty option

  (* value declarations *)
    and val_decl
      = MarkVDecl of val_decl mark
      | ValVDecl of atomic_pat list * exp
      | FunVDecl of funct list

  (* function definitions *)
    and funct
      = MarkFunct of funct mark
      | Funct of (vid * atomic_pat list * exp)

  (* types *)
    and ty
      = MarkTy of ty mark
      | NamedTy of (ty list * tyid)
      | VarTy of tyvar
      | TupleTy of ty list
      | FunTy of (ty * ty)

  (* expressions *)
    and exp
      = MarkExp of exp mark
      | LetExp of (val_decl list * exp)
      | IfExp of (exp * exp * exp)
      | CaseExp of (exp * match list)
      | AndAlsoExp of (exp * exp)
      | OrElseExp of (exp * exp)
      | BinaryExp of (exp * opid * exp)	(* infix binary expressions *)
      | ApplyExp of (exp * exp)		(* application *)
      | ConstExp of Literal.literal
      | TupleExp of exp list
      | PArrayExp of exp list		(* parallel array *)
      | PForallExp of (exp * pat * exp * exp option)
					(* parallel-array comprehension *)
      | SeqExp of exp list		(* sequence of two or more expressions *)
      | IdExp of vid			(* either variable or nullary constant *)

  (* pattern matching rules *)
    and match
      = MarkMatch of match mark
      | Match of (pat * exp)

    and pat
      = MarkPat of pat mark
      | ConPat of conid * atomic_pat list
      | TuplePat of atomic_pat list
      | AtomicPat of atomic_pat

    and atomic_pat
      = MarkAPat of atomic_pat mark
      | WildPat
      | IdPat of vid			(* either variable or nullary constant *)
      | ConstPat of Literal.literal

    type program = (decl list * exp)

  end
