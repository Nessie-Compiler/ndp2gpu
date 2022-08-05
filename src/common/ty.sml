(* ty.sml
 *
 * Types for the NKL and FKL representations.
 *)

structure Ty =
  struct

    datatype base_ty = BoolTy | IntTy | FloatTy

    datatype ty
      = BaseTy of base_ty
      | TupleTy of ty list
      | ArrayTy of ty
      | MaybeTy of ty
      | FunTy of {dom : ty list, rng : ty}
    (* for typing primitive operators, we need type operators *)
      | OperatorTy of {arity : int, dom : ty list, rng : ty}
      | VarTy of int
    (* to handle the type of the empty array, we include the "any" type *)
      | AnyTy

    fun toString (BaseTy BoolTy) = "bool"
      | toString (BaseTy IntTy) = "int"
      | toString (BaseTy FloatTy) = "float"
      | toString (TupleTy []) = "unit"
      | toString (TupleTy (ty::r)) = let
	  fun fmt (ty, l) = "," :: toString ty :: l
	  in
	    concat ("(" :: toString ty :: List.foldr fmt [")"] r)
	  end
      | toString (ArrayTy ty) = concat["[", toString ty, "]"]
      | toString (MaybeTy ty) = concat["<", toString ty, ">"]

    fun same (ty1 : ty, ty2) = (ty1 = ty2)

    fun typeOfLit (Literal.Bool _) = BaseTy BoolTy
      | typeOfLit (Literal.Int _) = BaseTy IntTy
      | typeOfLit (Literal.Flt _) = BaseTy FloatTy

  end

