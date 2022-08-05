(* prim-ty.sml
 *
 * Representation types after flattening
 *)

structure FlatTy =
  struct

    datatype base_ty
      = BoolTy | IntTy | FloatTy
      | BoolArray | IntArray | FloatArray

    datatype ty
      = BaseTy of base_ty
      | TupleTy of ty list
      | SumTy of ty * ty
      | FunTy of ty * ty

  end

