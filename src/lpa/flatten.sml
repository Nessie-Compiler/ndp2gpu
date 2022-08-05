structure Flatten : sig

    val ty : LPA.ty -> PrimTy.ty

  end = struct

    val intTy = PrimTy.BaseTy PrimTy.IntTy
    val boolArray = PrimTy.BaseTy PrimTy.BoolArray
    val intArray = PrimTy.BaseTy PrimTy.IntArray
    val floatArray = PrimTy.BaseTy PrimTy.FloatArray

  (* compute the flat representation of a type *)
    fun flattenTy ty = let
	  fun flattenArrayTy ty = (case ty
		 of LPA.BaseTy LPA.BoolTy => boolArray
		  | LPA.BaseTy LPA.IntTy => intArray
		  | LPA.BaseTy LPA.FloatTy => floatArray
		  | LPA.TupleTy[] => intTy
		  | LPA.TupleTy tys =>
		      PrimTy.TupleTy(List.map flattenArrayTy tys)
		  | LPA.SumTy(ty1, ty2) => PrimTy.TupleTy[
			boolArray, flattenArrayTy ty1, flattenArrayTy ty2
		      ]
		  | LPA.FunTy(ty1, ty2) =>
		      PrimTy.FunTy(flattenArrayTy ty1, flattenArrayTy ty2)
		  | LPA.ArrayTy ty =>
		      PrimTy.TupleTy[intArray, flattenArrayTy ty]
		(* end case *))
	  and flatten (LPA.BaseTy bty) = let
		val bty = (case bty
		       of LPA.BoolTy => PrimTy.BoolTy
			| LPA.IntTy => PrimTy.IntTy
			| LPA.FloatTy => PrimTy.FloatTy
		      (* end case *))
		in
		  PrimTy.BaseTy bty
		end
	    | flatten (LPA.TupleTy tys) = PrimTy.TupleTy(List.map flatten tys)
	    | flatten (LPA.SumTy(ty1, ty2)) = PrimTy.SumTy(flatten ty1, flatten ty2)
	    | flatten (LPA.FunTy(ty1, ty2)) =
		PrimTy.FunTy(flatten ty1, flatten ty2)
	    | flatten (LPA.RecTy(tv, ty)) = raise Fail "RecTy"
	    | flatten (LPA.VarTy tv) = raise Fail "VarTy"
	    | flatten (LPA.ArrayTy ty) = flattenArrayTy ty
	  in
	    flatten ty
	  end

    val ty = flattenTy

  end

