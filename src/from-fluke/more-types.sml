(* from "More types for nested data parallelism" *)

  type tyvar = string
  
  datatype ty
    = UnitTy | BoolTy | IntTy
    | VarTy of tyvar
    | ProductTy of ty * ty
    | SumTy of ty * ty
    | FunTy of ty * ty
    | RecTy of tyvar * ty
    | ArrayTy of ty

  fun tyToString ty = let
	fun prec (ProductTy _) = 3
	  | prec (SumTy _) = 2
	  | prec (FunTy _) = 1
	  | prec (RecTy _) = 0
	  | prec _ = 4
	fun toS UnitTy = "()"
	  | toS BoolTy = "bool"
	  | toS IntTy = "int"
	  | toS (ProductTy(ty1, ty2)) = concat[paren(3, ty1), "*", paren(3, ty2)]
	  | toS (SumTy(ty1, ty2)) = concat[paren(2, ty1), "*", paren(2, ty2)]
	  | toS (FunTy(ty1, ty2)) = concat[paren(1, ty1), "*", paren(0, ty2)]
	  | toS (RecTy(x, ty)) = concat["rec ", x, ".", toS ty]
	  | toS (ArrayTy ty) = concat["[", toS ty, "]"]
	and paren (p, ty) = if prec ty <= p then concat["(", toS ty, ")"] else toS ty
	in
	  toS ty
	end

  fun flatten (ProductTy(ty1, ty2)) = ProductTy(flatten ty1, flatten ty2)
    | flatten (SumTy(ty1, ty2)) = SumTy(flatten ty1, flatten ty2)
    | flatten (FunTy(ty1, ty2)) =
	ProductTy(FunTy(flatten ty1, flatten ty2), FunTy(ArrayTy(ty1), ArrayTy(ty2)))
    | flatten (RecTy(t, ty)) = RecTy(t, flatten ty)
    | flatten ty = ty

(***** Expressions *****)

  type var = string

  datatype exp
    = ConstExp of const
    | VarExp of var
    | LambdaExp of var * ty * exp
    | AppExp of exp * exp
    | RecExp of var * ty * exp
  
  and const
    = UnitConst
    | BoolConst of bool
    | IntConst of IntInf.int

  fun constToString UnitConst = "()"
    | constToString (BoolConst b) = Bool.toString b
    | constToString (IntConst n) = IntInf.toString n

  fun expToString e = let
	fun prec (LambdaExp _) = 2
	  | prec (AppExp _) = 3
	  | prec (RecExp _) = 2
	  | prec e = 4
	fun toS (ConstExp c) = constToString c
	  | toS (VarExp x) = x
	  | toS (LambdaExp(x, ty, e)) = concat[
		"\\", x, ":", tyToString ty, "=>", toS e
	      ]
	  | toS (AppExp(AppExp(VarExp "pair", e1), e2)) = concat[
		"<", toS e1, ",", toS e2, ">"
	      ]
	  | toS (AppExp(VarExp "+", AppExp(AppExp(VarExp "pair", e1), e2))) = concat[
		paren(3, e1), "+", paren(3, e2)
	      ]
	  | toS (AppExp(e1, e2)) = concat[
		paren(3, e1), " ", paren(4, e2)
	      ]
	  | toS (RecExp(x, ty, e)) = concat[
		"rec ", x, ":", tyToString ty, " = ", toS e
	      ]
	and paren (p, e) = if prec e < p then concat["(", toS e, ")"] else toS e
	in
	  toS e
	end

  fun member (x : var, []) = false
    | member (x, y::ys) = (x = y) orelse member(x, ys)

(* helper functions for constructing common operations *)
  fun pair (a, b) = AppExp(AppExp(VarExp "pair", a), b)
  fun fst a = AppExp(VarExp "fst", a)
  fun snd a = AppExp(VarExp "snd", a)
  fun len a = AppExp(VarExp "len", a)
  fun rep (a, b) = AppExp(AppExp(VarExp "rep", a), b)
  fun add (a, b) = AppExp(VarExp "+", pair(a, b))

(* vectorize an expression *)
  fun vectorize e = (case e
	 of ConstExp _ => e
	  | VarExp _ => e
	  | LambdaExp(x, ty, e) => let
	      val seqFn = LambdaExp(x, flatten ty, vectorize e)
	      val parFn = LambdaExp(x, ArrayTy ty, liftExp([x], e))
	      in
		pair(seqFn, parFn)
	      end
	  | AppExp(e1, e2) => AppExp(fst(vectorize e1), vectorize e2)
	  | RecExp(x, ty, e) => RecExp(x, flatten ty, vectorize e)
	(* end case *))

  and liftExp (cxt, e : exp) = (case (cxt, e)
	 of ([], _) => raise Fail "bogus context"
	  | (v::vs, VarExp x) => if member(x, cxt)
		then e
		else rep(len(VarExp v), VarExp x)
	  | (_, LambdaExp(x, ty, e)) => LambdaExp(x, ArrayTy ty, liftExp(x::cxt, e))
	  | (_, AppExp(e1, e2)) => AppExp(liftExp(cxt, e1), liftExp(cxt, e2))
	  | (_, RecExp(x, ty, e)) => RecExp(x, ArrayTy ty, liftExp(cxt@[x], e))
	(* end case *))

val example = LambdaExp("x", IntTy, add(VarExp "x", VarExp "y"));

fun prExp e = print(expToString e ^ "\n");
