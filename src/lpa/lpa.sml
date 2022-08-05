(* lpa.sml
 *
 * LPA (lambda with parallel arrays) representation.
 *)

structure LPA =
  struct

    datatype ty
      = BaseTy of base_ty
      | TupleTy of ty list
      | SumTy of ty * ty
      | FunTy of ty * ty
      | RecTy of tyvar * ty
      | VarTy of tyvar
      | ArrayTy of ty

    type var = Var.var

    datatype exp
      = Const of Literal.literal
      | Var of var
      | Pair of exp * exp
      | App of var * exp list
      | Fun of var * var * exp * exp
      | Let of var * exp * exp
      | If of exp * exp * exp
      | PArray of exp list

  end

