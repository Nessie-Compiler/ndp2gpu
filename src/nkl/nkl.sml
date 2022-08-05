(* Nkl.sml
 *
 * Normalized NKL representation.  In addition to using unique names for
 * variables, we simplify comprehensions by lifting expressions in the
 * generators and conditionals.
 *)

structure NKL =
  struct

    type var = Var.var

    datatype program = Prog of fundef list * exp

    and fundef = Fun of var * var list * exp

    and exp
      = Const of Literal.literal
      | Var of var
      | Tuple of exp list
      | App of var * exp list
      | Let of var * exp * exp
      | If of exp * exp * exp
      | PArray of exp list
      | ForEach of (exp * generator list)

    and generator = Gen of var * var

  end
