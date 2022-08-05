(* parse-tree.sml
 *
 * Parse-tree representation of NKL programs.
 *)

structure ParseTree =
  struct

    datatype ty
      = BaseTy of Ty.base_ty
      | TupleTy of ty list
      | ArrayTy of ty
      | MaybeTy of ty

    type var = Atom.atom

  (* operator symbols *)
    datatype rator
      = < | <= | == | >= | >
      | + | - | ++
      | * | /
      | ! | ##
      | SEL of int

    datatype program = Prog of fundef list * exp

    and datadef = Data of Atom.atom * ty list

    and ty
      = BaseTy of Ty.base_ty
      | DataTy of Atom.atom
      | TupleTy of ty list
      | ArrayTy of ty
      | MaybeTy of ty
      | FunTy of {dom : ty list, rng : ty}

    and fundef = Fun of var * arg list * exp

    and exp
      = Const of Literal.literal
      | Var of var
      | Tuple of exp list
      | App of var * exp list
      | Op of rator * exp list
      | Let of arg * exp * exp
      | If of exp * exp * exp
      | PArray of exp list
      | ForEach of (exp * generator list * exp option)

    and generator = Gen of var * exp

    withtype arg = var * ty

  end

