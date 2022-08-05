structure Prim =
  struct

    datatype 'ty prim
    (* Integer operations *)
      = IADD | ISUB | IMUL | IDIV | INEG | ILT | ILTE | IGT | IGTE | IEQ | INEQ
    (* Float operations *)
      | FADD | FSUB | FMUL | FDIV | FNEG | FLT | FLTE | FGT | FGTE | FEQ | FNEQ
    (* Parallel operations *)
      | MapP of 'ty * 'ty
      | DistP of 'ty

  end

