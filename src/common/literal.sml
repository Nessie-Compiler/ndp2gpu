structure Literal =
  struct

    datatype literal
      = Bool of bool
      | Int of IntInf.int
      | Flt of string

    fun toString (Bool b) = Bool.toString b
      | toString (Int n) = if (n < 0)
	  then "-" ^ IntInf.toString(~n)
	  else IntInf.toString n
      | toString (Flt f) = f

    fun int i = Int(IntInf.fromInt i)

  end

