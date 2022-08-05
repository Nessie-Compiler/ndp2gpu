structure Literal =
  struct

    datatype literal
      = Int of IntInf.int
      | Flt of string
      | Str of string

    fun toString (Int n) = if (n < 0)
	  then "-" ^ IntInf.toString(~n)
	  else IntInf.toString n
      | toString (Flt f) = f
      | toString (Str s) = concat["\"", String.toString s, "\""]

    fun int i = Int(IntInf.fromInt i)

  end

