structure Var =
  struct

    datatype kind = Scalar of var option ref | Lifted

    and var = V of {
	id : word,
	name : string,
	ty : Ty.ty,
	kind : kind,
	isPrim : bool
      }

    local
      val cnt = ref 0w0
    in
    fun nextId () = let val id = !cnt in cnt := !cnt+0w1; id end
    end

    fun make (name, ty, kind, isPrim) = V{
	    id = nextId(),
	    name = name,
	    ty = ty,
	    kind = kind,
	    isPrim = isPrim
	  }

    fun name (V{name, ...}) = name

    fun typeOf (V{ty, ...}) = ty

    fun toString (V{name, kind=Scalar _, ...}) = name
      | toString (V{name, ...}) = name ^ "^"

    fun new (x, ty) = make(x, ty, Scalar(ref NONE), false)

    fun prim (x, arity, dom, rng) =
	  make(x, Ty.OperatorTy{arity=arity, dom=dom, rng=rng}, Scalar(ref NONE), true)

    fun lift (V{name, ty, kind, isPrim, ...}) = (case kind
	   of Scalar(ref(SOME x)) => x
	    | Scalar r => let val x' = make(name, Lifted, isPrim)
		in
		  r := SOME x';
		  x'
		end
	    | Lifted => raise Fail "lift"
	  (* end case *))

    fun lifted (V{kind=Lifted, ...}) = true
      | lifted _ = false

    fun isPrim (V{isPrim, ...}) = isPrim

    fun same (V{id=a, ...}, V{id=b, ...}) = (a = b)
    fun compare (V{id=a, ...}, V{id=b, ...}) = Word.compare(a, b)

    structure Map = RedBlackMapFn (
      struct
	type ord_key = var
	val compare = compare
      end)

    structure Set = RedBlackSetFn (
      struct
	type ord_key = var
	val compare = compare
      end)

  end
 
