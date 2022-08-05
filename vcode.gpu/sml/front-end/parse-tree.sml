(* parse-tree.sml
 *
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Parse-tree representation of MinML programs.
 *)

structure ParseTree =
  struct
    type id = Atom.atom

    datatype function
      = Function of (id * fnType * statement list)
         and fnType
           = InputOutput of (basicType list * basicType list)
           | Untyped
         and basicType
           = BTInteger
           | BTBool
           | BTFloat
           | BTSegDes
           | BTChar
         and intFloat
           = IFInt
           | IFFloat
         and intBool
           = IBInt
           | IBBool
         and intBoolFloat
           = IBFInt
           | IBFBool
           | IBFFloat
         and statement
           = AddVector of intFloat
           | SubtractVector of intFloat
           | MultiplyVector of intFloat
           | DivideVector of intFloat
           | ModuloVector
           | LessThanVector of intFloat
           | LessThanEqualVector of intFloat 
           | GreaterThanVector of intFloat 
           | GreaterThanEqualVector of intFloat 
           | EqualVector of intFloat 
           | NotEqualVector of intFloat 
           | LShiftVector
           | RShiftVector
           | NotVector of intBool 
           | AndVector of intBool 
           | OrVector of intBool 
           | XorVector of intBool 
           | SelectVector of intBoolFloat 
	   | RandVector
	   | FloorVector
	   | CeilVector
	   | TruncVector
	   | RoundVector
	   | LogVector
	   | SqrtVector
	   | ExpVector
	   | SinVector
	   | CosVector
	   | TanVector
	   | ASinVector
	   | ACosVector
	   | ATanVector
	   | SinHVector
	   | CosHVector
	   | TanHVector
	   | IntToFloatVector
	   | IntToBoolVector
	   | BoolToIntVector
           | PlusScan of intFloat
           | MultScan of intFloat
           | MaxScan of intFloat
           | MinScan of intFloat
           | AndScan of intBool
           | OrScan of intBool
           | XorScan of intBool
           | PlusReduce of intFloat
           | MultReduce of intFloat
           | MaxReduce of intFloat
           | MinReduce of intFloat
           | AndReduce of intBool
           | OrReduce of intBool
           | XorReduce of intBool
           | Permute of intBoolFloat
           | DPermute of intBoolFloat
           | FPermute of intBoolFloat
           | BPermute of intBoolFloat
           | BFPermute of intBoolFloat
           | DFPermute of intBoolFloat
           | Dist of intBoolFloat
           | Index
           | Length of intBoolFloat
           | Extract of intBoolFloat
           | Replace of intBoolFloat
           | RankUp of intFloat
           | RankDown of intFloat
           | Pack of intBoolFloat
           | MakeSegdes
           | Lengths
             (* ctl_op *)
           | Copy of IntInf.int * IntInf.int
           | Pop of IntInf.int * IntInf.int
           | CPop of IntInf.int * IntInf.int
           | Pair
           | Unpair
           | Call of id
           | If of statement list * statement list
           | Exit
           | ConstInt of IntInf.int
           | ConstFD of fileCon
           | ConstFloat of Real.real
           | ConstBool of Bool.bool
           | ConstSegdes of IntInf.int
           | IntVector of IntInf.int list
           | BoolVector of Bool.bool list
           | FloatVector of Real.real list
           | SegdesVector of IntInf.int list
           | StringVector of String.string
             (* io_op *)
           | StartTimer
           | StopTimer
           | Read of basicType
           | Write of basicType
           | FOpen
           | FClose
           | FRead of basicType
           | FReadChar
           | FWrite of basicType
           | Spawn
           | SRand (* of IntInf.int *)
         and fileCon
           = StdIn
           | StdOut
           | StdErr
           | NullStream

    type program = function list

  end
