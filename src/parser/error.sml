structure Error =
  struct

  (* global flag to record the existance of errors *)
    val anyErrors = ref false

  (* the current input file *)
    val sourceFile = ref ""

  (* the current sourcemap *)
    val sourceMap = ref(AntlrStreamPos.mkSourcemap())

  (* print an error message and record the fact that there was an error *)
    fun say l = (
          anyErrors := true;
          TextIO.output(TextIO.stdErr, String.concat l);
          TextIO.output1(TextIO.stdErr, #"\n"))

  end
