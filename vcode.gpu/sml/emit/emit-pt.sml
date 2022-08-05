structure EmitPT =
struct

    structure PP = TextIOPP

    open HashTable
    open ParseTree

    exception NotInHashTable
    val hash_fn : string->word = HashString.hashString
    val cmp_fn : (string*string)->bool = (op =)
    val initial_size : int = 101
    val idCount = ref 0
    val identifierMap : (string,string) hash_table = mkTable (hash_fn, cmp_fn)
                                                        (initial_size, NotInHashTable)

    fun str strm s = PP.string strm s
    fun sp strm = PP.space strm 1
    fun nl strm = PP.newline strm

    fun genIdentifier(idtype) = (idCount := (!idCount)+1;
                               idtype ^ Int.toString (!idCount))
    fun getCIdentifier(vcodeIdentifier) = let
    in
        lookup identifierMap vcodeIdentifier handle NotInHashTable => (let
                                                 val translated = genIdentifier("fun")
                                                    in
                                                        insert identifierMap(vcodeIdentifier, translated) ; translated
                                                    end)
    end
        

    fun genPopIntPtr(strm, var) = (str strm ("int *" ^ var ^ " = g_Stack->PopIntPtr();"); nl strm)
    fun genPopSegDes(strm, var) = (str strm ("int *" ^ var ^ " = g_Stack->PopSegDes();"); nl strm)
    fun genPushIntPtr(strm, var) = (str strm ("g_Stack->PushIntPtr(" ^ var ^ ");") ; nl strm)
    fun genPushSegDes(strm, var) = (str strm ("g_Stack->PushSegDes(" ^ var ^ ");") ; nl strm)
    fun emitMain(strm, Function(id, _, _)) = (
        str strm ("int main () {");
        nl strm;
        str strm ("  " ^ (getCIdentifier (Atom.toString id)) ^ "();");
        nl strm;
        str strm ("}");
        nl strm)

    fun emitProgram(strm, functions) = (emitHeader strm;
                                        emitFunctionDeclarations (strm, functions);
                                        emitMain (strm, hd functions);
                                        emitFunctions (strm, functions))
    and emitHeader strm =
        (str strm "#include \"ndp2gpu.h\"";
         nl strm;
         str strm "#include <stdio.h>";
         nl strm)
    and emitFunctionDeclarations (strm, []) = nl strm
      | emitFunctionDeclarations (strm, Function(id,_, _)::functions) =
        (str strm (concat["extern \"C\" void ", (getCIdentifier (Atom.toString id)) ^ "();"]);
         nl strm;
         emitFunctionDeclarations (strm, functions))
    and emitFunctions(strm, []) = ()
      | emitFunctions(strm, function::functions) =
        (emitFunction (strm, function);
         emitFunctions (strm, functions))
    and emitFunction(strm, Function(id, typ, statements)) =
        (PP.openHVBox strm (PP.Abs 2);
         str strm ("void " ^ (getCIdentifier (Atom.toString id)) ^ "() {");
         nl strm;
         emitStatements(strm, statements);
         PP.closeBox strm;
         str strm "}";
         nl strm)
    and emitStatements(strm, []) = ()
      | emitStatements(strm, statement::statements) =
        (emitStatement(strm, statement);
         emitStatements(strm, statements))
    and emitStatement(strm, AddVector(IFInt)) =
        (str strm ("VectorAddInts();"); nl strm)
      | emitStatement(strm, AddVector(arg:intFloat)) =
        (str strm "/* UNIMPLEMENTED AddVector */" ; nl strm)
      | emitStatement(strm, SubtractVector(arg:intFloat)) =
        (str strm "/* Unimplemented SubtractVector */" ; nl strm)
      | emitStatement(strm, MultiplyVector(IFInt)) =
        (str strm "VectorMultiplyInts();"; nl strm)
      | emitStatement(strm, MultiplyVector(arg:intFloat)) =
        (str strm "/* Unimplemented MultiplyVector */" ; nl strm)
      | emitStatement(strm, DivideVector(arg:intFloat)) =
        (str strm "/* Unimplemented DivideVector */" ; nl strm)
      | emitStatement(strm, ModuloVector) =
        (str strm "/* Unimplemented ModuloVector */" ; nl strm)
      | emitStatement(strm, LessThanVector(arg:intFloat)) =
        (str strm "/* Unimplemented LessThanVector */" ; nl strm)
      | emitStatement(strm, LessThanEqualVector(arg:intFloat) ) =
        (str strm "/* Unimplemented LessThanEqualVector */" ; nl strm)
      | emitStatement(strm, GreaterThanVector(arg:intFloat) ) =
        (str strm "/* Unimplemented GreaterThanVector */" ; nl strm)
      | emitStatement(strm, GreaterThanEqualVector(arg:intFloat) ) =
        (str strm "/* Unimplemented GreaterThanEqualVector */" ; nl strm)
      | emitStatement(strm, EqualVector(arg:intFloat) ) =
        (str strm "/* Unimplemented EqualVector */" ; nl strm)
      | emitStatement(strm, NotEqualVector(arg:intFloat) ) =
        (str strm "/* Unimplemented NotEqualVector */" ; nl strm)
      | emitStatement(strm, LShiftVector) =
        (str strm "/* Unimplemented LShiftVector */" ; nl strm)
      | emitStatement(strm, RShiftVector) =
        (str strm "/* Unimplemented RShiftVector */" ; nl strm)
      | emitStatement(strm, NotVector(arg:intBool) ) =
        (str strm "/* Unimplemented NotVector */" ; nl strm)
      | emitStatement(strm, AndVector(arg:intBool) ) =
        (str strm "/* Unimplemented AndVector */" ; nl strm)
      | emitStatement(strm, OrVector(arg:intBool) ) =
        (str strm "/* Unimplemented OrVector */" ; nl strm)
      | emitStatement(strm, XorVector(arg:intBool) ) =
        (str strm "/* Unimplemented XorVector */" ; nl strm)
      | emitStatement(strm, SelectVector(arg:intBoolFloat) ) =
        (str strm "/* Unimplemented SelectVector */" ; nl strm)
      | emitStatement(strm, RandVector) =
        (str strm "/* Unimplemented RandVector */" ; nl strm)
      | emitStatement(strm, FloorVector) =
        (str strm "/* Unimplemented FloorVector */" ; nl strm)
      | emitStatement(strm, CeilVector) =
        (str strm "/* Unimplemented CeilVector */" ; nl strm)
      | emitStatement(strm, TruncVector) =
        (str strm "/* Unimplemented TruncVector */" ; nl strm)
      | emitStatement(strm, RoundVector) =
        (str strm "/* Unimplemented RoundVector */" ; nl strm)
      | emitStatement(strm, LogVector) =
        (str strm "/* Unimplemented LogVector */" ; nl strm)
      | emitStatement(strm, SqrtVector) =
        (str strm "/* Unimplemented SqrtVector */" ; nl strm)
      | emitStatement(strm, ExpVector) =
        (str strm "/* Unimplemented ExpVector */" ; nl strm)
      | emitStatement(strm, SinVector) =
        (str strm "/* Unimplemented SinVector */" ; nl strm)
      | emitStatement(strm, CosVector) =
        (str strm "/* Unimplemented CosVector */" ; nl strm)
      | emitStatement(strm, TanVector) =
        (str strm "/* Unimplemented TanVector */" ; nl strm)
      | emitStatement(strm, ASinVector) =
        (str strm "/* Unimplemented ASinVector */" ; nl strm)
      | emitStatement(strm, ACosVector) =
        (str strm "/* Unimplemented ACosVector */" ; nl strm)
      | emitStatement(strm, ATanVector) =
        (str strm "/* Unimplemented ATanVector */" ; nl strm)
      | emitStatement(strm, SinHVector) =
        (str strm "/* Unimplemented SinHVector */" ; nl strm)
      | emitStatement(strm, CosHVector) =
        (str strm "/* Unimplemented CosHVector */" ; nl strm)
      | emitStatement(strm, TanHVector) =
        (str strm "/* Unimplemented TanHVector */" ; nl strm)
      | emitStatement(strm, IntToFloatVector) =
        (str strm "/* Unimplemented IntToFloatVector */" ; nl strm)
      | emitStatement(strm, IntToBoolVector) =
        (str strm "/* Unimplemented IntToBoolVector */" ; nl strm)
      | emitStatement(strm, BoolToIntVector) =
        (str strm "/* Unimplemented BoolToIntVector */" ; nl strm)
      | emitStatement(strm, PlusScan(arg:intFloat)) =
        (str strm "/* Unimplemented PlusScan */" ; nl strm)
      | emitStatement(strm, MultScan(arg:intFloat)) =
        (str strm "/* Unimplemented MultScan */" ; nl strm)
      | emitStatement(strm, MaxScan(arg:intFloat)) =
        (str strm "/* Unimplemented MaxScan */" ; nl strm)
      | emitStatement(strm, MinScan(arg:intFloat)) =
        (str strm "/* Unimplemented MinScan */" ; nl strm)
      | emitStatement(strm, AndScan(arg:intBool)) =
        (str strm "/* Unimplemented AndScan */" ; nl strm)
      | emitStatement(strm, OrScan(arg:intBool)) =
        (str strm "/* Unimplemented OrScan */" ; nl strm)
      | emitStatement(strm, XorScan(arg:intBool)) =
        (str strm "/* Unimplemented XorScan */" ; nl strm)
      | emitStatement(strm, PlusReduce(arg:intFloat)) =
        (str strm "/* Unimplemented PlusReduce */" ; nl strm)
      | emitStatement(strm, MultReduce(arg:intFloat)) =
        (str strm "/* Unimplemented MultReduce */" ; nl strm)
      | emitStatement(strm, MaxReduce(arg:intFloat)) =
        (str strm "/* Unimplemented MaxReduce */" ; nl strm)
      | emitStatement(strm, MinReduce(arg:intFloat)) =
        (str strm "/* Unimplemented MinReduce */" ; nl strm)
      | emitStatement(strm, AndReduce(arg:intBool)) =
        (str strm "/* Unimplemented AndReduce */" ; nl strm)
      | emitStatement(strm, OrReduce(arg:intBool)) =
        (str strm "/* Unimplemented OrReduce */" ; nl strm)
      | emitStatement(strm, XorReduce(arg:intBool)) =
        (str strm "/* Unimplemented XorReduce */" ; nl strm)
      | emitStatement(strm, Permute(arg:intBoolFloat)) =
        (str strm "/* Unimplemented Permute */" ; nl strm)
      | emitStatement(strm, DPermute(IBFInt)) = 
        (str strm "DPermute();" ; nl strm)
      | emitStatement(strm, DPermute(arg:intBoolFloat)) =
        (str strm "/* Unimplemented DPermute */" ; nl strm)
      | emitStatement(strm, FPermute(arg:intBoolFloat)) =
        (str strm "/* Unimplemented FPermute */" ; nl strm)
      | emitStatement(strm, BPermute(arg:intBoolFloat)) =
        (str strm "/* Unimplemented BPermute */" ; nl strm)
      | emitStatement(strm, BFPermute(arg:intBoolFloat)) =
        (str strm "/* Unimplemented BFPermute */" ; nl strm)
      | emitStatement(strm, DFPermute(arg:intBoolFloat)) =
        (str strm "/* Unimplemented DFPermute */" ; nl strm)
      | emitStatement(strm, Dist(IBFInt)) = 
        (str strm "DistributeIntegers();" ; nl strm)
      | emitStatement(strm, Dist(arg:intBoolFloat)) =
        (str strm "/* Unimplemented Dist */" ; nl strm)
      | emitStatement(strm, Index) =
        (str strm "Index();" ; nl strm)
      | emitStatement(strm, Length(arg:intBoolFloat)) =
        (str strm "/* Unimplemented Length */" ; nl strm)
      | emitStatement(strm, Extract(arg:intBoolFloat)) =
        (str strm "/* Unimplemented Extract */" ; nl strm)
      | emitStatement(strm, Replace(arg:intBoolFloat)) =
        (str strm "/* Unimplemented Replace */" ; nl strm)
      | emitStatement(strm, RankUp(arg:intFloat)) =
        (str strm "/* Unimplemented RankUp */" ; nl strm)
      | emitStatement(strm, RankDown(arg:intFloat)) =
        (str strm "/* Unimplemented RankDown */" ; nl strm)
      | emitStatement(strm, Pack(arg:intBoolFloat)) =
        (str strm "/* Unimplemented Pack */" ; nl strm)
      | emitStatement(strm, MakeSegdes) =
        (str strm "MakeSegDes();" ; nl strm)
      | emitStatement(strm, Lengths) = 
        (str strm "Lengths();" ; nl strm)
      | emitStatement(strm, Copy(arg1:IntInf.int, arg2:IntInf.int)) =
        (str strm "g_Stack->Copy(";
         str strm (IntInf.toString arg1);
         str strm ", ";
         str strm (IntInf.toString arg2);
         str strm ");";
         nl strm)
      | emitStatement(strm, Pop(arg1:IntInf.int, arg2:IntInf.int)) =
        (str strm "g_Stack->Pop(";
         str strm (IntInf.toString arg1);
         str strm ", ";
         str strm (IntInf.toString arg2);
         str strm ");";
         nl strm)
      | emitStatement(strm, CPop(arg1:IntInf.int, arg2:IntInf.int)) =
        (str strm "g_Stack->CPop(";
         str strm (IntInf.toString arg1);
         str strm ", ";
         str strm (IntInf.toString arg2);
         str strm ");";
         nl strm)
      | emitStatement(strm, Pair) =
        (str strm "/* Unimplemented Pair */" ; nl strm)
      | emitStatement(strm, Unpair) =
        (str strm "/* Unimplemented Unpair */" ; nl strm)
      | emitStatement(strm, Call(arg:id)) =
        (str strm (getCIdentifier(Atom.toString arg));
         str strm "();";
         nl strm)
      | emitStatement(strm, If(thens:statement list, elses:statement list)) =
        (str strm "/* Unimplemented If */" ; nl strm)
      | emitStatement(strm, Exit) =
        (str strm "/* Unimplemented Exit */" ; nl strm)
      | emitStatement(strm, ConstInt(arg:IntInf.int)) =
        (str strm ("g_Stack->PushInt(" ^ (IntInf.toString arg) ^ ");");
         nl strm)
      | emitStatement(strm, ConstFD(arg:fileCon)) =
        (str strm "/* Unimplemented ConstFD */" ; nl strm)
      | emitStatement(strm, ConstFloat(arg:Real.real)) =
        (str strm "/* Unimplemented ConstFloat */" ; nl strm)
      | emitStatement(strm, ConstBool(Bool.true)) =
        (str strm "g_Stack->PushBool(1);";
         nl strm)
      | emitStatement(strm, ConstBool(Bool.false)) =
        (str strm "g_Stack->PushBool(0);";
         nl strm)
      | emitStatement(strm, ConstSegdes(arg:IntInf.int)) =
        (str strm "/* Unimplemented ConstSegdes */" ; nl strm)
      | emitStatement(strm, IntVector(arg:IntInf.int list)) =
        (str strm "/* Unimplemented IntVector */" ; nl strm)
      | emitStatement(strm, BoolVector(arg:Bool.bool list)) =
        (str strm "/* Unimplemented BoolVector */" ; nl strm)
      | emitStatement(strm, FloatVector(arg:Real.real list)) =
        (str strm "/* Unimplemented FloatVector */" ; nl strm)
      | emitStatement(strm, SegdesVector(arg:IntInf.int list)) =
        (str strm "/* Unimplemented SegdesVector */" ; nl strm)
      | emitStatement(strm, StringVector(arg:String.string)) =
        (str strm ("g_Stack->PushString(\"" ^ (String.toCString arg) ^ "\", " ^ (Int.toString (String.size arg)) ^ ");");
         nl strm)
      | emitStatement(strm, StartTimer) =
        (str strm "/* Unimplemented StartTimer */" ; nl strm)
      | emitStatement(strm, StopTimer) =
        (str strm "/* Unimplemented StopTimer */" ; nl strm)
      | emitStatement(strm, Read(arg:basicType)) =
        (str strm "/* Unimplemented Read */" ; nl strm)
      | emitStatement(strm, Write(BTChar)) =  let
            val i = genIdentifier("ignored")
            val retval = genIdentifier("retval")
            val pstr = genIdentifier("pstr")
        in
            (str strm ("int " ^ i ^ ";");
             nl strm;
             str strm ("int " ^ retval ^ ";");
             nl strm;
             str strm ("char *" ^ pstr ^ " = g_Stack->PopString(" ^ i ^ ");");
             nl strm;
             str strm (retval ^ " = printf(\"%s\", " ^ pstr ^ ");");
             nl strm;
             str strm ("g_Stack->PushBool(" ^ retval ^"==0);");
             nl strm;
             str strm ("g_Stack->PushString(\"\",0);");
             nl strm)
        end
      | emitStatement(strm, Write(arg:basicType)) =
        (str strm "/* Unimplemented Write */" ; nl strm)
      | emitStatement(strm, FOpen) =
        (str strm "/* Unimplemented FOpen */" ; nl strm)
      | emitStatement(strm, FClose) =
        (str strm "/* Unimplemented FClose */" ; nl strm)
      | emitStatement(strm, FRead(arg:basicType)) =
        (str strm "/* Unimplemented FRead */" ; nl strm)
      | emitStatement(strm, FReadChar) =
        (str strm "/* Unimplemented FReadChar */" ; nl strm)
      | emitStatement(strm, FWrite(arg:basicType)) =
        (str strm "/* Unimplemented FWrite */" ; nl strm)
      | emitStatement(strm, Spawn) =
        (str strm "/* Unimplemented Spawn */" ; nl strm)
      | emitStatement(strm, SRand (* of IntInf.int *)) =
        (str strm "/* Unimplemented SRand */" ; nl strm)

    fun emit(strm, t) = emitProgram(strm, t)

    fun output (outS, prog) = let
	  val dev = SimpleTextIODev.openDev {dst=outS, wid=90}
	  val strm = PP.openStream dev
	  in
	    emit (strm, prog);
	    PP.flushStream strm;
	    PP.closeStream strm
	  end

    fun print prog = output (TextIO.stdOut, prog)

  end

