structure 
VCodeTokens = struct

    datatype token = EOF
      | STREAM of string
      | STRING of string
      | BOOLEAN of string
      | REAL of string
      | NUM of IntInf.int
      | LABEL of Atom.atom
      | RP
      | LP
      | SEGDES
      | CHAR
      | FLOAT
      | INT
      | BOOL
      | SRAND
      | STOP_TIMER
      | START_TIMER
      | EXIT
      | FREAD_CHAR
      | FREAD
      | FWRITE
      | FCLOSE
      | FOPEN
      | WRITE
      | READ
      | CONST
      | ENDIF
      | ELSE
      | IF
      | RET
      | CALL
      | UNPAIR
      | PAIR
      | CPOP
      | POP
      | COPY
      | LENGTHS
      | MAKE_SEGDES
      | LENGTH
      | INDEX
      | DIST
      | RANK_DOWN
      | RANK_UP
      | PACK
      | REPLACE
      | EXTRACT
      | DFPERMUTE
      | BFPERMUTE
      | BPERMUTE
      | FPERMUTE
      | DPERMUTE
      | PERMUTE
      | XOR_REDUCE
      | OR_REDUCE
      | AND_REDUCE
      | MIN_REDUCE
      | MAX_REDUCE
      | MUL_REDUCE
      | ADD_REDUCE
      | XOR_SCAN
      | OR_SCAN
      | AND_SCAN
      | MIN_SCAN
      | MAX_SCAN
      | MUL_SCAN
      | ADD_SCAN
      | TANH
      | COSH
      | SINH
      | ATAN
      | ACOS
      | ASIN
      | TAN
      | COS
      | SIN
      | EXP
      | SQRT
      | LOG
      | B_TO_I
      | I_TO_B
      | I_TO_F
      | ROUND
      | TRUNC
      | CEIL
      | FLOOR
      | RAND
      | SELECT
      | XOR
      | OR
      | AND
      | NOT
      | RSHIFT
      | LSHIFT
      | NEQ
      | EQ
      | GTE
      | GT
      | LTE
      | LT
      | MOD
      | DIV
      | MUL
      | SUB
      | ADD
      | FUNC

    val allToks = [EOF, RP, LP, SEGDES, CHAR, FLOAT, INT, BOOL, SRAND, STOP_TIMER, START_TIMER, EXIT, FREAD_CHAR, FREAD, FWRITE, FCLOSE, FOPEN, WRITE, READ, CONST, ENDIF, ELSE, IF, RET, CALL, UNPAIR, PAIR, CPOP, POP, COPY, LENGTHS, MAKE_SEGDES, LENGTH, INDEX, DIST, RANK_DOWN, RANK_UP, PACK, REPLACE, EXTRACT, DFPERMUTE, BFPERMUTE, BPERMUTE, FPERMUTE, DPERMUTE, PERMUTE, XOR_REDUCE, OR_REDUCE, AND_REDUCE, MIN_REDUCE, MAX_REDUCE, MUL_REDUCE, ADD_REDUCE, XOR_SCAN, OR_SCAN, AND_SCAN, MIN_SCAN, MAX_SCAN, MUL_SCAN, ADD_SCAN, TANH, COSH, SINH, ATAN, ACOS, ASIN, TAN, COS, SIN, EXP, SQRT, LOG, B_TO_I, I_TO_B, I_TO_F, ROUND, TRUNC, CEIL, FLOOR, RAND, SELECT, XOR, OR, AND, NOT, RSHIFT, LSHIFT, NEQ, EQ, GTE, GT, LTE, LT, MOD, DIV, MUL, SUB, ADD, FUNC]

    fun toString tok =
(case (tok)
 of (EOF) => "EOF"
  | (STREAM(_)) => "STREAM"
  | (STRING(_)) => "STRING"
  | (BOOLEAN(_)) => "BOOLEAN"
  | (REAL(_)) => "REAL"
  | (NUM(_)) => "NUM"
  | (LABEL(_)) => "LABEL"
  | (RP) => ")"
  | (LP) => "("
  | (SEGDES) => "SEGDES"
  | (CHAR) => "CHAR"
  | (FLOAT) => "FLOAT"
  | (INT) => "INT"
  | (BOOL) => "BOOL"
  | (SRAND) => "SRAND"
  | (STOP_TIMER) => "STOP_TIMER"
  | (START_TIMER) => "START_TIMER"
  | (EXIT) => "EXIT"
  | (FREAD_CHAR) => "FREAD_CHAR"
  | (FREAD) => "FREAD"
  | (FWRITE) => "FWRITE"
  | (FCLOSE) => "FCLOSE"
  | (FOPEN) => "FOPEN"
  | (WRITE) => "WRITE"
  | (READ) => "READ"
  | (CONST) => "CONST"
  | (ENDIF) => "ENDIF"
  | (ELSE) => "ELSE"
  | (IF) => "IF"
  | (RET) => "RET"
  | (CALL) => "CALL"
  | (UNPAIR) => "UNPAIR"
  | (PAIR) => "PAIR"
  | (CPOP) => "CPOP"
  | (POP) => "POP"
  | (COPY) => "COPY"
  | (LENGTHS) => "LENGTHS"
  | (MAKE_SEGDES) => "MAKE_SEGDES"
  | (LENGTH) => "LENGTH"
  | (INDEX) => "INDEX"
  | (DIST) => "DIST"
  | (RANK_DOWN) => "RANK_DOWN"
  | (RANK_UP) => "RANK_UP"
  | (PACK) => "PACK"
  | (REPLACE) => "REPLACE"
  | (EXTRACT) => "EXTRACT"
  | (DFPERMUTE) => "DFPERMUTE"
  | (BFPERMUTE) => "BFPERMUTE"
  | (BPERMUTE) => "BPERMUTE"
  | (FPERMUTE) => "FPERMUTE"
  | (DPERMUTE) => "DPERMUTE"
  | (PERMUTE) => "PERMUTE"
  | (XOR_REDUCE) => "XOR_REDUCE"
  | (OR_REDUCE) => "OR_REDUCE"
  | (AND_REDUCE) => "AND_REDUCE"
  | (MIN_REDUCE) => "MIN_REDUCE"
  | (MAX_REDUCE) => "MAX_REDUCE"
  | (MUL_REDUCE) => "*_REDUCE"
  | (ADD_REDUCE) => "+_REDUCE"
  | (XOR_SCAN) => "XOR_SCAN"
  | (OR_SCAN) => "OR_SCAN"
  | (AND_SCAN) => "AND_SCAN"
  | (MIN_SCAN) => "MIN_SCAN"
  | (MAX_SCAN) => "MAX_SCAN"
  | (MUL_SCAN) => "*_SCAN"
  | (ADD_SCAN) => "+_SCAN"
  | (TANH) => "TANH"
  | (COSH) => "COSH"
  | (SINH) => "SINH"
  | (ATAN) => "ATAN"
  | (ACOS) => "ACOS"
  | (ASIN) => "ASIN"
  | (TAN) => "TAN"
  | (COS) => "COS"
  | (SIN) => "SIN"
  | (EXP) => "EXP"
  | (SQRT) => "SQRT"
  | (LOG) => "LOG"
  | (B_TO_I) => "B_TO_I"
  | (I_TO_B) => "I_TO_B"
  | (I_TO_F) => "I_TO_F"
  | (ROUND) => "ROUND"
  | (TRUNC) => "TRUNC"
  | (CEIL) => "CEIL"
  | (FLOOR) => "FLOOR"
  | (RAND) => "RAND"
  | (SELECT) => "SELECT"
  | (XOR) => "XOR"
  | (OR) => "OR"
  | (AND) => "AND"
  | (NOT) => "NOT"
  | (RSHIFT) => "RSHIFT"
  | (LSHIFT) => "LSHIFT"
  | (NEQ) => "!="
  | (EQ) => "="
  | (GTE) => ">="
  | (GT) => ">"
  | (LTE) => "<="
  | (LT) => "<"
  | (MOD) => "%"
  | (DIV) => "/"
  | (MUL) => "*"
  | (SUB) => "-"
  | (ADD) => "+"
  | (FUNC) => "FUNC"
(* end case *))
    fun isKW tok =
(case (tok)
 of (EOF) => false
  | (STREAM(_)) => false
  | (STRING(_)) => false
  | (BOOLEAN(_)) => false
  | (REAL(_)) => false
  | (NUM(_)) => false
  | (LABEL(_)) => false
  | (RP) => false
  | (LP) => false
  | (SEGDES) => false
  | (CHAR) => false
  | (FLOAT) => false
  | (INT) => false
  | (BOOL) => false
  | (SRAND) => false
  | (STOP_TIMER) => false
  | (START_TIMER) => false
  | (EXIT) => false
  | (FREAD_CHAR) => false
  | (FREAD) => false
  | (FWRITE) => false
  | (FCLOSE) => false
  | (FOPEN) => false
  | (WRITE) => false
  | (READ) => false
  | (CONST) => false
  | (ENDIF) => false
  | (ELSE) => false
  | (IF) => false
  | (RET) => false
  | (CALL) => false
  | (UNPAIR) => false
  | (PAIR) => false
  | (CPOP) => false
  | (POP) => false
  | (COPY) => false
  | (LENGTHS) => false
  | (MAKE_SEGDES) => false
  | (LENGTH) => false
  | (INDEX) => false
  | (DIST) => false
  | (RANK_DOWN) => false
  | (RANK_UP) => false
  | (PACK) => false
  | (REPLACE) => false
  | (EXTRACT) => false
  | (DFPERMUTE) => false
  | (BFPERMUTE) => false
  | (BPERMUTE) => false
  | (FPERMUTE) => false
  | (DPERMUTE) => false
  | (PERMUTE) => false
  | (XOR_REDUCE) => false
  | (OR_REDUCE) => false
  | (AND_REDUCE) => false
  | (MIN_REDUCE) => false
  | (MAX_REDUCE) => false
  | (MUL_REDUCE) => false
  | (ADD_REDUCE) => false
  | (XOR_SCAN) => false
  | (OR_SCAN) => false
  | (AND_SCAN) => false
  | (MIN_SCAN) => false
  | (MAX_SCAN) => false
  | (MUL_SCAN) => false
  | (ADD_SCAN) => false
  | (TANH) => false
  | (COSH) => false
  | (SINH) => false
  | (ATAN) => false
  | (ACOS) => false
  | (ASIN) => false
  | (TAN) => false
  | (COS) => false
  | (SIN) => false
  | (EXP) => false
  | (SQRT) => false
  | (LOG) => false
  | (B_TO_I) => false
  | (I_TO_B) => false
  | (I_TO_F) => false
  | (ROUND) => false
  | (TRUNC) => false
  | (CEIL) => false
  | (FLOOR) => false
  | (RAND) => false
  | (SELECT) => false
  | (XOR) => false
  | (OR) => false
  | (AND) => false
  | (NOT) => false
  | (RSHIFT) => false
  | (LSHIFT) => false
  | (NEQ) => false
  | (EQ) => false
  | (GTE) => false
  | (GT) => false
  | (LTE) => false
  | (LT) => false
  | (MOD) => false
  | (DIV) => false
  | (MUL) => false
  | (SUB) => false
  | (ADD) => false
  | (FUNC) => false
(* end case *))


  fun toksToString toks = String.concatWith " " (map toString toks)

  fun isEOF EOF = true
    | isEOF _ = false

end

functor VCodeParseFn(Lex : ANTLR_LEXER) = struct

  local
    structure Tok = 
VCodeTokens
    structure UserCode = struct


  structure VC = VCode


fun Function_PROD_1_ACT (RET, FUNC, LABEL, Instruction, RET_SPAN : (Lex.pos * Lex.pos), FUNC_SPAN : (Lex.pos * Lex.pos), LABEL_SPAN : (Lex.pos * Lex.pos), Instruction_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.FUNC(LABEL, Instruction))
fun Instruction_PROD_1_ACT (ADD, IntOrFloat, ADD_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.ADD IntOrFloat)
fun Instruction_PROD_2_ACT (SUB, IntOrFloat, SUB_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.SUB IntOrFloat)
fun Instruction_PROD_3_ACT (MUL, IntOrFloat, MUL_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.MUL IntOrFloat)
fun Instruction_PROD_4_ACT (DIV, IntOrFloat, DIV_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.DIV IntOrFloat)
fun Instruction_PROD_5_ACT (MOD, MOD_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.MOD)
fun Instruction_PROD_6_ACT (LT, IntOrFloat, LT_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.LT IntOrFloat)
fun Instruction_PROD_7_ACT (LTE, IntOrFloat, LTE_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.LTE IntOrFloat)
fun Instruction_PROD_8_ACT (GT, IntOrFloat, GT_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.GT IntOrFloat)
fun Instruction_PROD_9_ACT (GTE, IntOrFloat, GTE_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.GTE IntOrFloat)
fun Instruction_PROD_10_ACT (EQ, IntOrFloat, EQ_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.EQ IntOrFloat)
fun Instruction_PROD_11_ACT (NEQ, IntOrFloat, NEQ_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.NEQ IntOrFloat)
fun Instruction_PROD_12_ACT (LSHIFT, LSHIFT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.LSHIFT)
fun Instruction_PROD_13_ACT (RSHIFT, RSHIFT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.RSHIFT)
fun Instruction_PROD_14_ACT (NOT, BoolOrInt, NOT_SPAN : (Lex.pos * Lex.pos), BoolOrInt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.NOT BoolOrInt)
fun Instruction_PROD_15_ACT (AND, BoolOrInt, AND_SPAN : (Lex.pos * Lex.pos), BoolOrInt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.AND BoolOrInt)
fun Instruction_PROD_16_ACT (OR, BoolOrInt, OR_SPAN : (Lex.pos * Lex.pos), BoolOrInt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.OR BoolOrInt)
fun Instruction_PROD_17_ACT (XOR, BoolOrInt, XOR_SPAN : (Lex.pos * Lex.pos), BoolOrInt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.XOR BoolOrInt)
fun Instruction_PROD_18_ACT (SELECT, BoolIntOrFloat, SELECT_SPAN : (Lex.pos * Lex.pos), BoolIntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.SELECT BoolIntOrFloat)
fun Instruction_PROD_19_ACT (RAND, RAND_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.RAND)
fun Instruction_PROD_20_ACT (FLOOR, FLOOR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.FLOOR)
fun Instruction_PROD_21_ACT (CEIL, CEIL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.CEIL)
fun Instruction_PROD_22_ACT (TRUNC, TRUNC_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.TRUNC)
fun Instruction_PROD_23_ACT (ROUND, ROUND_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.ROUND)
fun Instruction_PROD_24_ACT (I_TO_F, I_TO_F_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.I_TO_F)
fun Instruction_PROD_25_ACT (I_TO_B, I_TO_B_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.I_TO_B)
fun Instruction_PROD_26_ACT (B_TO_I, B_TO_I_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.B_TO_I)
fun Instruction_PROD_27_ACT (LOG, LOG_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.LOG)
fun Instruction_PROD_28_ACT (SQRT, SQRT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.SQRT)
fun Instruction_PROD_29_ACT (EXP, EXP_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.EXP)
fun Instruction_PROD_30_ACT (SIN, SIN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.SIN)
fun Instruction_PROD_31_ACT (COS, COS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.COS)
fun Instruction_PROD_32_ACT (TAN, TAN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.TAN)
fun Instruction_PROD_33_ACT (ASIN, ASIN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.ASIN)
fun Instruction_PROD_34_ACT (ACOS, ACOS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.ACOS)
fun Instruction_PROD_35_ACT (ATAN, ATAN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.ATAN)
fun Instruction_PROD_36_ACT (SINH, SINH_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.SINH)
fun Instruction_PROD_37_ACT (COSH, COSH_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.COSH)
fun Instruction_PROD_38_ACT (TANH, TANH_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.TANH)
fun Instruction_PROD_39_ACT (IntOrFloat, ADD_SCAN, IntOrFloat_SPAN : (Lex.pos * Lex.pos), ADD_SCAN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.ADD_SCAN IntOrFloat)
fun Instruction_PROD_40_ACT (MUL_SCAN, IntOrFloat, MUL_SCAN_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.MUL_SCAN IntOrFloat)
fun Instruction_PROD_41_ACT (MAX_SCAN, IntOrFloat, MAX_SCAN_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.MAX_SCAN IntOrFloat)
fun Instruction_PROD_42_ACT (IntOrFloat, MIN_SCAN, IntOrFloat_SPAN : (Lex.pos * Lex.pos), MIN_SCAN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.MIN_SCAN IntOrFloat)
fun Instruction_PROD_43_ACT (BoolOrInt, AND_SCAN, BoolOrInt_SPAN : (Lex.pos * Lex.pos), AND_SCAN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.AND_SCAN BoolOrInt)
fun Instruction_PROD_44_ACT (BoolOrInt, OR_SCAN, BoolOrInt_SPAN : (Lex.pos * Lex.pos), OR_SCAN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.OR_SCAN BoolOrInt)
fun Instruction_PROD_45_ACT (BoolOrInt, XOR_SCAN, BoolOrInt_SPAN : (Lex.pos * Lex.pos), XOR_SCAN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.XOR_SCAN BoolOrInt)
fun Instruction_PROD_46_ACT (ADD_REDUCE, IntOrFloat, ADD_REDUCE_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.ADD_REDUCE IntOrFloat)
fun Instruction_PROD_47_ACT (MUL_REDUCE, IntOrFloat, MUL_REDUCE_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.MUL_REDUCE IntOrFloat)
fun Instruction_PROD_48_ACT (MAX_REDUCE, IntOrFloat, MAX_REDUCE_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.MAX_REDUCE IntOrFloat)
fun Instruction_PROD_49_ACT (IntOrFloat, MIN_REDUCE, IntOrFloat_SPAN : (Lex.pos * Lex.pos), MIN_REDUCE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.MIN_REDUCE IntOrFloat)
fun Instruction_PROD_50_ACT (BoolOrInt, AND_REDUCE, BoolOrInt_SPAN : (Lex.pos * Lex.pos), AND_REDUCE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.AND_REDUCE BoolOrInt)
fun Instruction_PROD_51_ACT (BoolOrInt, OR_REDUCE, BoolOrInt_SPAN : (Lex.pos * Lex.pos), OR_REDUCE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.OR_REDUCE BoolOrInt)
fun Instruction_PROD_52_ACT (XOR_REDUCE, BoolOrInt, XOR_REDUCE_SPAN : (Lex.pos * Lex.pos), BoolOrInt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.XOR_REDUCE BoolOrInt)
fun Instruction_PROD_53_ACT (BoolIntOrFloat, PERMUTE, BoolIntOrFloat_SPAN : (Lex.pos * Lex.pos), PERMUTE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.PERMUTE BoolIntOrFloat)
fun Instruction_PROD_54_ACT (BoolIntOrFloat, DPERMUTE, BoolIntOrFloat_SPAN : (Lex.pos * Lex.pos), DPERMUTE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.DPERMUTE BoolIntOrFloat)
fun Instruction_PROD_55_ACT (FPERMUTE, BoolIntOrFloat, FPERMUTE_SPAN : (Lex.pos * Lex.pos), BoolIntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.FPERMUTE BoolIntOrFloat)
fun Instruction_PROD_56_ACT (BoolIntOrFloat, BPERMUTE, BoolIntOrFloat_SPAN : (Lex.pos * Lex.pos), BPERMUTE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.BPERMUTE BoolIntOrFloat)
fun Instruction_PROD_57_ACT (BoolIntOrFloat, BFPERMUTE, BoolIntOrFloat_SPAN : (Lex.pos * Lex.pos), BFPERMUTE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.BFPERMUTE BoolIntOrFloat)
fun Instruction_PROD_58_ACT (BoolIntOrFloat, DFPERMUTE, BoolIntOrFloat_SPAN : (Lex.pos * Lex.pos), DFPERMUTE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.DFPERMUTE BoolIntOrFloat)
fun Instruction_PROD_59_ACT (BoolIntOrFloat, EXTRACT, BoolIntOrFloat_SPAN : (Lex.pos * Lex.pos), EXTRACT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.EXTRACT BoolIntOrFloat)
fun Instruction_PROD_60_ACT (REPLACE, BoolIntOrFloat, REPLACE_SPAN : (Lex.pos * Lex.pos), BoolIntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.REPLACE BoolIntOrFloat)
fun Instruction_PROD_61_ACT (PACK, BoolIntOrFloat, PACK_SPAN : (Lex.pos * Lex.pos), BoolIntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.PACK BoolIntOrFloat)
fun Instruction_PROD_62_ACT (RANK_UP, IntOrFloat, RANK_UP_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.RANK_UP IntOrFloat)
fun Instruction_PROD_63_ACT (IntOrFloat, RANK_DOWN, IntOrFloat_SPAN : (Lex.pos * Lex.pos), RANK_DOWN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.RANK_DOWN IntOrFloat)
fun Instruction_PROD_64_ACT (DIST, BoolIntOrFloat, DIST_SPAN : (Lex.pos * Lex.pos), BoolIntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.DIST BoolIntOrFloat)
fun Instruction_PROD_65_ACT (INDEX, INDEX_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.INDEX)
fun Instruction_PROD_66_ACT (BoolIntOrFloat, LENGTH, BoolIntOrFloat_SPAN : (Lex.pos * Lex.pos), LENGTH_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.LENGTH BoolIntOrFloat)
fun Instruction_PROD_67_ACT (MAKE_SEGDES, MAKE_SEGDES_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.MAKE_SEGDES)
fun Instruction_PROD_68_ACT (LENGTHS, LENGTHS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.LENGTHS)
fun Instruction_PROD_69_ACT (COPY, Offset1, Offset2, COPY_SPAN : (Lex.pos * Lex.pos), Offset1_SPAN : (Lex.pos * Lex.pos), Offset2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.COPY(Offset1, Offset2))
fun Instruction_PROD_70_ACT (POP, Offset1, Offset2, POP_SPAN : (Lex.pos * Lex.pos), Offset1_SPAN : (Lex.pos * Lex.pos), Offset2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.POP(Offset1, Offset2))
fun Instruction_PROD_71_ACT (CPOP, Offset1, Offset2, CPOP_SPAN : (Lex.pos * Lex.pos), Offset1_SPAN : (Lex.pos * Lex.pos), Offset2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.CPOP(Offset1, Offset2))
fun Instruction_PROD_72_ACT (PAIR, PAIR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.PAIR)
fun Instruction_PROD_73_ACT (UNPAIR, UNPAIR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.UNPAIR)
fun Instruction_PROD_74_ACT (CALL, LABEL, CALL_SPAN : (Lex.pos * Lex.pos), LABEL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.CALL LABEL)
fun Instruction_PROD_75_ACT (IF, ELSE, Instruction1, Instruction2, ENDIF, IF_SPAN : (Lex.pos * Lex.pos), ELSE_SPAN : (Lex.pos * Lex.pos), Instruction1_SPAN : (Lex.pos * Lex.pos), Instruction2_SPAN : (Lex.pos * Lex.pos), ENDIF_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.IF(Instruction1, Instruction2))
fun Instruction_PROD_76_ACT (CONST, Constant, CONST_SPAN : (Lex.pos * Lex.pos), Constant_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Constant)
fun Instruction_PROD_77_ACT (READ, Type, READ_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.READ Type)
fun Instruction_PROD_78_ACT (Type, WRITE, Type_SPAN : (Lex.pos * Lex.pos), WRITE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.WRITE Type)
fun Instruction_PROD_79_ACT (FOPEN, FOPEN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.FOPEN)
fun Instruction_PROD_80_ACT (FCLOSE, FCLOSE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.FCLOSE)
fun Instruction_PROD_81_ACT (Type, FWRITE, Type_SPAN : (Lex.pos * Lex.pos), FWRITE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.FWRITE Type)
fun Instruction_PROD_82_ACT (Type, FREAD, Type_SPAN : (Lex.pos * Lex.pos), FREAD_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.FREAD Type)
fun Instruction_PROD_83_ACT (FREAD_CHAR, FREAD_CHAR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.FREAD_CHAR)
fun Instruction_PROD_84_ACT (EXIT, EXIT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.EXIT)
fun Instruction_PROD_85_ACT (START_TIMER, START_TIMER_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.START_TIMER)
fun Instruction_PROD_86_ACT (STOP_TIMER, STOP_TIMER_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.STOP_TIMER)
fun Instruction_PROD_87_ACT (INT, SRAND, INT_SPAN : (Lex.pos * Lex.pos), SRAND_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.SRAND)
fun Offset_PROD_1_ACT (NUM, NUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( IntInf.toInt NUM)
fun Constant_PROD_1_ACT (INT, NumValue, INT_SPAN : (Lex.pos * Lex.pos), NumValue_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.CONST(VC.INT, [NumValue]))
fun Constant_PROD_2_ACT (INT, STREAM, INT_SPAN : (Lex.pos * Lex.pos), STREAM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.CONST(VC.INT, [STREAM]))
fun Constant_PROD_3_ACT (REAL, FLOAT, REAL_SPAN : (Lex.pos * Lex.pos), FLOAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.CONST(VC.FLOAT, [REAL]))
fun Constant_PROD_4_ACT (BOOL, BOOLEAN, BOOL_SPAN : (Lex.pos * Lex.pos), BOOLEAN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.CONST(VC.BOOL, [BOOLEAN]))
fun Constant_PROD_5_ACT (SEGDES, NumValue, SEGDES_SPAN : (Lex.pos * Lex.pos), NumValue_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.CONST(VC.SEGDES, [NumValue]))
fun Constant_PROD_6_ACT (CHAR, STRING, CHAR_SPAN : (Lex.pos * Lex.pos), STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.CONST(VC.CHAR, [STRING]))
fun Constant_PROD_7_ACT (LP, RP, Type, Value, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), Value_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.CONST(Type, Value))
fun NumValue_PROD_1_ACT (NUM, NUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( if (NUM < 0)
                                    then "-" ^ IntInf.toString(~NUM)
                                    else IntInf.toString NUM)
fun Value_PROD_1_ACT (NumValue, NumValue_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( NumValue)
fun Value_PROD_2_ACT (REAL, REAL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( REAL)
fun Value_PROD_3_ACT (BOOLEAN, BOOLEAN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( BOOLEAN)
fun IntOrFloat_PROD_1_ACT (INT, INT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.INT)
fun IntOrFloat_PROD_2_ACT (FLOAT, FLOAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.FLOAT)
fun BoolOrInt_PROD_1_ACT (BOOL, BOOL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.BOOL)
fun BoolOrInt_PROD_2_ACT (INT, INT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.INT)
fun BoolIntOrFloat_PROD_1_ACT (BOOL, BOOL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.BOOL)
fun BoolIntOrFloat_PROD_2_ACT (INT, INT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.INT)
fun BoolIntOrFloat_PROD_3_ACT (FLOAT, FLOAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.FLOAT)
fun Type_PROD_1_ACT (BOOL, BOOL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.BOOL)
fun Type_PROD_2_ACT (INT, INT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.INT)
fun Type_PROD_3_ACT (FLOAT, FLOAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.FLOAT)
fun Type_PROD_4_ACT (CHAR, CHAR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.CHAR)
fun Type_PROD_5_ACT (SEGDES, SEGDES_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( VC.SEGDES)

    end

    structure Err = AntlrErrHandler(
      structure Tok = Tok
      structure Lex = Lex)
    structure EBNF = AntlrEBNF(struct
			         type strm = Err.wstream
			         val getSpan = Err.getSpan
			       end)

    fun mk lexFn = let
fun getS() = {}
fun putS{} = ()
fun unwrap (ret, strm, repairs) = (ret, strm, repairs)
        val (eh, lex) = Err.mkErrHandler {get = getS, put = putS}
	fun fail() = Err.failure eh
	fun tryProds (strm, prods) = let
	  fun try [] = fail()
	    | try (prod :: prods) = 
	        (Err.whileDisabled eh (fn() => prod strm)) 
		handle Err.ParseError => try (prods)
          in try prods end
fun matchEOF strm = (case (lex(strm))
 of (Tok.EOF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSTREAM strm = (case (lex(strm))
 of (Tok.STREAM(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchSTRING strm = (case (lex(strm))
 of (Tok.STRING(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchBOOLEAN strm = (case (lex(strm))
 of (Tok.BOOLEAN(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchREAL strm = (case (lex(strm))
 of (Tok.REAL(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchNUM strm = (case (lex(strm))
 of (Tok.NUM(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchLABEL strm = (case (lex(strm))
 of (Tok.LABEL(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchRP strm = (case (lex(strm))
 of (Tok.RP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLP strm = (case (lex(strm))
 of (Tok.LP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSEGDES strm = (case (lex(strm))
 of (Tok.SEGDES, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCHAR strm = (case (lex(strm))
 of (Tok.CHAR, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchFLOAT strm = (case (lex(strm))
 of (Tok.FLOAT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchINT strm = (case (lex(strm))
 of (Tok.INT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchBOOL strm = (case (lex(strm))
 of (Tok.BOOL, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSRAND strm = (case (lex(strm))
 of (Tok.SRAND, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSTOP_TIMER strm = (case (lex(strm))
 of (Tok.STOP_TIMER, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSTART_TIMER strm = (case (lex(strm))
 of (Tok.START_TIMER, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchEXIT strm = (case (lex(strm))
 of (Tok.EXIT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchFREAD_CHAR strm = (case (lex(strm))
 of (Tok.FREAD_CHAR, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchFREAD strm = (case (lex(strm))
 of (Tok.FREAD, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchFWRITE strm = (case (lex(strm))
 of (Tok.FWRITE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchFCLOSE strm = (case (lex(strm))
 of (Tok.FCLOSE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchFOPEN strm = (case (lex(strm))
 of (Tok.FOPEN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchWRITE strm = (case (lex(strm))
 of (Tok.WRITE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchREAD strm = (case (lex(strm))
 of (Tok.READ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCONST strm = (case (lex(strm))
 of (Tok.CONST, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchENDIF strm = (case (lex(strm))
 of (Tok.ENDIF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchELSE strm = (case (lex(strm))
 of (Tok.ELSE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchIF strm = (case (lex(strm))
 of (Tok.IF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRET strm = (case (lex(strm))
 of (Tok.RET, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCALL strm = (case (lex(strm))
 of (Tok.CALL, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchUNPAIR strm = (case (lex(strm))
 of (Tok.UNPAIR, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchPAIR strm = (case (lex(strm))
 of (Tok.PAIR, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCPOP strm = (case (lex(strm))
 of (Tok.CPOP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchPOP strm = (case (lex(strm))
 of (Tok.POP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCOPY strm = (case (lex(strm))
 of (Tok.COPY, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLENGTHS strm = (case (lex(strm))
 of (Tok.LENGTHS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchMAKE_SEGDES strm = (case (lex(strm))
 of (Tok.MAKE_SEGDES, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLENGTH strm = (case (lex(strm))
 of (Tok.LENGTH, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchINDEX strm = (case (lex(strm))
 of (Tok.INDEX, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDIST strm = (case (lex(strm))
 of (Tok.DIST, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRANK_DOWN strm = (case (lex(strm))
 of (Tok.RANK_DOWN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRANK_UP strm = (case (lex(strm))
 of (Tok.RANK_UP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchPACK strm = (case (lex(strm))
 of (Tok.PACK, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchREPLACE strm = (case (lex(strm))
 of (Tok.REPLACE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchEXTRACT strm = (case (lex(strm))
 of (Tok.EXTRACT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDFPERMUTE strm = (case (lex(strm))
 of (Tok.DFPERMUTE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchBFPERMUTE strm = (case (lex(strm))
 of (Tok.BFPERMUTE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchBPERMUTE strm = (case (lex(strm))
 of (Tok.BPERMUTE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchFPERMUTE strm = (case (lex(strm))
 of (Tok.FPERMUTE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDPERMUTE strm = (case (lex(strm))
 of (Tok.DPERMUTE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchPERMUTE strm = (case (lex(strm))
 of (Tok.PERMUTE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchXOR_REDUCE strm = (case (lex(strm))
 of (Tok.XOR_REDUCE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOR_REDUCE strm = (case (lex(strm))
 of (Tok.OR_REDUCE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchAND_REDUCE strm = (case (lex(strm))
 of (Tok.AND_REDUCE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchMIN_REDUCE strm = (case (lex(strm))
 of (Tok.MIN_REDUCE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchMAX_REDUCE strm = (case (lex(strm))
 of (Tok.MAX_REDUCE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchMUL_REDUCE strm = (case (lex(strm))
 of (Tok.MUL_REDUCE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchADD_REDUCE strm = (case (lex(strm))
 of (Tok.ADD_REDUCE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchXOR_SCAN strm = (case (lex(strm))
 of (Tok.XOR_SCAN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOR_SCAN strm = (case (lex(strm))
 of (Tok.OR_SCAN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchAND_SCAN strm = (case (lex(strm))
 of (Tok.AND_SCAN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchMIN_SCAN strm = (case (lex(strm))
 of (Tok.MIN_SCAN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchMAX_SCAN strm = (case (lex(strm))
 of (Tok.MAX_SCAN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchMUL_SCAN strm = (case (lex(strm))
 of (Tok.MUL_SCAN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchADD_SCAN strm = (case (lex(strm))
 of (Tok.ADD_SCAN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchTANH strm = (case (lex(strm))
 of (Tok.TANH, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCOSH strm = (case (lex(strm))
 of (Tok.COSH, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSINH strm = (case (lex(strm))
 of (Tok.SINH, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchATAN strm = (case (lex(strm))
 of (Tok.ATAN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchACOS strm = (case (lex(strm))
 of (Tok.ACOS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchASIN strm = (case (lex(strm))
 of (Tok.ASIN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchTAN strm = (case (lex(strm))
 of (Tok.TAN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCOS strm = (case (lex(strm))
 of (Tok.COS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSIN strm = (case (lex(strm))
 of (Tok.SIN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchEXP strm = (case (lex(strm))
 of (Tok.EXP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSQRT strm = (case (lex(strm))
 of (Tok.SQRT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLOG strm = (case (lex(strm))
 of (Tok.LOG, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchB_TO_I strm = (case (lex(strm))
 of (Tok.B_TO_I, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchI_TO_B strm = (case (lex(strm))
 of (Tok.I_TO_B, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchI_TO_F strm = (case (lex(strm))
 of (Tok.I_TO_F, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchROUND strm = (case (lex(strm))
 of (Tok.ROUND, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchTRUNC strm = (case (lex(strm))
 of (Tok.TRUNC, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCEIL strm = (case (lex(strm))
 of (Tok.CEIL, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchFLOOR strm = (case (lex(strm))
 of (Tok.FLOOR, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRAND strm = (case (lex(strm))
 of (Tok.RAND, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSELECT strm = (case (lex(strm))
 of (Tok.SELECT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchXOR strm = (case (lex(strm))
 of (Tok.XOR, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOR strm = (case (lex(strm))
 of (Tok.OR, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchAND strm = (case (lex(strm))
 of (Tok.AND, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchNOT strm = (case (lex(strm))
 of (Tok.NOT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRSHIFT strm = (case (lex(strm))
 of (Tok.RSHIFT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLSHIFT strm = (case (lex(strm))
 of (Tok.LSHIFT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchNEQ strm = (case (lex(strm))
 of (Tok.NEQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchEQ strm = (case (lex(strm))
 of (Tok.EQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchGTE strm = (case (lex(strm))
 of (Tok.GTE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchGT strm = (case (lex(strm))
 of (Tok.GT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLTE strm = (case (lex(strm))
 of (Tok.LTE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLT strm = (case (lex(strm))
 of (Tok.LT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchMOD strm = (case (lex(strm))
 of (Tok.MOD, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDIV strm = (case (lex(strm))
 of (Tok.DIV, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchMUL strm = (case (lex(strm))
 of (Tok.MUL, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSUB strm = (case (lex(strm))
 of (Tok.SUB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchADD strm = (case (lex(strm))
 of (Tok.ADD, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchFUNC strm = (case (lex(strm))
 of (Tok.FUNC, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))

val (Program_NT) = 
let
fun Type_NT (strm) = let
      fun Type_PROD_1 (strm) = let
            val (BOOL_RES, BOOL_SPAN, strm') = matchBOOL(strm)
            val FULL_SPAN = (#1(BOOL_SPAN), #2(BOOL_SPAN))
            in
              (UserCode.Type_PROD_1_ACT (BOOL_RES, BOOL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Type_PROD_2 (strm) = let
            val (INT_RES, INT_SPAN, strm') = matchINT(strm)
            val FULL_SPAN = (#1(INT_SPAN), #2(INT_SPAN))
            in
              (UserCode.Type_PROD_2_ACT (INT_RES, INT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Type_PROD_3 (strm) = let
            val (FLOAT_RES, FLOAT_SPAN, strm') = matchFLOAT(strm)
            val FULL_SPAN = (#1(FLOAT_SPAN), #2(FLOAT_SPAN))
            in
              (UserCode.Type_PROD_3_ACT (FLOAT_RES, FLOAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Type_PROD_4 (strm) = let
            val (CHAR_RES, CHAR_SPAN, strm') = matchCHAR(strm)
            val FULL_SPAN = (#1(CHAR_SPAN), #2(CHAR_SPAN))
            in
              (UserCode.Type_PROD_4_ACT (CHAR_RES, CHAR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Type_PROD_5 (strm) = let
            val (SEGDES_RES, SEGDES_SPAN, strm') = matchSEGDES(strm)
            val FULL_SPAN = (#1(SEGDES_SPAN), #2(SEGDES_SPAN))
            in
              (UserCode.Type_PROD_5_ACT (SEGDES_RES, SEGDES_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.SEGDES, _, strm') => Type_PROD_5(strm)
          | (Tok.FLOAT, _, strm') => Type_PROD_3(strm)
          | (Tok.BOOL, _, strm') => Type_PROD_1(strm)
          | (Tok.INT, _, strm') => Type_PROD_2(strm)
          | (Tok.CHAR, _, strm') => Type_PROD_4(strm)
          | _ => fail()
        (* end case *))
      end
fun NumValue_NT (strm) = let
      val (NUM_RES, NUM_SPAN, strm') = matchNUM(strm)
      val FULL_SPAN = (#1(NUM_SPAN), #2(NUM_SPAN))
      in
        (UserCode.NumValue_PROD_1_ACT (NUM_RES, NUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Value_NT (strm) = let
      fun Value_PROD_1 (strm) = let
            val (NumValue_RES, NumValue_SPAN, strm') = NumValue_NT(strm)
            val FULL_SPAN = (#1(NumValue_SPAN), #2(NumValue_SPAN))
            in
              (UserCode.Value_PROD_1_ACT (NumValue_RES, NumValue_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Value_PROD_2 (strm) = let
            val (REAL_RES, REAL_SPAN, strm') = matchREAL(strm)
            val FULL_SPAN = (#1(REAL_SPAN), #2(REAL_SPAN))
            in
              (UserCode.Value_PROD_2_ACT (REAL_RES, REAL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Value_PROD_3 (strm) = let
            val (BOOLEAN_RES, BOOLEAN_SPAN, strm') = matchBOOLEAN(strm)
            val FULL_SPAN = (#1(BOOLEAN_SPAN), #2(BOOLEAN_SPAN))
            in
              (UserCode.Value_PROD_3_ACT (BOOLEAN_RES, BOOLEAN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.BOOLEAN(_), _, strm') => Value_PROD_3(strm)
          | (Tok.NUM(_), _, strm') => Value_PROD_1(strm)
          | (Tok.REAL(_), _, strm') => Value_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
fun Constant_NT (strm) = let
      fun Constant_PROD_1 (strm) = let
            val (INT_RES, INT_SPAN, strm') = matchINT(strm)
            val (NumValue_RES, NumValue_SPAN, strm') = NumValue_NT(strm')
            val FULL_SPAN = (#1(INT_SPAN), #2(NumValue_SPAN))
            in
              (UserCode.Constant_PROD_1_ACT (INT_RES, NumValue_RES, INT_SPAN : (Lex.pos * Lex.pos), NumValue_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Constant_PROD_2 (strm) = let
            val (INT_RES, INT_SPAN, strm') = matchINT(strm)
            val (STREAM_RES, STREAM_SPAN, strm') = matchSTREAM(strm')
            val FULL_SPAN = (#1(INT_SPAN), #2(STREAM_SPAN))
            in
              (UserCode.Constant_PROD_2_ACT (INT_RES, STREAM_RES, INT_SPAN : (Lex.pos * Lex.pos), STREAM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Constant_PROD_3 (strm) = let
            val (FLOAT_RES, FLOAT_SPAN, strm') = matchFLOAT(strm)
            val (REAL_RES, REAL_SPAN, strm') = matchREAL(strm')
            val FULL_SPAN = (#1(FLOAT_SPAN), #2(REAL_SPAN))
            in
              (UserCode.Constant_PROD_3_ACT (REAL_RES, FLOAT_RES, REAL_SPAN : (Lex.pos * Lex.pos), FLOAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Constant_PROD_4 (strm) = let
            val (BOOL_RES, BOOL_SPAN, strm') = matchBOOL(strm)
            val (BOOLEAN_RES, BOOLEAN_SPAN, strm') = matchBOOLEAN(strm')
            val FULL_SPAN = (#1(BOOL_SPAN), #2(BOOLEAN_SPAN))
            in
              (UserCode.Constant_PROD_4_ACT (BOOL_RES, BOOLEAN_RES, BOOL_SPAN : (Lex.pos * Lex.pos), BOOLEAN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Constant_PROD_5 (strm) = let
            val (SEGDES_RES, SEGDES_SPAN, strm') = matchSEGDES(strm)
            val (NumValue_RES, NumValue_SPAN, strm') = NumValue_NT(strm')
            val FULL_SPAN = (#1(SEGDES_SPAN), #2(NumValue_SPAN))
            in
              (UserCode.Constant_PROD_5_ACT (SEGDES_RES, NumValue_RES, SEGDES_SPAN : (Lex.pos * Lex.pos), NumValue_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Constant_PROD_6 (strm) = let
            val (CHAR_RES, CHAR_SPAN, strm') = matchCHAR(strm)
            val (STRING_RES, STRING_SPAN, strm') = matchSTRING(strm')
            val FULL_SPAN = (#1(CHAR_SPAN), #2(STRING_SPAN))
            in
              (UserCode.Constant_PROD_6_ACT (CHAR_RES, STRING_RES, CHAR_SPAN : (Lex.pos * Lex.pos), STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Constant_PROD_7 (strm) = let
            val (Type_RES, Type_SPAN, strm') = Type_NT(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            fun Constant_PROD_7_SUBRULE_1_NT (strm) = let
                  val (Value_RES, Value_SPAN, strm') = Value_NT(strm)
                  val FULL_SPAN = (#1(Value_SPAN), #2(Value_SPAN))
                  in
                    ((Value_RES), FULL_SPAN, strm')
                  end
            fun Constant_PROD_7_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.NUM(_), _, strm') => true
                    | (Tok.REAL(_), _, strm') => true
                    | (Tok.BOOLEAN(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (Value_RES, Value_SPAN, strm') = EBNF.closure(Constant_PROD_7_SUBRULE_1_PRED, Constant_PROD_7_SUBRULE_1_NT, strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(Type_SPAN), #2(RP_SPAN))
            in
              (UserCode.Constant_PROD_7_ACT (LP_RES, RP_RES, Type_RES, Value_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), Value_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.SEGDES, _, strm') =>
              (case (lex(strm'))
               of (Tok.NUM(_), _, strm') => Constant_PROD_5(strm)
                | (Tok.LP, _, strm') => Constant_PROD_7(strm)
                | _ => fail()
              (* end case *))
          | (Tok.FLOAT, _, strm') =>
              (case (lex(strm'))
               of (Tok.REAL(_), _, strm') => Constant_PROD_3(strm)
                | (Tok.LP, _, strm') => Constant_PROD_7(strm)
                | _ => fail()
              (* end case *))
          | (Tok.INT, _, strm') =>
              (case (lex(strm'))
               of (Tok.NUM(_), _, strm') => Constant_PROD_1(strm)
                | (Tok.LP, _, strm') => Constant_PROD_7(strm)
                | (Tok.STREAM(_), _, strm') => Constant_PROD_2(strm)
                | _ => fail()
              (* end case *))
          | (Tok.BOOL, _, strm') =>
              (case (lex(strm'))
               of (Tok.BOOLEAN(_), _, strm') => Constant_PROD_4(strm)
                | (Tok.LP, _, strm') => Constant_PROD_7(strm)
                | _ => fail()
              (* end case *))
          | (Tok.CHAR, _, strm') =>
              (case (lex(strm'))
               of (Tok.STRING(_), _, strm') => Constant_PROD_6(strm)
                | (Tok.LP, _, strm') => Constant_PROD_7(strm)
                | _ => fail()
              (* end case *))
          | _ => fail()
        (* end case *))
      end
fun Offset_NT (strm) = let
      val (NUM_RES, NUM_SPAN, strm') = matchNUM(strm)
      val FULL_SPAN = (#1(NUM_SPAN), #2(NUM_SPAN))
      in
        (UserCode.Offset_PROD_1_ACT (NUM_RES, NUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun BoolIntOrFloat_NT (strm) = let
      fun BoolIntOrFloat_PROD_1 (strm) = let
            val (BOOL_RES, BOOL_SPAN, strm') = matchBOOL(strm)
            val FULL_SPAN = (#1(BOOL_SPAN), #2(BOOL_SPAN))
            in
              (UserCode.BoolIntOrFloat_PROD_1_ACT (BOOL_RES, BOOL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun BoolIntOrFloat_PROD_2 (strm) = let
            val (INT_RES, INT_SPAN, strm') = matchINT(strm)
            val FULL_SPAN = (#1(INT_SPAN), #2(INT_SPAN))
            in
              (UserCode.BoolIntOrFloat_PROD_2_ACT (INT_RES, INT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun BoolIntOrFloat_PROD_3 (strm) = let
            val (FLOAT_RES, FLOAT_SPAN, strm') = matchFLOAT(strm)
            val FULL_SPAN = (#1(FLOAT_SPAN), #2(FLOAT_SPAN))
            in
              (UserCode.BoolIntOrFloat_PROD_3_ACT (FLOAT_RES, FLOAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.FLOAT, _, strm') => BoolIntOrFloat_PROD_3(strm)
          | (Tok.BOOL, _, strm') => BoolIntOrFloat_PROD_1(strm)
          | (Tok.INT, _, strm') => BoolIntOrFloat_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
fun IntOrFloat_NT (strm) = let
      fun IntOrFloat_PROD_1 (strm) = let
            val (INT_RES, INT_SPAN, strm') = matchINT(strm)
            val FULL_SPAN = (#1(INT_SPAN), #2(INT_SPAN))
            in
              (UserCode.IntOrFloat_PROD_1_ACT (INT_RES, INT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun IntOrFloat_PROD_2 (strm) = let
            val (FLOAT_RES, FLOAT_SPAN, strm') = matchFLOAT(strm)
            val FULL_SPAN = (#1(FLOAT_SPAN), #2(FLOAT_SPAN))
            in
              (UserCode.IntOrFloat_PROD_2_ACT (FLOAT_RES, FLOAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.FLOAT, _, strm') => IntOrFloat_PROD_2(strm)
          | (Tok.INT, _, strm') => IntOrFloat_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun BoolOrInt_NT (strm) = let
      fun BoolOrInt_PROD_1 (strm) = let
            val (BOOL_RES, BOOL_SPAN, strm') = matchBOOL(strm)
            val FULL_SPAN = (#1(BOOL_SPAN), #2(BOOL_SPAN))
            in
              (UserCode.BoolOrInt_PROD_1_ACT (BOOL_RES, BOOL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun BoolOrInt_PROD_2 (strm) = let
            val (INT_RES, INT_SPAN, strm') = matchINT(strm)
            val FULL_SPAN = (#1(INT_SPAN), #2(INT_SPAN))
            in
              (UserCode.BoolOrInt_PROD_2_ACT (INT_RES, INT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.INT, _, strm') => BoolOrInt_PROD_2(strm)
          | (Tok.BOOL, _, strm') => BoolOrInt_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun Instruction_NT (strm) = let
      fun Instruction_PROD_1 (strm) = let
            val (ADD_RES, ADD_SPAN, strm') = matchADD(strm)
            val (IntOrFloat_RES, IntOrFloat_SPAN, strm') = IntOrFloat_NT(strm')
            val FULL_SPAN = (#1(ADD_SPAN), #2(IntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_1_ACT (ADD_RES, IntOrFloat_RES, ADD_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_2 (strm) = let
            val (SUB_RES, SUB_SPAN, strm') = matchSUB(strm)
            val (IntOrFloat_RES, IntOrFloat_SPAN, strm') = IntOrFloat_NT(strm')
            val FULL_SPAN = (#1(SUB_SPAN), #2(IntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_2_ACT (SUB_RES, IntOrFloat_RES, SUB_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_3 (strm) = let
            val (MUL_RES, MUL_SPAN, strm') = matchMUL(strm)
            val (IntOrFloat_RES, IntOrFloat_SPAN, strm') = IntOrFloat_NT(strm')
            val FULL_SPAN = (#1(MUL_SPAN), #2(IntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_3_ACT (MUL_RES, IntOrFloat_RES, MUL_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_4 (strm) = let
            val (DIV_RES, DIV_SPAN, strm') = matchDIV(strm)
            val (IntOrFloat_RES, IntOrFloat_SPAN, strm') = IntOrFloat_NT(strm')
            val FULL_SPAN = (#1(DIV_SPAN), #2(IntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_4_ACT (DIV_RES, IntOrFloat_RES, DIV_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_5 (strm) = let
            val (MOD_RES, MOD_SPAN, strm') = matchMOD(strm)
            val FULL_SPAN = (#1(MOD_SPAN), #2(MOD_SPAN))
            in
              (UserCode.Instruction_PROD_5_ACT (MOD_RES, MOD_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_6 (strm) = let
            val (LT_RES, LT_SPAN, strm') = matchLT(strm)
            val (IntOrFloat_RES, IntOrFloat_SPAN, strm') = IntOrFloat_NT(strm')
            val FULL_SPAN = (#1(LT_SPAN), #2(IntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_6_ACT (LT_RES, IntOrFloat_RES, LT_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_7 (strm) = let
            val (LTE_RES, LTE_SPAN, strm') = matchLTE(strm)
            val (IntOrFloat_RES, IntOrFloat_SPAN, strm') = IntOrFloat_NT(strm')
            val FULL_SPAN = (#1(LTE_SPAN), #2(IntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_7_ACT (LTE_RES, IntOrFloat_RES, LTE_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_8 (strm) = let
            val (GT_RES, GT_SPAN, strm') = matchGT(strm)
            val (IntOrFloat_RES, IntOrFloat_SPAN, strm') = IntOrFloat_NT(strm')
            val FULL_SPAN = (#1(GT_SPAN), #2(IntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_8_ACT (GT_RES, IntOrFloat_RES, GT_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_9 (strm) = let
            val (GTE_RES, GTE_SPAN, strm') = matchGTE(strm)
            val (IntOrFloat_RES, IntOrFloat_SPAN, strm') = IntOrFloat_NT(strm')
            val FULL_SPAN = (#1(GTE_SPAN), #2(IntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_9_ACT (GTE_RES, IntOrFloat_RES, GTE_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_10 (strm) = let
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm)
            val (IntOrFloat_RES, IntOrFloat_SPAN, strm') = IntOrFloat_NT(strm')
            val FULL_SPAN = (#1(EQ_SPAN), #2(IntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_10_ACT (EQ_RES, IntOrFloat_RES, EQ_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_11 (strm) = let
            val (NEQ_RES, NEQ_SPAN, strm') = matchNEQ(strm)
            val (IntOrFloat_RES, IntOrFloat_SPAN, strm') = IntOrFloat_NT(strm')
            val FULL_SPAN = (#1(NEQ_SPAN), #2(IntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_11_ACT (NEQ_RES, IntOrFloat_RES, NEQ_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_12 (strm) = let
            val (LSHIFT_RES, LSHIFT_SPAN, strm') = matchLSHIFT(strm)
            val FULL_SPAN = (#1(LSHIFT_SPAN), #2(LSHIFT_SPAN))
            in
              (UserCode.Instruction_PROD_12_ACT (LSHIFT_RES, LSHIFT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_13 (strm) = let
            val (RSHIFT_RES, RSHIFT_SPAN, strm') = matchRSHIFT(strm)
            val FULL_SPAN = (#1(RSHIFT_SPAN), #2(RSHIFT_SPAN))
            in
              (UserCode.Instruction_PROD_13_ACT (RSHIFT_RES, RSHIFT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_14 (strm) = let
            val (NOT_RES, NOT_SPAN, strm') = matchNOT(strm)
            val (BoolOrInt_RES, BoolOrInt_SPAN, strm') = BoolOrInt_NT(strm')
            val FULL_SPAN = (#1(NOT_SPAN), #2(BoolOrInt_SPAN))
            in
              (UserCode.Instruction_PROD_14_ACT (NOT_RES, BoolOrInt_RES, NOT_SPAN : (Lex.pos * Lex.pos), BoolOrInt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_15 (strm) = let
            val (AND_RES, AND_SPAN, strm') = matchAND(strm)
            val (BoolOrInt_RES, BoolOrInt_SPAN, strm') = BoolOrInt_NT(strm')
            val FULL_SPAN = (#1(AND_SPAN), #2(BoolOrInt_SPAN))
            in
              (UserCode.Instruction_PROD_15_ACT (AND_RES, BoolOrInt_RES, AND_SPAN : (Lex.pos * Lex.pos), BoolOrInt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_16 (strm) = let
            val (OR_RES, OR_SPAN, strm') = matchOR(strm)
            val (BoolOrInt_RES, BoolOrInt_SPAN, strm') = BoolOrInt_NT(strm')
            val FULL_SPAN = (#1(OR_SPAN), #2(BoolOrInt_SPAN))
            in
              (UserCode.Instruction_PROD_16_ACT (OR_RES, BoolOrInt_RES, OR_SPAN : (Lex.pos * Lex.pos), BoolOrInt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_17 (strm) = let
            val (XOR_RES, XOR_SPAN, strm') = matchXOR(strm)
            val (BoolOrInt_RES, BoolOrInt_SPAN, strm') = BoolOrInt_NT(strm')
            val FULL_SPAN = (#1(XOR_SPAN), #2(BoolOrInt_SPAN))
            in
              (UserCode.Instruction_PROD_17_ACT (XOR_RES, BoolOrInt_RES, XOR_SPAN : (Lex.pos * Lex.pos), BoolOrInt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_18 (strm) = let
            val (SELECT_RES, SELECT_SPAN, strm') = matchSELECT(strm)
            val (BoolIntOrFloat_RES, BoolIntOrFloat_SPAN, strm') = BoolIntOrFloat_NT(strm')
            val FULL_SPAN = (#1(SELECT_SPAN), #2(BoolIntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_18_ACT (SELECT_RES, BoolIntOrFloat_RES, SELECT_SPAN : (Lex.pos * Lex.pos), BoolIntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_19 (strm) = let
            val (RAND_RES, RAND_SPAN, strm') = matchRAND(strm)
            val FULL_SPAN = (#1(RAND_SPAN), #2(RAND_SPAN))
            in
              (UserCode.Instruction_PROD_19_ACT (RAND_RES, RAND_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_20 (strm) = let
            val (FLOOR_RES, FLOOR_SPAN, strm') = matchFLOOR(strm)
            val FULL_SPAN = (#1(FLOOR_SPAN), #2(FLOOR_SPAN))
            in
              (UserCode.Instruction_PROD_20_ACT (FLOOR_RES, FLOOR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_21 (strm) = let
            val (CEIL_RES, CEIL_SPAN, strm') = matchCEIL(strm)
            val FULL_SPAN = (#1(CEIL_SPAN), #2(CEIL_SPAN))
            in
              (UserCode.Instruction_PROD_21_ACT (CEIL_RES, CEIL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_22 (strm) = let
            val (TRUNC_RES, TRUNC_SPAN, strm') = matchTRUNC(strm)
            val FULL_SPAN = (#1(TRUNC_SPAN), #2(TRUNC_SPAN))
            in
              (UserCode.Instruction_PROD_22_ACT (TRUNC_RES, TRUNC_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_23 (strm) = let
            val (ROUND_RES, ROUND_SPAN, strm') = matchROUND(strm)
            val FULL_SPAN = (#1(ROUND_SPAN), #2(ROUND_SPAN))
            in
              (UserCode.Instruction_PROD_23_ACT (ROUND_RES, ROUND_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_24 (strm) = let
            val (I_TO_F_RES, I_TO_F_SPAN, strm') = matchI_TO_F(strm)
            val FULL_SPAN = (#1(I_TO_F_SPAN), #2(I_TO_F_SPAN))
            in
              (UserCode.Instruction_PROD_24_ACT (I_TO_F_RES, I_TO_F_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_25 (strm) = let
            val (I_TO_B_RES, I_TO_B_SPAN, strm') = matchI_TO_B(strm)
            val FULL_SPAN = (#1(I_TO_B_SPAN), #2(I_TO_B_SPAN))
            in
              (UserCode.Instruction_PROD_25_ACT (I_TO_B_RES, I_TO_B_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_26 (strm) = let
            val (B_TO_I_RES, B_TO_I_SPAN, strm') = matchB_TO_I(strm)
            val FULL_SPAN = (#1(B_TO_I_SPAN), #2(B_TO_I_SPAN))
            in
              (UserCode.Instruction_PROD_26_ACT (B_TO_I_RES, B_TO_I_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_27 (strm) = let
            val (LOG_RES, LOG_SPAN, strm') = matchLOG(strm)
            val FULL_SPAN = (#1(LOG_SPAN), #2(LOG_SPAN))
            in
              (UserCode.Instruction_PROD_27_ACT (LOG_RES, LOG_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_28 (strm) = let
            val (SQRT_RES, SQRT_SPAN, strm') = matchSQRT(strm)
            val FULL_SPAN = (#1(SQRT_SPAN), #2(SQRT_SPAN))
            in
              (UserCode.Instruction_PROD_28_ACT (SQRT_RES, SQRT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_29 (strm) = let
            val (EXP_RES, EXP_SPAN, strm') = matchEXP(strm)
            val FULL_SPAN = (#1(EXP_SPAN), #2(EXP_SPAN))
            in
              (UserCode.Instruction_PROD_29_ACT (EXP_RES, EXP_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_30 (strm) = let
            val (SIN_RES, SIN_SPAN, strm') = matchSIN(strm)
            val FULL_SPAN = (#1(SIN_SPAN), #2(SIN_SPAN))
            in
              (UserCode.Instruction_PROD_30_ACT (SIN_RES, SIN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_31 (strm) = let
            val (COS_RES, COS_SPAN, strm') = matchCOS(strm)
            val FULL_SPAN = (#1(COS_SPAN), #2(COS_SPAN))
            in
              (UserCode.Instruction_PROD_31_ACT (COS_RES, COS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_32 (strm) = let
            val (TAN_RES, TAN_SPAN, strm') = matchTAN(strm)
            val FULL_SPAN = (#1(TAN_SPAN), #2(TAN_SPAN))
            in
              (UserCode.Instruction_PROD_32_ACT (TAN_RES, TAN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_33 (strm) = let
            val (ASIN_RES, ASIN_SPAN, strm') = matchASIN(strm)
            val FULL_SPAN = (#1(ASIN_SPAN), #2(ASIN_SPAN))
            in
              (UserCode.Instruction_PROD_33_ACT (ASIN_RES, ASIN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_34 (strm) = let
            val (ACOS_RES, ACOS_SPAN, strm') = matchACOS(strm)
            val FULL_SPAN = (#1(ACOS_SPAN), #2(ACOS_SPAN))
            in
              (UserCode.Instruction_PROD_34_ACT (ACOS_RES, ACOS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_35 (strm) = let
            val (ATAN_RES, ATAN_SPAN, strm') = matchATAN(strm)
            val FULL_SPAN = (#1(ATAN_SPAN), #2(ATAN_SPAN))
            in
              (UserCode.Instruction_PROD_35_ACT (ATAN_RES, ATAN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_36 (strm) = let
            val (SINH_RES, SINH_SPAN, strm') = matchSINH(strm)
            val FULL_SPAN = (#1(SINH_SPAN), #2(SINH_SPAN))
            in
              (UserCode.Instruction_PROD_36_ACT (SINH_RES, SINH_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_37 (strm) = let
            val (COSH_RES, COSH_SPAN, strm') = matchCOSH(strm)
            val FULL_SPAN = (#1(COSH_SPAN), #2(COSH_SPAN))
            in
              (UserCode.Instruction_PROD_37_ACT (COSH_RES, COSH_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_38 (strm) = let
            val (TANH_RES, TANH_SPAN, strm') = matchTANH(strm)
            val FULL_SPAN = (#1(TANH_SPAN), #2(TANH_SPAN))
            in
              (UserCode.Instruction_PROD_38_ACT (TANH_RES, TANH_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_39 (strm) = let
            val (ADD_SCAN_RES, ADD_SCAN_SPAN, strm') = matchADD_SCAN(strm)
            val (IntOrFloat_RES, IntOrFloat_SPAN, strm') = IntOrFloat_NT(strm')
            val FULL_SPAN = (#1(ADD_SCAN_SPAN), #2(IntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_39_ACT (IntOrFloat_RES, ADD_SCAN_RES, IntOrFloat_SPAN : (Lex.pos * Lex.pos), ADD_SCAN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_40 (strm) = let
            val (MUL_SCAN_RES, MUL_SCAN_SPAN, strm') = matchMUL_SCAN(strm)
            val (IntOrFloat_RES, IntOrFloat_SPAN, strm') = IntOrFloat_NT(strm')
            val FULL_SPAN = (#1(MUL_SCAN_SPAN), #2(IntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_40_ACT (MUL_SCAN_RES, IntOrFloat_RES, MUL_SCAN_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_41 (strm) = let
            val (MAX_SCAN_RES, MAX_SCAN_SPAN, strm') = matchMAX_SCAN(strm)
            val (IntOrFloat_RES, IntOrFloat_SPAN, strm') = IntOrFloat_NT(strm')
            val FULL_SPAN = (#1(MAX_SCAN_SPAN), #2(IntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_41_ACT (MAX_SCAN_RES, IntOrFloat_RES, MAX_SCAN_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_42 (strm) = let
            val (MIN_SCAN_RES, MIN_SCAN_SPAN, strm') = matchMIN_SCAN(strm)
            val (IntOrFloat_RES, IntOrFloat_SPAN, strm') = IntOrFloat_NT(strm')
            val FULL_SPAN = (#1(MIN_SCAN_SPAN), #2(IntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_42_ACT (IntOrFloat_RES, MIN_SCAN_RES, IntOrFloat_SPAN : (Lex.pos * Lex.pos), MIN_SCAN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_43 (strm) = let
            val (AND_SCAN_RES, AND_SCAN_SPAN, strm') = matchAND_SCAN(strm)
            val (BoolOrInt_RES, BoolOrInt_SPAN, strm') = BoolOrInt_NT(strm')
            val FULL_SPAN = (#1(AND_SCAN_SPAN), #2(BoolOrInt_SPAN))
            in
              (UserCode.Instruction_PROD_43_ACT (BoolOrInt_RES, AND_SCAN_RES, BoolOrInt_SPAN : (Lex.pos * Lex.pos), AND_SCAN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_44 (strm) = let
            val (OR_SCAN_RES, OR_SCAN_SPAN, strm') = matchOR_SCAN(strm)
            val (BoolOrInt_RES, BoolOrInt_SPAN, strm') = BoolOrInt_NT(strm')
            val FULL_SPAN = (#1(OR_SCAN_SPAN), #2(BoolOrInt_SPAN))
            in
              (UserCode.Instruction_PROD_44_ACT (BoolOrInt_RES, OR_SCAN_RES, BoolOrInt_SPAN : (Lex.pos * Lex.pos), OR_SCAN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_45 (strm) = let
            val (XOR_SCAN_RES, XOR_SCAN_SPAN, strm') = matchXOR_SCAN(strm)
            val (BoolOrInt_RES, BoolOrInt_SPAN, strm') = BoolOrInt_NT(strm')
            val FULL_SPAN = (#1(XOR_SCAN_SPAN), #2(BoolOrInt_SPAN))
            in
              (UserCode.Instruction_PROD_45_ACT (BoolOrInt_RES, XOR_SCAN_RES, BoolOrInt_SPAN : (Lex.pos * Lex.pos), XOR_SCAN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_46 (strm) = let
            val (ADD_REDUCE_RES, ADD_REDUCE_SPAN, strm') = matchADD_REDUCE(strm)
            val (IntOrFloat_RES, IntOrFloat_SPAN, strm') = IntOrFloat_NT(strm')
            val FULL_SPAN = (#1(ADD_REDUCE_SPAN), #2(IntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_46_ACT (ADD_REDUCE_RES, IntOrFloat_RES, ADD_REDUCE_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_47 (strm) = let
            val (MUL_REDUCE_RES, MUL_REDUCE_SPAN, strm') = matchMUL_REDUCE(strm)
            val (IntOrFloat_RES, IntOrFloat_SPAN, strm') = IntOrFloat_NT(strm')
            val FULL_SPAN = (#1(MUL_REDUCE_SPAN), #2(IntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_47_ACT (MUL_REDUCE_RES, IntOrFloat_RES, MUL_REDUCE_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_48 (strm) = let
            val (MAX_REDUCE_RES, MAX_REDUCE_SPAN, strm') = matchMAX_REDUCE(strm)
            val (IntOrFloat_RES, IntOrFloat_SPAN, strm') = IntOrFloat_NT(strm')
            val FULL_SPAN = (#1(MAX_REDUCE_SPAN), #2(IntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_48_ACT (MAX_REDUCE_RES, IntOrFloat_RES, MAX_REDUCE_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_49 (strm) = let
            val (MIN_REDUCE_RES, MIN_REDUCE_SPAN, strm') = matchMIN_REDUCE(strm)
            val (IntOrFloat_RES, IntOrFloat_SPAN, strm') = IntOrFloat_NT(strm')
            val FULL_SPAN = (#1(MIN_REDUCE_SPAN), #2(IntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_49_ACT (IntOrFloat_RES, MIN_REDUCE_RES, IntOrFloat_SPAN : (Lex.pos * Lex.pos), MIN_REDUCE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_50 (strm) = let
            val (AND_REDUCE_RES, AND_REDUCE_SPAN, strm') = matchAND_REDUCE(strm)
            val (BoolOrInt_RES, BoolOrInt_SPAN, strm') = BoolOrInt_NT(strm')
            val FULL_SPAN = (#1(AND_REDUCE_SPAN), #2(BoolOrInt_SPAN))
            in
              (UserCode.Instruction_PROD_50_ACT (BoolOrInt_RES, AND_REDUCE_RES, BoolOrInt_SPAN : (Lex.pos * Lex.pos), AND_REDUCE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_51 (strm) = let
            val (OR_REDUCE_RES, OR_REDUCE_SPAN, strm') = matchOR_REDUCE(strm)
            val (BoolOrInt_RES, BoolOrInt_SPAN, strm') = BoolOrInt_NT(strm')
            val FULL_SPAN = (#1(OR_REDUCE_SPAN), #2(BoolOrInt_SPAN))
            in
              (UserCode.Instruction_PROD_51_ACT (BoolOrInt_RES, OR_REDUCE_RES, BoolOrInt_SPAN : (Lex.pos * Lex.pos), OR_REDUCE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_52 (strm) = let
            val (XOR_REDUCE_RES, XOR_REDUCE_SPAN, strm') = matchXOR_REDUCE(strm)
            val (BoolOrInt_RES, BoolOrInt_SPAN, strm') = BoolOrInt_NT(strm')
            val FULL_SPAN = (#1(XOR_REDUCE_SPAN), #2(BoolOrInt_SPAN))
            in
              (UserCode.Instruction_PROD_52_ACT (XOR_REDUCE_RES, BoolOrInt_RES, XOR_REDUCE_SPAN : (Lex.pos * Lex.pos), BoolOrInt_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_53 (strm) = let
            val (PERMUTE_RES, PERMUTE_SPAN, strm') = matchPERMUTE(strm)
            val (BoolIntOrFloat_RES, BoolIntOrFloat_SPAN, strm') = BoolIntOrFloat_NT(strm')
            val FULL_SPAN = (#1(PERMUTE_SPAN), #2(BoolIntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_53_ACT (BoolIntOrFloat_RES, PERMUTE_RES, BoolIntOrFloat_SPAN : (Lex.pos * Lex.pos), PERMUTE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_54 (strm) = let
            val (DPERMUTE_RES, DPERMUTE_SPAN, strm') = matchDPERMUTE(strm)
            val (BoolIntOrFloat_RES, BoolIntOrFloat_SPAN, strm') = BoolIntOrFloat_NT(strm')
            val FULL_SPAN = (#1(DPERMUTE_SPAN), #2(BoolIntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_54_ACT (BoolIntOrFloat_RES, DPERMUTE_RES, BoolIntOrFloat_SPAN : (Lex.pos * Lex.pos), DPERMUTE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_55 (strm) = let
            val (FPERMUTE_RES, FPERMUTE_SPAN, strm') = matchFPERMUTE(strm)
            val (BoolIntOrFloat_RES, BoolIntOrFloat_SPAN, strm') = BoolIntOrFloat_NT(strm')
            val FULL_SPAN = (#1(FPERMUTE_SPAN), #2(BoolIntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_55_ACT (FPERMUTE_RES, BoolIntOrFloat_RES, FPERMUTE_SPAN : (Lex.pos * Lex.pos), BoolIntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_56 (strm) = let
            val (BPERMUTE_RES, BPERMUTE_SPAN, strm') = matchBPERMUTE(strm)
            val (BoolIntOrFloat_RES, BoolIntOrFloat_SPAN, strm') = BoolIntOrFloat_NT(strm')
            val FULL_SPAN = (#1(BPERMUTE_SPAN), #2(BoolIntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_56_ACT (BoolIntOrFloat_RES, BPERMUTE_RES, BoolIntOrFloat_SPAN : (Lex.pos * Lex.pos), BPERMUTE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_57 (strm) = let
            val (BFPERMUTE_RES, BFPERMUTE_SPAN, strm') = matchBFPERMUTE(strm)
            val (BoolIntOrFloat_RES, BoolIntOrFloat_SPAN, strm') = BoolIntOrFloat_NT(strm')
            val FULL_SPAN = (#1(BFPERMUTE_SPAN), #2(BoolIntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_57_ACT (BoolIntOrFloat_RES, BFPERMUTE_RES, BoolIntOrFloat_SPAN : (Lex.pos * Lex.pos), BFPERMUTE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_58 (strm) = let
            val (DFPERMUTE_RES, DFPERMUTE_SPAN, strm') = matchDFPERMUTE(strm)
            val (BoolIntOrFloat_RES, BoolIntOrFloat_SPAN, strm') = BoolIntOrFloat_NT(strm')
            val FULL_SPAN = (#1(DFPERMUTE_SPAN), #2(BoolIntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_58_ACT (BoolIntOrFloat_RES, DFPERMUTE_RES, BoolIntOrFloat_SPAN : (Lex.pos * Lex.pos), DFPERMUTE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_59 (strm) = let
            val (EXTRACT_RES, EXTRACT_SPAN, strm') = matchEXTRACT(strm)
            val (BoolIntOrFloat_RES, BoolIntOrFloat_SPAN, strm') = BoolIntOrFloat_NT(strm')
            val FULL_SPAN = (#1(EXTRACT_SPAN), #2(BoolIntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_59_ACT (BoolIntOrFloat_RES, EXTRACT_RES, BoolIntOrFloat_SPAN : (Lex.pos * Lex.pos), EXTRACT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_60 (strm) = let
            val (REPLACE_RES, REPLACE_SPAN, strm') = matchREPLACE(strm)
            val (BoolIntOrFloat_RES, BoolIntOrFloat_SPAN, strm') = BoolIntOrFloat_NT(strm')
            val FULL_SPAN = (#1(REPLACE_SPAN), #2(BoolIntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_60_ACT (REPLACE_RES, BoolIntOrFloat_RES, REPLACE_SPAN : (Lex.pos * Lex.pos), BoolIntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_61 (strm) = let
            val (PACK_RES, PACK_SPAN, strm') = matchPACK(strm)
            val (BoolIntOrFloat_RES, BoolIntOrFloat_SPAN, strm') = BoolIntOrFloat_NT(strm')
            val FULL_SPAN = (#1(PACK_SPAN), #2(BoolIntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_61_ACT (PACK_RES, BoolIntOrFloat_RES, PACK_SPAN : (Lex.pos * Lex.pos), BoolIntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_62 (strm) = let
            val (RANK_UP_RES, RANK_UP_SPAN, strm') = matchRANK_UP(strm)
            val (IntOrFloat_RES, IntOrFloat_SPAN, strm') = IntOrFloat_NT(strm')
            val FULL_SPAN = (#1(RANK_UP_SPAN), #2(IntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_62_ACT (RANK_UP_RES, IntOrFloat_RES, RANK_UP_SPAN : (Lex.pos * Lex.pos), IntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_63 (strm) = let
            val (RANK_DOWN_RES, RANK_DOWN_SPAN, strm') = matchRANK_DOWN(strm)
            val (IntOrFloat_RES, IntOrFloat_SPAN, strm') = IntOrFloat_NT(strm')
            val FULL_SPAN = (#1(RANK_DOWN_SPAN), #2(IntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_63_ACT (IntOrFloat_RES, RANK_DOWN_RES, IntOrFloat_SPAN : (Lex.pos * Lex.pos), RANK_DOWN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_64 (strm) = let
            val (DIST_RES, DIST_SPAN, strm') = matchDIST(strm)
            val (BoolIntOrFloat_RES, BoolIntOrFloat_SPAN, strm') = BoolIntOrFloat_NT(strm')
            val FULL_SPAN = (#1(DIST_SPAN), #2(BoolIntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_64_ACT (DIST_RES, BoolIntOrFloat_RES, DIST_SPAN : (Lex.pos * Lex.pos), BoolIntOrFloat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_65 (strm) = let
            val (INDEX_RES, INDEX_SPAN, strm') = matchINDEX(strm)
            val FULL_SPAN = (#1(INDEX_SPAN), #2(INDEX_SPAN))
            in
              (UserCode.Instruction_PROD_65_ACT (INDEX_RES, INDEX_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_66 (strm) = let
            val (LENGTH_RES, LENGTH_SPAN, strm') = matchLENGTH(strm)
            val (BoolIntOrFloat_RES, BoolIntOrFloat_SPAN, strm') = BoolIntOrFloat_NT(strm')
            val FULL_SPAN = (#1(LENGTH_SPAN), #2(BoolIntOrFloat_SPAN))
            in
              (UserCode.Instruction_PROD_66_ACT (BoolIntOrFloat_RES, LENGTH_RES, BoolIntOrFloat_SPAN : (Lex.pos * Lex.pos), LENGTH_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_67 (strm) = let
            val (MAKE_SEGDES_RES, MAKE_SEGDES_SPAN, strm') = matchMAKE_SEGDES(strm)
            val FULL_SPAN = (#1(MAKE_SEGDES_SPAN), #2(MAKE_SEGDES_SPAN))
            in
              (UserCode.Instruction_PROD_67_ACT (MAKE_SEGDES_RES, MAKE_SEGDES_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_68 (strm) = let
            val (LENGTHS_RES, LENGTHS_SPAN, strm') = matchLENGTHS(strm)
            val FULL_SPAN = (#1(LENGTHS_SPAN), #2(LENGTHS_SPAN))
            in
              (UserCode.Instruction_PROD_68_ACT (LENGTHS_RES, LENGTHS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_69 (strm) = let
            val (COPY_RES, COPY_SPAN, strm') = matchCOPY(strm)
            val (Offset1_RES, Offset1_SPAN, strm') = Offset_NT(strm')
            val (Offset2_RES, Offset2_SPAN, strm') = Offset_NT(strm')
            val FULL_SPAN = (#1(COPY_SPAN), #2(Offset2_SPAN))
            in
              (UserCode.Instruction_PROD_69_ACT (COPY_RES, Offset1_RES, Offset2_RES, COPY_SPAN : (Lex.pos * Lex.pos), Offset1_SPAN : (Lex.pos * Lex.pos), Offset2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_70 (strm) = let
            val (POP_RES, POP_SPAN, strm') = matchPOP(strm)
            val (Offset1_RES, Offset1_SPAN, strm') = Offset_NT(strm')
            val (Offset2_RES, Offset2_SPAN, strm') = Offset_NT(strm')
            val FULL_SPAN = (#1(POP_SPAN), #2(Offset2_SPAN))
            in
              (UserCode.Instruction_PROD_70_ACT (POP_RES, Offset1_RES, Offset2_RES, POP_SPAN : (Lex.pos * Lex.pos), Offset1_SPAN : (Lex.pos * Lex.pos), Offset2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_71 (strm) = let
            val (CPOP_RES, CPOP_SPAN, strm') = matchCPOP(strm)
            val (Offset1_RES, Offset1_SPAN, strm') = Offset_NT(strm')
            val (Offset2_RES, Offset2_SPAN, strm') = Offset_NT(strm')
            val FULL_SPAN = (#1(CPOP_SPAN), #2(Offset2_SPAN))
            in
              (UserCode.Instruction_PROD_71_ACT (CPOP_RES, Offset1_RES, Offset2_RES, CPOP_SPAN : (Lex.pos * Lex.pos), Offset1_SPAN : (Lex.pos * Lex.pos), Offset2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_72 (strm) = let
            val (PAIR_RES, PAIR_SPAN, strm') = matchPAIR(strm)
            val FULL_SPAN = (#1(PAIR_SPAN), #2(PAIR_SPAN))
            in
              (UserCode.Instruction_PROD_72_ACT (PAIR_RES, PAIR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_73 (strm) = let
            val (UNPAIR_RES, UNPAIR_SPAN, strm') = matchUNPAIR(strm)
            val FULL_SPAN = (#1(UNPAIR_SPAN), #2(UNPAIR_SPAN))
            in
              (UserCode.Instruction_PROD_73_ACT (UNPAIR_RES, UNPAIR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_74 (strm) = let
            val (CALL_RES, CALL_SPAN, strm') = matchCALL(strm)
            val (LABEL_RES, LABEL_SPAN, strm') = matchLABEL(strm')
            val FULL_SPAN = (#1(CALL_SPAN), #2(LABEL_SPAN))
            in
              (UserCode.Instruction_PROD_74_ACT (CALL_RES, LABEL_RES, CALL_SPAN : (Lex.pos * Lex.pos), LABEL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_75 (strm) = let
            val (IF_RES, IF_SPAN, strm') = matchIF(strm)
            fun Instruction_PROD_75_SUBRULE_1_NT (strm) = let
                  val (Instruction_RES, Instruction_SPAN, strm') = Instruction_NT(strm)
                  val FULL_SPAN = (#1(Instruction_SPAN), #2(Instruction_SPAN))
                  in
                    ((Instruction_RES), FULL_SPAN, strm')
                  end
            fun Instruction_PROD_75_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.ADD, _, strm') => true
                    | (Tok.SUB, _, strm') => true
                    | (Tok.MUL, _, strm') => true
                    | (Tok.DIV, _, strm') => true
                    | (Tok.MOD, _, strm') => true
                    | (Tok.LT, _, strm') => true
                    | (Tok.LTE, _, strm') => true
                    | (Tok.GT, _, strm') => true
                    | (Tok.GTE, _, strm') => true
                    | (Tok.EQ, _, strm') => true
                    | (Tok.NEQ, _, strm') => true
                    | (Tok.LSHIFT, _, strm') => true
                    | (Tok.RSHIFT, _, strm') => true
                    | (Tok.NOT, _, strm') => true
                    | (Tok.AND, _, strm') => true
                    | (Tok.OR, _, strm') => true
                    | (Tok.XOR, _, strm') => true
                    | (Tok.SELECT, _, strm') => true
                    | (Tok.RAND, _, strm') => true
                    | (Tok.FLOOR, _, strm') => true
                    | (Tok.CEIL, _, strm') => true
                    | (Tok.TRUNC, _, strm') => true
                    | (Tok.ROUND, _, strm') => true
                    | (Tok.I_TO_F, _, strm') => true
                    | (Tok.I_TO_B, _, strm') => true
                    | (Tok.B_TO_I, _, strm') => true
                    | (Tok.LOG, _, strm') => true
                    | (Tok.SQRT, _, strm') => true
                    | (Tok.EXP, _, strm') => true
                    | (Tok.SIN, _, strm') => true
                    | (Tok.COS, _, strm') => true
                    | (Tok.TAN, _, strm') => true
                    | (Tok.ASIN, _, strm') => true
                    | (Tok.ACOS, _, strm') => true
                    | (Tok.ATAN, _, strm') => true
                    | (Tok.SINH, _, strm') => true
                    | (Tok.COSH, _, strm') => true
                    | (Tok.TANH, _, strm') => true
                    | (Tok.ADD_SCAN, _, strm') => true
                    | (Tok.MUL_SCAN, _, strm') => true
                    | (Tok.MAX_SCAN, _, strm') => true
                    | (Tok.MIN_SCAN, _, strm') => true
                    | (Tok.AND_SCAN, _, strm') => true
                    | (Tok.OR_SCAN, _, strm') => true
                    | (Tok.XOR_SCAN, _, strm') => true
                    | (Tok.ADD_REDUCE, _, strm') => true
                    | (Tok.MUL_REDUCE, _, strm') => true
                    | (Tok.MAX_REDUCE, _, strm') => true
                    | (Tok.MIN_REDUCE, _, strm') => true
                    | (Tok.AND_REDUCE, _, strm') => true
                    | (Tok.OR_REDUCE, _, strm') => true
                    | (Tok.XOR_REDUCE, _, strm') => true
                    | (Tok.PERMUTE, _, strm') => true
                    | (Tok.DPERMUTE, _, strm') => true
                    | (Tok.FPERMUTE, _, strm') => true
                    | (Tok.BPERMUTE, _, strm') => true
                    | (Tok.BFPERMUTE, _, strm') => true
                    | (Tok.DFPERMUTE, _, strm') => true
                    | (Tok.EXTRACT, _, strm') => true
                    | (Tok.REPLACE, _, strm') => true
                    | (Tok.PACK, _, strm') => true
                    | (Tok.RANK_UP, _, strm') => true
                    | (Tok.RANK_DOWN, _, strm') => true
                    | (Tok.DIST, _, strm') => true
                    | (Tok.INDEX, _, strm') => true
                    | (Tok.LENGTH, _, strm') => true
                    | (Tok.MAKE_SEGDES, _, strm') => true
                    | (Tok.LENGTHS, _, strm') => true
                    | (Tok.COPY, _, strm') => true
                    | (Tok.POP, _, strm') => true
                    | (Tok.CPOP, _, strm') => true
                    | (Tok.PAIR, _, strm') => true
                    | (Tok.UNPAIR, _, strm') => true
                    | (Tok.CALL, _, strm') => true
                    | (Tok.IF, _, strm') => true
                    | (Tok.CONST, _, strm') => true
                    | (Tok.READ, _, strm') => true
                    | (Tok.WRITE, _, strm') => true
                    | (Tok.FOPEN, _, strm') => true
                    | (Tok.FCLOSE, _, strm') => true
                    | (Tok.FWRITE, _, strm') => true
                    | (Tok.FREAD, _, strm') => true
                    | (Tok.FREAD_CHAR, _, strm') => true
                    | (Tok.EXIT, _, strm') => true
                    | (Tok.START_TIMER, _, strm') => true
                    | (Tok.STOP_TIMER, _, strm') => true
                    | (Tok.SRAND, _, strm') => true
                    | _ => false
                  (* end case *))
            val (Instruction1_RES, Instruction1_SPAN, strm') = EBNF.closure(Instruction_PROD_75_SUBRULE_1_PRED, Instruction_PROD_75_SUBRULE_1_NT, strm')
            val (ELSE_RES, ELSE_SPAN, strm') = matchELSE(strm')
            fun Instruction_PROD_75_SUBRULE_2_NT (strm) = let
                  val (Instruction_RES, Instruction_SPAN, strm') = Instruction_NT(strm)
                  val FULL_SPAN = (#1(Instruction_SPAN), #2(Instruction_SPAN))
                  in
                    ((Instruction_RES), FULL_SPAN, strm')
                  end
            fun Instruction_PROD_75_SUBRULE_2_PRED (strm) = (case (lex(strm))
                   of (Tok.ADD, _, strm') => true
                    | (Tok.SUB, _, strm') => true
                    | (Tok.MUL, _, strm') => true
                    | (Tok.DIV, _, strm') => true
                    | (Tok.MOD, _, strm') => true
                    | (Tok.LT, _, strm') => true
                    | (Tok.LTE, _, strm') => true
                    | (Tok.GT, _, strm') => true
                    | (Tok.GTE, _, strm') => true
                    | (Tok.EQ, _, strm') => true
                    | (Tok.NEQ, _, strm') => true
                    | (Tok.LSHIFT, _, strm') => true
                    | (Tok.RSHIFT, _, strm') => true
                    | (Tok.NOT, _, strm') => true
                    | (Tok.AND, _, strm') => true
                    | (Tok.OR, _, strm') => true
                    | (Tok.XOR, _, strm') => true
                    | (Tok.SELECT, _, strm') => true
                    | (Tok.RAND, _, strm') => true
                    | (Tok.FLOOR, _, strm') => true
                    | (Tok.CEIL, _, strm') => true
                    | (Tok.TRUNC, _, strm') => true
                    | (Tok.ROUND, _, strm') => true
                    | (Tok.I_TO_F, _, strm') => true
                    | (Tok.I_TO_B, _, strm') => true
                    | (Tok.B_TO_I, _, strm') => true
                    | (Tok.LOG, _, strm') => true
                    | (Tok.SQRT, _, strm') => true
                    | (Tok.EXP, _, strm') => true
                    | (Tok.SIN, _, strm') => true
                    | (Tok.COS, _, strm') => true
                    | (Tok.TAN, _, strm') => true
                    | (Tok.ASIN, _, strm') => true
                    | (Tok.ACOS, _, strm') => true
                    | (Tok.ATAN, _, strm') => true
                    | (Tok.SINH, _, strm') => true
                    | (Tok.COSH, _, strm') => true
                    | (Tok.TANH, _, strm') => true
                    | (Tok.ADD_SCAN, _, strm') => true
                    | (Tok.MUL_SCAN, _, strm') => true
                    | (Tok.MAX_SCAN, _, strm') => true
                    | (Tok.MIN_SCAN, _, strm') => true
                    | (Tok.AND_SCAN, _, strm') => true
                    | (Tok.OR_SCAN, _, strm') => true
                    | (Tok.XOR_SCAN, _, strm') => true
                    | (Tok.ADD_REDUCE, _, strm') => true
                    | (Tok.MUL_REDUCE, _, strm') => true
                    | (Tok.MAX_REDUCE, _, strm') => true
                    | (Tok.MIN_REDUCE, _, strm') => true
                    | (Tok.AND_REDUCE, _, strm') => true
                    | (Tok.OR_REDUCE, _, strm') => true
                    | (Tok.XOR_REDUCE, _, strm') => true
                    | (Tok.PERMUTE, _, strm') => true
                    | (Tok.DPERMUTE, _, strm') => true
                    | (Tok.FPERMUTE, _, strm') => true
                    | (Tok.BPERMUTE, _, strm') => true
                    | (Tok.BFPERMUTE, _, strm') => true
                    | (Tok.DFPERMUTE, _, strm') => true
                    | (Tok.EXTRACT, _, strm') => true
                    | (Tok.REPLACE, _, strm') => true
                    | (Tok.PACK, _, strm') => true
                    | (Tok.RANK_UP, _, strm') => true
                    | (Tok.RANK_DOWN, _, strm') => true
                    | (Tok.DIST, _, strm') => true
                    | (Tok.INDEX, _, strm') => true
                    | (Tok.LENGTH, _, strm') => true
                    | (Tok.MAKE_SEGDES, _, strm') => true
                    | (Tok.LENGTHS, _, strm') => true
                    | (Tok.COPY, _, strm') => true
                    | (Tok.POP, _, strm') => true
                    | (Tok.CPOP, _, strm') => true
                    | (Tok.PAIR, _, strm') => true
                    | (Tok.UNPAIR, _, strm') => true
                    | (Tok.CALL, _, strm') => true
                    | (Tok.IF, _, strm') => true
                    | (Tok.CONST, _, strm') => true
                    | (Tok.READ, _, strm') => true
                    | (Tok.WRITE, _, strm') => true
                    | (Tok.FOPEN, _, strm') => true
                    | (Tok.FCLOSE, _, strm') => true
                    | (Tok.FWRITE, _, strm') => true
                    | (Tok.FREAD, _, strm') => true
                    | (Tok.FREAD_CHAR, _, strm') => true
                    | (Tok.EXIT, _, strm') => true
                    | (Tok.START_TIMER, _, strm') => true
                    | (Tok.STOP_TIMER, _, strm') => true
                    | (Tok.SRAND, _, strm') => true
                    | _ => false
                  (* end case *))
            val (Instruction2_RES, Instruction2_SPAN, strm') = EBNF.closure(Instruction_PROD_75_SUBRULE_2_PRED, Instruction_PROD_75_SUBRULE_2_NT, strm')
            val (ENDIF_RES, ENDIF_SPAN, strm') = matchENDIF(strm')
            val FULL_SPAN = (#1(IF_SPAN), #2(ENDIF_SPAN))
            in
              (UserCode.Instruction_PROD_75_ACT (IF_RES, ELSE_RES, Instruction1_RES, Instruction2_RES, ENDIF_RES, IF_SPAN : (Lex.pos * Lex.pos), ELSE_SPAN : (Lex.pos * Lex.pos), Instruction1_SPAN : (Lex.pos * Lex.pos), Instruction2_SPAN : (Lex.pos * Lex.pos), ENDIF_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_76 (strm) = let
            val (CONST_RES, CONST_SPAN, strm') = matchCONST(strm)
            val (Constant_RES, Constant_SPAN, strm') = Constant_NT(strm')
            val FULL_SPAN = (#1(CONST_SPAN), #2(Constant_SPAN))
            in
              (UserCode.Instruction_PROD_76_ACT (CONST_RES, Constant_RES, CONST_SPAN : (Lex.pos * Lex.pos), Constant_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_77 (strm) = let
            val (READ_RES, READ_SPAN, strm') = matchREAD(strm)
            val (Type_RES, Type_SPAN, strm') = Type_NT(strm')
            val FULL_SPAN = (#1(READ_SPAN), #2(Type_SPAN))
            in
              (UserCode.Instruction_PROD_77_ACT (READ_RES, Type_RES, READ_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_78 (strm) = let
            val (WRITE_RES, WRITE_SPAN, strm') = matchWRITE(strm)
            val (Type_RES, Type_SPAN, strm') = Type_NT(strm')
            val FULL_SPAN = (#1(WRITE_SPAN), #2(Type_SPAN))
            in
              (UserCode.Instruction_PROD_78_ACT (Type_RES, WRITE_RES, Type_SPAN : (Lex.pos * Lex.pos), WRITE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_79 (strm) = let
            val (FOPEN_RES, FOPEN_SPAN, strm') = matchFOPEN(strm)
            val FULL_SPAN = (#1(FOPEN_SPAN), #2(FOPEN_SPAN))
            in
              (UserCode.Instruction_PROD_79_ACT (FOPEN_RES, FOPEN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_80 (strm) = let
            val (FCLOSE_RES, FCLOSE_SPAN, strm') = matchFCLOSE(strm)
            val FULL_SPAN = (#1(FCLOSE_SPAN), #2(FCLOSE_SPAN))
            in
              (UserCode.Instruction_PROD_80_ACT (FCLOSE_RES, FCLOSE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_81 (strm) = let
            val (FWRITE_RES, FWRITE_SPAN, strm') = matchFWRITE(strm)
            val (Type_RES, Type_SPAN, strm') = Type_NT(strm')
            val FULL_SPAN = (#1(FWRITE_SPAN), #2(Type_SPAN))
            in
              (UserCode.Instruction_PROD_81_ACT (Type_RES, FWRITE_RES, Type_SPAN : (Lex.pos * Lex.pos), FWRITE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_82 (strm) = let
            val (FREAD_RES, FREAD_SPAN, strm') = matchFREAD(strm)
            val (Type_RES, Type_SPAN, strm') = Type_NT(strm')
            val FULL_SPAN = (#1(FREAD_SPAN), #2(Type_SPAN))
            in
              (UserCode.Instruction_PROD_82_ACT (Type_RES, FREAD_RES, Type_SPAN : (Lex.pos * Lex.pos), FREAD_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_83 (strm) = let
            val (FREAD_CHAR_RES, FREAD_CHAR_SPAN, strm') = matchFREAD_CHAR(strm)
            val FULL_SPAN = (#1(FREAD_CHAR_SPAN), #2(FREAD_CHAR_SPAN))
            in
              (UserCode.Instruction_PROD_83_ACT (FREAD_CHAR_RES, FREAD_CHAR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_84 (strm) = let
            val (EXIT_RES, EXIT_SPAN, strm') = matchEXIT(strm)
            val FULL_SPAN = (#1(EXIT_SPAN), #2(EXIT_SPAN))
            in
              (UserCode.Instruction_PROD_84_ACT (EXIT_RES, EXIT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_85 (strm) = let
            val (START_TIMER_RES, START_TIMER_SPAN, strm') = matchSTART_TIMER(strm)
            val FULL_SPAN = (#1(START_TIMER_SPAN), #2(START_TIMER_SPAN))
            in
              (UserCode.Instruction_PROD_85_ACT (START_TIMER_RES, START_TIMER_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_86 (strm) = let
            val (STOP_TIMER_RES, STOP_TIMER_SPAN, strm') = matchSTOP_TIMER(strm)
            val FULL_SPAN = (#1(STOP_TIMER_SPAN), #2(STOP_TIMER_SPAN))
            in
              (UserCode.Instruction_PROD_86_ACT (STOP_TIMER_RES, STOP_TIMER_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Instruction_PROD_87 (strm) = let
            val (SRAND_RES, SRAND_SPAN, strm') = matchSRAND(strm)
            val (INT_RES, INT_SPAN, strm') = matchINT(strm')
            val FULL_SPAN = (#1(SRAND_SPAN), #2(INT_SPAN))
            in
              (UserCode.Instruction_PROD_87_ACT (INT_RES, SRAND_RES, INT_SPAN : (Lex.pos * Lex.pos), SRAND_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.SRAND, _, strm') => Instruction_PROD_87(strm)
          | (Tok.START_TIMER, _, strm') => Instruction_PROD_85(strm)
          | (Tok.FREAD_CHAR, _, strm') => Instruction_PROD_83(strm)
          | (Tok.FWRITE, _, strm') => Instruction_PROD_81(strm)
          | (Tok.FOPEN, _, strm') => Instruction_PROD_79(strm)
          | (Tok.READ, _, strm') => Instruction_PROD_77(strm)
          | (Tok.IF, _, strm') => Instruction_PROD_75(strm)
          | (Tok.UNPAIR, _, strm') => Instruction_PROD_73(strm)
          | (Tok.CPOP, _, strm') => Instruction_PROD_71(strm)
          | (Tok.COPY, _, strm') => Instruction_PROD_69(strm)
          | (Tok.MAKE_SEGDES, _, strm') => Instruction_PROD_67(strm)
          | (Tok.INDEX, _, strm') => Instruction_PROD_65(strm)
          | (Tok.RANK_DOWN, _, strm') => Instruction_PROD_63(strm)
          | (Tok.PACK, _, strm') => Instruction_PROD_61(strm)
          | (Tok.EXTRACT, _, strm') => Instruction_PROD_59(strm)
          | (Tok.BFPERMUTE, _, strm') => Instruction_PROD_57(strm)
          | (Tok.FPERMUTE, _, strm') => Instruction_PROD_55(strm)
          | (Tok.PERMUTE, _, strm') => Instruction_PROD_53(strm)
          | (Tok.OR_REDUCE, _, strm') => Instruction_PROD_51(strm)
          | (Tok.MIN_REDUCE, _, strm') => Instruction_PROD_49(strm)
          | (Tok.MUL_REDUCE, _, strm') => Instruction_PROD_47(strm)
          | (Tok.XOR_SCAN, _, strm') => Instruction_PROD_45(strm)
          | (Tok.AND_SCAN, _, strm') => Instruction_PROD_43(strm)
          | (Tok.MAX_SCAN, _, strm') => Instruction_PROD_41(strm)
          | (Tok.ADD_SCAN, _, strm') => Instruction_PROD_39(strm)
          | (Tok.COSH, _, strm') => Instruction_PROD_37(strm)
          | (Tok.ATAN, _, strm') => Instruction_PROD_35(strm)
          | (Tok.ASIN, _, strm') => Instruction_PROD_33(strm)
          | (Tok.COS, _, strm') => Instruction_PROD_31(strm)
          | (Tok.EXP, _, strm') => Instruction_PROD_29(strm)
          | (Tok.LOG, _, strm') => Instruction_PROD_27(strm)
          | (Tok.I_TO_B, _, strm') => Instruction_PROD_25(strm)
          | (Tok.ROUND, _, strm') => Instruction_PROD_23(strm)
          | (Tok.CEIL, _, strm') => Instruction_PROD_21(strm)
          | (Tok.RAND, _, strm') => Instruction_PROD_19(strm)
          | (Tok.XOR, _, strm') => Instruction_PROD_17(strm)
          | (Tok.AND, _, strm') => Instruction_PROD_15(strm)
          | (Tok.RSHIFT, _, strm') => Instruction_PROD_13(strm)
          | (Tok.NEQ, _, strm') => Instruction_PROD_11(strm)
          | (Tok.GTE, _, strm') => Instruction_PROD_9(strm)
          | (Tok.LTE, _, strm') => Instruction_PROD_7(strm)
          | (Tok.MOD, _, strm') => Instruction_PROD_5(strm)
          | (Tok.MUL, _, strm') => Instruction_PROD_3(strm)
          | (Tok.ADD, _, strm') => Instruction_PROD_1(strm)
          | (Tok.SUB, _, strm') => Instruction_PROD_2(strm)
          | (Tok.DIV, _, strm') => Instruction_PROD_4(strm)
          | (Tok.LT, _, strm') => Instruction_PROD_6(strm)
          | (Tok.GT, _, strm') => Instruction_PROD_8(strm)
          | (Tok.EQ, _, strm') => Instruction_PROD_10(strm)
          | (Tok.LSHIFT, _, strm') => Instruction_PROD_12(strm)
          | (Tok.NOT, _, strm') => Instruction_PROD_14(strm)
          | (Tok.OR, _, strm') => Instruction_PROD_16(strm)
          | (Tok.SELECT, _, strm') => Instruction_PROD_18(strm)
          | (Tok.FLOOR, _, strm') => Instruction_PROD_20(strm)
          | (Tok.TRUNC, _, strm') => Instruction_PROD_22(strm)
          | (Tok.I_TO_F, _, strm') => Instruction_PROD_24(strm)
          | (Tok.B_TO_I, _, strm') => Instruction_PROD_26(strm)
          | (Tok.SQRT, _, strm') => Instruction_PROD_28(strm)
          | (Tok.SIN, _, strm') => Instruction_PROD_30(strm)
          | (Tok.TAN, _, strm') => Instruction_PROD_32(strm)
          | (Tok.ACOS, _, strm') => Instruction_PROD_34(strm)
          | (Tok.SINH, _, strm') => Instruction_PROD_36(strm)
          | (Tok.TANH, _, strm') => Instruction_PROD_38(strm)
          | (Tok.MUL_SCAN, _, strm') => Instruction_PROD_40(strm)
          | (Tok.MIN_SCAN, _, strm') => Instruction_PROD_42(strm)
          | (Tok.OR_SCAN, _, strm') => Instruction_PROD_44(strm)
          | (Tok.ADD_REDUCE, _, strm') => Instruction_PROD_46(strm)
          | (Tok.MAX_REDUCE, _, strm') => Instruction_PROD_48(strm)
          | (Tok.AND_REDUCE, _, strm') => Instruction_PROD_50(strm)
          | (Tok.XOR_REDUCE, _, strm') => Instruction_PROD_52(strm)
          | (Tok.DPERMUTE, _, strm') => Instruction_PROD_54(strm)
          | (Tok.BPERMUTE, _, strm') => Instruction_PROD_56(strm)
          | (Tok.DFPERMUTE, _, strm') => Instruction_PROD_58(strm)
          | (Tok.REPLACE, _, strm') => Instruction_PROD_60(strm)
          | (Tok.RANK_UP, _, strm') => Instruction_PROD_62(strm)
          | (Tok.DIST, _, strm') => Instruction_PROD_64(strm)
          | (Tok.LENGTH, _, strm') => Instruction_PROD_66(strm)
          | (Tok.LENGTHS, _, strm') => Instruction_PROD_68(strm)
          | (Tok.POP, _, strm') => Instruction_PROD_70(strm)
          | (Tok.PAIR, _, strm') => Instruction_PROD_72(strm)
          | (Tok.CALL, _, strm') => Instruction_PROD_74(strm)
          | (Tok.CONST, _, strm') => Instruction_PROD_76(strm)
          | (Tok.WRITE, _, strm') => Instruction_PROD_78(strm)
          | (Tok.FCLOSE, _, strm') => Instruction_PROD_80(strm)
          | (Tok.FREAD, _, strm') => Instruction_PROD_82(strm)
          | (Tok.EXIT, _, strm') => Instruction_PROD_84(strm)
          | (Tok.STOP_TIMER, _, strm') => Instruction_PROD_86(strm)
          | _ => fail()
        (* end case *))
      end
fun Function_NT (strm) = let
      val (FUNC_RES, FUNC_SPAN, strm') = matchFUNC(strm)
      val (LABEL_RES, LABEL_SPAN, strm') = matchLABEL(strm')
      fun Function_PROD_1_SUBRULE_1_NT (strm) = let
            val (Instruction_RES, Instruction_SPAN, strm') = Instruction_NT(strm)
            val FULL_SPAN = (#1(Instruction_SPAN), #2(Instruction_SPAN))
            in
              ((Instruction_RES), FULL_SPAN, strm')
            end
      fun Function_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.ADD, _, strm') => true
              | (Tok.SUB, _, strm') => true
              | (Tok.MUL, _, strm') => true
              | (Tok.DIV, _, strm') => true
              | (Tok.MOD, _, strm') => true
              | (Tok.LT, _, strm') => true
              | (Tok.LTE, _, strm') => true
              | (Tok.GT, _, strm') => true
              | (Tok.GTE, _, strm') => true
              | (Tok.EQ, _, strm') => true
              | (Tok.NEQ, _, strm') => true
              | (Tok.LSHIFT, _, strm') => true
              | (Tok.RSHIFT, _, strm') => true
              | (Tok.NOT, _, strm') => true
              | (Tok.AND, _, strm') => true
              | (Tok.OR, _, strm') => true
              | (Tok.XOR, _, strm') => true
              | (Tok.SELECT, _, strm') => true
              | (Tok.RAND, _, strm') => true
              | (Tok.FLOOR, _, strm') => true
              | (Tok.CEIL, _, strm') => true
              | (Tok.TRUNC, _, strm') => true
              | (Tok.ROUND, _, strm') => true
              | (Tok.I_TO_F, _, strm') => true
              | (Tok.I_TO_B, _, strm') => true
              | (Tok.B_TO_I, _, strm') => true
              | (Tok.LOG, _, strm') => true
              | (Tok.SQRT, _, strm') => true
              | (Tok.EXP, _, strm') => true
              | (Tok.SIN, _, strm') => true
              | (Tok.COS, _, strm') => true
              | (Tok.TAN, _, strm') => true
              | (Tok.ASIN, _, strm') => true
              | (Tok.ACOS, _, strm') => true
              | (Tok.ATAN, _, strm') => true
              | (Tok.SINH, _, strm') => true
              | (Tok.COSH, _, strm') => true
              | (Tok.TANH, _, strm') => true
              | (Tok.ADD_SCAN, _, strm') => true
              | (Tok.MUL_SCAN, _, strm') => true
              | (Tok.MAX_SCAN, _, strm') => true
              | (Tok.MIN_SCAN, _, strm') => true
              | (Tok.AND_SCAN, _, strm') => true
              | (Tok.OR_SCAN, _, strm') => true
              | (Tok.XOR_SCAN, _, strm') => true
              | (Tok.ADD_REDUCE, _, strm') => true
              | (Tok.MUL_REDUCE, _, strm') => true
              | (Tok.MAX_REDUCE, _, strm') => true
              | (Tok.MIN_REDUCE, _, strm') => true
              | (Tok.AND_REDUCE, _, strm') => true
              | (Tok.OR_REDUCE, _, strm') => true
              | (Tok.XOR_REDUCE, _, strm') => true
              | (Tok.PERMUTE, _, strm') => true
              | (Tok.DPERMUTE, _, strm') => true
              | (Tok.FPERMUTE, _, strm') => true
              | (Tok.BPERMUTE, _, strm') => true
              | (Tok.BFPERMUTE, _, strm') => true
              | (Tok.DFPERMUTE, _, strm') => true
              | (Tok.EXTRACT, _, strm') => true
              | (Tok.REPLACE, _, strm') => true
              | (Tok.PACK, _, strm') => true
              | (Tok.RANK_UP, _, strm') => true
              | (Tok.RANK_DOWN, _, strm') => true
              | (Tok.DIST, _, strm') => true
              | (Tok.INDEX, _, strm') => true
              | (Tok.LENGTH, _, strm') => true
              | (Tok.MAKE_SEGDES, _, strm') => true
              | (Tok.LENGTHS, _, strm') => true
              | (Tok.COPY, _, strm') => true
              | (Tok.POP, _, strm') => true
              | (Tok.CPOP, _, strm') => true
              | (Tok.PAIR, _, strm') => true
              | (Tok.UNPAIR, _, strm') => true
              | (Tok.CALL, _, strm') => true
              | (Tok.IF, _, strm') => true
              | (Tok.CONST, _, strm') => true
              | (Tok.READ, _, strm') => true
              | (Tok.WRITE, _, strm') => true
              | (Tok.FOPEN, _, strm') => true
              | (Tok.FCLOSE, _, strm') => true
              | (Tok.FWRITE, _, strm') => true
              | (Tok.FREAD, _, strm') => true
              | (Tok.FREAD_CHAR, _, strm') => true
              | (Tok.EXIT, _, strm') => true
              | (Tok.START_TIMER, _, strm') => true
              | (Tok.STOP_TIMER, _, strm') => true
              | (Tok.SRAND, _, strm') => true
              | _ => false
            (* end case *))
      val (Instruction_RES, Instruction_SPAN, strm') = EBNF.closure(Function_PROD_1_SUBRULE_1_PRED, Function_PROD_1_SUBRULE_1_NT, strm')
      val (RET_RES, RET_SPAN, strm') = matchRET(strm')
      val FULL_SPAN = (#1(FUNC_SPAN), #2(RET_SPAN))
      in
        (UserCode.Function_PROD_1_ACT (RET_RES, FUNC_RES, LABEL_RES, Instruction_RES, RET_SPAN : (Lex.pos * Lex.pos), FUNC_SPAN : (Lex.pos * Lex.pos), LABEL_SPAN : (Lex.pos * Lex.pos), Instruction_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Program_NT (strm) = let
      fun Program_PROD_1_SUBRULE_1_NT (strm) = let
            val (Function_RES, Function_SPAN, strm') = Function_NT(strm)
            val FULL_SPAN = (#1(Function_SPAN), #2(Function_SPAN))
            in
              ((Function_RES), FULL_SPAN, strm')
            end
      fun Program_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.FUNC, _, strm') => true
              | _ => false
            (* end case *))
      val (Function_RES, Function_SPAN, strm') = EBNF.closure(Program_PROD_1_SUBRULE_1_PRED, Program_PROD_1_SUBRULE_1_NT, strm)
      val FULL_SPAN = (#1(Function_SPAN), #2(Function_SPAN))
      in
        ((Function_RES), FULL_SPAN, strm')
      end
in
  (Program_NT)
end
val Program_NT =  fn s => unwrap (Err.launch (eh, lexFn, Program_NT , true) s)

in (Program_NT) end
  in
fun parse lexFn  s = let val (Program_NT) = mk lexFn in Program_NT s end

  end

end
