structure 
MinMLTokens = struct

    datatype token = EOF
      | STRING of string
      | NUMBER of IntInf.int
      | NAME of Atom.atom
      | TYVAR of Atom.atom
      | WILD
      | DARROW
      | ARROW
      | BAR
      | SEMI
      | COMMA
      | HASH
      | TILDE
      | NEQ
      | DEQ
      | EQ
      | TIMES
      | MINUS
      | DPLUS
      | PLUS
      | AT
      | DCOLON
      | COLON
      | GT
      | GTEQ
      | LT
      | LTEQ
      | BANG
      | RPB
      | LPB
      | RCB
      | LCB
      | RB
      | LB
      | RP
      | LP
      | KW_val
      | KW_type
      | KW_then
      | KW_orelse
      | KW_of
      | KW_mod
      | KW_let
      | KW_in
      | KW_if
      | KW_fun
      | KW_end
      | KW_else
      | KW_div
      | KW_datatype
      | KW_case
      | KW_andalso
      | KW_and

    val allToks = [EOF, WILD, DARROW, ARROW, BAR, SEMI, COMMA, HASH, TILDE, NEQ, DEQ, EQ, TIMES, MINUS, DPLUS, PLUS, AT, DCOLON, COLON, GT, GTEQ, LT, LTEQ, BANG, RPB, LPB, RCB, LCB, RB, LB, RP, LP, KW_val, KW_type, KW_then, KW_orelse, KW_of, KW_mod, KW_let, KW_in, KW_if, KW_fun, KW_end, KW_else, KW_div, KW_datatype, KW_case, KW_andalso, KW_and]

    fun toString tok =
(case (tok)
 of (EOF) => "EOF"
  | (STRING(_)) => "STRING"
  | (NUMBER(_)) => "NUMBER"
  | (NAME(_)) => "NAME"
  | (TYVAR(_)) => "TYVAR"
  | (WILD) => "_"
  | (DARROW) => "=>"
  | (ARROW) => "->"
  | (BAR) => "|"
  | (SEMI) => ";"
  | (COMMA) => ","
  | (HASH) => "#"
  | (TILDE) => "~"
  | (NEQ) => "!="
  | (DEQ) => "=="
  | (EQ) => "="
  | (TIMES) => "*"
  | (MINUS) => "-"
  | (DPLUS) => "++"
  | (PLUS) => "+"
  | (AT) => "@"
  | (DCOLON) => "::"
  | (COLON) => ":"
  | (GT) => ">"
  | (GTEQ) => ">="
  | (LT) => "<"
  | (LTEQ) => "<="
  | (BANG) => "!"
  | (RPB) => "|]"
  | (LPB) => "[|"
  | (RCB) => "}"
  | (LCB) => "{"
  | (RB) => "]"
  | (LB) => "["
  | (RP) => ")"
  | (LP) => "("
  | (KW_val) => "val"
  | (KW_type) => "type"
  | (KW_then) => "then"
  | (KW_orelse) => "orelse"
  | (KW_of) => "of"
  | (KW_mod) => "mod"
  | (KW_let) => "let"
  | (KW_in) => "in"
  | (KW_if) => "if"
  | (KW_fun) => "fun"
  | (KW_end) => "end"
  | (KW_else) => "else"
  | (KW_div) => "div"
  | (KW_datatype) => "datatype"
  | (KW_case) => "case"
  | (KW_andalso) => "andalso"
  | (KW_and) => "and"
(* end case *))
    fun isKW tok =
(case (tok)
 of (EOF) => false
  | (STRING(_)) => false
  | (NUMBER(_)) => false
  | (NAME(_)) => false
  | (TYVAR(_)) => false
  | (WILD) => false
  | (DARROW) => false
  | (ARROW) => false
  | (BAR) => false
  | (SEMI) => false
  | (COMMA) => false
  | (HASH) => false
  | (TILDE) => false
  | (NEQ) => false
  | (DEQ) => false
  | (EQ) => false
  | (TIMES) => false
  | (MINUS) => false
  | (DPLUS) => false
  | (PLUS) => false
  | (AT) => false
  | (DCOLON) => false
  | (COLON) => false
  | (GT) => false
  | (GTEQ) => false
  | (LT) => false
  | (LTEQ) => false
  | (BANG) => false
  | (RPB) => false
  | (LPB) => false
  | (RCB) => false
  | (LCB) => false
  | (RB) => false
  | (LB) => false
  | (RP) => false
  | (LP) => false
  | (KW_val) => false
  | (KW_type) => false
  | (KW_then) => false
  | (KW_orelse) => false
  | (KW_of) => false
  | (KW_mod) => false
  | (KW_let) => false
  | (KW_in) => false
  | (KW_if) => false
  | (KW_fun) => false
  | (KW_end) => false
  | (KW_else) => false
  | (KW_div) => false
  | (KW_datatype) => false
  | (KW_case) => false
  | (KW_andalso) => false
  | (KW_and) => false
(* end case *))


  fun toksToString toks = String.concatWith " " (map toString toks)

  fun isEOF EOF = true
    | isEOF _ = false

end

functor MinMLParseFn(Lex : ANTLR_LEXER) = struct

  local
    structure Tok = 
MinMLTokens
    structure UserCode = struct

 
  structure L = Literal
  structure PT = ParseTree
  structure Op = BasisNames


  fun mark cons ((l, _) : AntlrStreamPos.span, tr) = cons{
	  lnum = AntlrStreamPos.lineNo (!Error.sourceMap) l,
	  tree = tr
	}

  fun flatten NONE = []
    | flatten (SOME(x, l)) = x::l


  val markDecl = mark PT.MarkDecl
  val markTy = mark PT.MarkTy
  val markExp = mark PT.MarkExp
  val markMatch = mark PT.MarkMatch
  val markPat = mark PT.MarkPat
  val markAtomPat = mark PT.MarkAPat


  fun mkTyApp (arg, []) = arg
    | mkTyApp (arg, (span, id)::r) =
	mkTyApp (markTy(span, PT.NamedTy([arg], id)), r)


  fun mkCondExp con = let
	fun mk (e, []) = e
	  | mk (e, e'::r) = mk (con(e', e), r)
	in
	  mk
	end


  fun mkBinApp (e1, rator, e2) = PT.BinaryExp(e1, rator, e2)


  fun mkLBinExp (e, []) = e
    | mkLBinExp (e, (id, e')::r) = mkLBinExp (mkBinApp(e, id, e'), r)


  fun mkRBinExp (e, []) = e
    | mkRBinExp (e, [(id, e')]) = mkBinApp(e, id, e')
    | mkRBinExp (e, (id, e')::r) = mkBinApp(e, id, mkRBinExp(e', r))


  fun mkApply (e, []) = e
    | mkApply (e, e'::r) = mkApply (PT.ApplyExp(e, e'), r)


fun Program_PROD_1_ACT (SR, Exp, SR_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( SR, Exp)
fun TopDecl_PROD_1_ACT (EQ, NAME, Type, KW_type, TyParams, EQ_SPAN : (Lex.pos * Lex.pos), NAME_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), KW_type_SPAN : (Lex.pos * Lex.pos), TyParams_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markDecl (KW_type_SPAN, PT.TyDecl(TyParams, NAME, Type)))
fun TopDecl_PROD_2_ACT (EQ, SR, NAME, KW_datatype, TyParams, ConsDecl, EQ_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), NAME_SPAN : (Lex.pos * Lex.pos), KW_datatype_SPAN : (Lex.pos * Lex.pos), TyParams_SPAN : (Lex.pos * Lex.pos), ConsDecl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markDecl (KW_datatype_SPAN, PT.DataDecl(TyParams, NAME, ConsDecl :: SR)))
fun TopDecl_PROD_3_ACT (ValueDecl, ValueDecl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markDecl (ValueDecl_SPAN, PT.ValueDecl ValueDecl))
fun TyParams_PROD_1_ACT (FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( [])
fun TyParams_PROD_2_ACT (TYVAR, TYVAR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( [TYVAR])
fun TyParams_PROD_3_ACT (LP, RP, SR, TYVAR, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), TYVAR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( TYVAR :: SR)
fun Type_PROD_1_ACT (SR, TupleType, SR_SPAN : (Lex.pos * Lex.pos), TupleType_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( case SR of SOME ty => markTy(FULL_SPAN, PT.FunTy(TupleType, ty)) | _ => TupleType)
fun TupleType_PROD_1_ACT (SR, TyApp, SR_SPAN : (Lex.pos * Lex.pos), TyApp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( if null SR
		    then TyApp
		    else markTy (FULL_SPAN, PT.TupleTy (TyApp :: SR)))
fun TyApp_PROD_1_SUBRULE_1_PROD_1_ACT (NAME, NAME_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( NAME_SPAN, NAME)
fun TyApp_PROD_1_ACT (SR, NAME, SR_SPAN : (Lex.pos * Lex.pos), NAME_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( mkTyApp (markTy(NAME_SPAN, PT.NamedTy([], NAME)), SR))
fun TyApp_PROD_2_SUBRULE_1_PROD_1_ACT (NAME, TYVAR, NAME_SPAN : (Lex.pos * Lex.pos), TYVAR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( NAME_SPAN, NAME)
fun TyApp_PROD_2_ACT (SR, TYVAR, SR_SPAN : (Lex.pos * Lex.pos), TYVAR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( mkTyApp (markTy(TYVAR_SPAN, PT.VarTy TYVAR), SR))
fun TyApp_PROD_3_ACT (LP, Type, optTypes, LP_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), optTypes_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markTy (FULL_SPAN, optTypes Type))
fun optTypes_PROD_1_ACT (RP, NAME, RP_SPAN : (Lex.pos * Lex.pos), NAME_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( case NAME
		     of SOME id => (fn ty => PT.NamedTy([ty], id))
		      | _ => (fn ty => ty)
		    )
fun optTypes_PROD_2_ACT (RP, SR, NAME, RP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), NAME_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( fn ty => PT.NamedTy(ty::SR, NAME))
fun ConsDecl_PROD_1_ACT (SR, NAME, SR_SPAN : (Lex.pos * Lex.pos), NAME_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( mark PT.MarkConDecl (FULL_SPAN, PT.ConDecl(NAME, SR)))
fun ValueDecl_PROD_1_ACT (EQ, Exp, KW_val, TuplePat, EQ_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), KW_val_SPAN : (Lex.pos * Lex.pos), TuplePat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( mark PT.MarkVDecl (KW_val_SPAN, PT.ValVDecl(TuplePat, Exp)))
fun ValueDecl_PROD_2_ACT (SR, FunDef, KW_fun, SR_SPAN : (Lex.pos * Lex.pos), FunDef_SPAN : (Lex.pos * Lex.pos), KW_fun_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( mark PT.MarkVDecl (KW_fun_SPAN, PT.FunVDecl(FunDef :: SR)))
fun FunDef_PROD_1_ACT (EQ, Exp, NAME, TuplePat, EQ_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), NAME_SPAN : (Lex.pos * Lex.pos), TuplePat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( mark PT.MarkFunct (FULL_SPAN, PT.Funct(NAME, TuplePat, Exp)))
fun Exp_PROD_1_ACT (AndalsoExp, AndalsoExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( AndalsoExp)
fun Exp_PROD_2_ACT (SR, Exp, ValueDecl, KW_in, KW_end, KW_let, SR_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), ValueDecl_SPAN : (Lex.pos * Lex.pos), KW_in_SPAN : (Lex.pos * Lex.pos), KW_end_SPAN : (Lex.pos * Lex.pos), KW_let_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markExp (FULL_SPAN, PT.LetExp(ValueDecl, if null SR then Exp else PT.SeqExp(Exp::SR))))
fun Exp_PROD_3_ACT (Exp1, Exp2, Exp3, KW_else, KW_then, KW_if, Exp1_SPAN : (Lex.pos * Lex.pos), Exp2_SPAN : (Lex.pos * Lex.pos), Exp3_SPAN : (Lex.pos * Lex.pos), KW_else_SPAN : (Lex.pos * Lex.pos), KW_then_SPAN : (Lex.pos * Lex.pos), KW_if_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markExp (FULL_SPAN, PT.IfExp(Exp1, Exp2, Exp3)))
fun Exp_PROD_4_ACT (Exp, KW_case, KW_of, MatchRules, Exp_SPAN : (Lex.pos * Lex.pos), KW_case_SPAN : (Lex.pos * Lex.pos), KW_of_SPAN : (Lex.pos * Lex.pos), MatchRules_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markExp (FULL_SPAN, PT.CaseExp(Exp, MatchRules)))
fun ClosedExp_PROD_1_ACT (AndalsoExp, AndalsoExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( AndalsoExp)
fun ClosedExp_PROD_2_ACT (SR, Exp, ValueDecl, KW_in, KW_end, KW_let, SR_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), ValueDecl_SPAN : (Lex.pos * Lex.pos), KW_in_SPAN : (Lex.pos * Lex.pos), KW_end_SPAN : (Lex.pos * Lex.pos), KW_let_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markExp (FULL_SPAN, PT.LetExp(ValueDecl, if null SR then Exp else PT.SeqExp(Exp::SR))))
fun ClosedExp_PROD_3_ACT (Exp1, Exp2, KW_else, KW_then, KW_if, ClosedExp, Exp1_SPAN : (Lex.pos * Lex.pos), Exp2_SPAN : (Lex.pos * Lex.pos), KW_else_SPAN : (Lex.pos * Lex.pos), KW_then_SPAN : (Lex.pos * Lex.pos), KW_if_SPAN : (Lex.pos * Lex.pos), ClosedExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markExp (FULL_SPAN, PT.IfExp(Exp1, Exp2, ClosedExp)))
fun MatchRules_PROD_1_ACT (SR, Pat, DARROW, ClosedExp, SR_SPAN : (Lex.pos * Lex.pos), Pat_SPAN : (Lex.pos * Lex.pos), DARROW_SPAN : (Lex.pos * Lex.pos), ClosedExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markMatch (Pat_SPAN, PT.Match(Pat, ClosedExp)) :: Option.getOpt(SR, []))
fun MatchRules_PROD_2_ACT (Exp, Pat, DARROW, Exp_SPAN : (Lex.pos * Lex.pos), Pat_SPAN : (Lex.pos * Lex.pos), DARROW_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( [markMatch (FULL_SPAN, PT.Match(Pat, Exp))])
fun AndalsoExp_PROD_1_ACT (SR, OrelseExp, SR_SPAN : (Lex.pos * Lex.pos), OrelseExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markExp (FULL_SPAN, mkCondExp PT.AndAlsoExp (OrelseExp, SR)))
fun OrelseExp_PROD_1_ACT (SR, RExp, SR_SPAN : (Lex.pos * Lex.pos), RExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markExp (FULL_SPAN, mkCondExp PT.OrElseExp (RExp, SR)))
fun RExp_PROD_1_ACT (SR, LExp, SR_SPAN : (Lex.pos * Lex.pos), LExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markExp (FULL_SPAN, mkLBinExp(LExp, SR)))
fun RelOp_PROD_1_ACT (LT, LT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Op.lt)
fun RelOp_PROD_2_ACT (LTEQ, LTEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Op.lte)
fun RelOp_PROD_3_ACT (DEQ, DEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Op.eq)
fun RelOp_PROD_4_ACT (NEQ, NEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Op.neq)
fun RelOp_PROD_5_ACT (GTEQ, GTEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Op.gte)
fun RelOp_PROD_6_ACT (GT, GT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Op.gt)
fun LExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1_ACT (AExp, DCOLON, AExp_SPAN : (Lex.pos * Lex.pos), DCOLON_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Op.listCons)
fun LExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2_ACT (AT, AExp, AT_SPAN : (Lex.pos * Lex.pos), AExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Op.append)
fun LExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_3_ACT (AExp, DPLUS, AExp_SPAN : (Lex.pos * Lex.pos), DPLUS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Op.arrayConcat)
fun LExp_PROD_1_SUBRULE_1_PROD_1_ACT (SR, AExp, SR_SPAN : (Lex.pos * Lex.pos), AExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( SR, AExp)
fun LExp_PROD_1_ACT (SR, AExp, SR_SPAN : (Lex.pos * Lex.pos), AExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markExp (FULL_SPAN, mkRBinExp(AExp, SR)))
fun AExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1_ACT (MExp, PLUS, MExp_SPAN : (Lex.pos * Lex.pos), PLUS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Op.plus)
fun AExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2_ACT (MExp, MINUS, MExp_SPAN : (Lex.pos * Lex.pos), MINUS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Op.minus)
fun AExp_PROD_1_SUBRULE_1_PROD_1_ACT (SR, MExp, SR_SPAN : (Lex.pos * Lex.pos), MExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( SR, MExp)
fun AExp_PROD_1_ACT (SR, MExp, SR_SPAN : (Lex.pos * Lex.pos), MExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markExp (FULL_SPAN, mkLBinExp(MExp, SR)))
fun MExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1_ACT (TIMES, ApplyExp, TIMES_SPAN : (Lex.pos * Lex.pos), ApplyExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Op.times)
fun MExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2_ACT (ApplyExp, KW_div, ApplyExp_SPAN : (Lex.pos * Lex.pos), KW_div_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Op.div)
fun MExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_3_ACT (ApplyExp, KW_mod, ApplyExp_SPAN : (Lex.pos * Lex.pos), KW_mod_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Op.mod)
fun MExp_PROD_1_SUBRULE_1_PROD_1_ACT (SR, ApplyExp, SR_SPAN : (Lex.pos * Lex.pos), ApplyExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( SR, ApplyExp)
fun MExp_PROD_1_ACT (SR, ApplyExp, SR_SPAN : (Lex.pos * Lex.pos), ApplyExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markExp (FULL_SPAN, mkLBinExp(ApplyExp, SR)))
fun ApplyExp_PROD_1_ACT (SubscriptExp1, SubscriptExp2, SubscriptExp1_SPAN : (Lex.pos * Lex.pos), SubscriptExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markExp (FULL_SPAN, mkApply(SubscriptExp1, SubscriptExp2)))
fun ApplyExp_PROD_2_ACT (TILDE, SubscriptExp, TILDE_SPAN : (Lex.pos * Lex.pos), SubscriptExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markExp (FULL_SPAN, PT.ApplyExp(PT.IdExp Op.uMinus, SubscriptExp)))
fun ApplyExp_PROD_3_ACT (HASH, SubscriptExp, HASH_SPAN : (Lex.pos * Lex.pos), SubscriptExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markExp (FULL_SPAN, PT.ApplyExp(PT.IdExp Op.length, SubscriptExp)))
fun SubscriptExp_PROD_1_SUBRULE_1_PROD_1_ACT (BANG, AtomicExp, BANG_SPAN : (Lex.pos * Lex.pos), AtomicExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Op.subscript, AtomicExp)
fun SubscriptExp_PROD_1_ACT (SR, AtomicExp, SR_SPAN : (Lex.pos * Lex.pos), AtomicExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markExp (FULL_SPAN, mkLBinExp(AtomicExp, SR)))
fun AtomicExp_PROD_1_ACT (NAME, NAME_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markExp (FULL_SPAN, PT.IdExp NAME))
fun AtomicExp_PROD_2_ACT (NUMBER, NUMBER_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markExp (FULL_SPAN, PT.ConstExp(L.Int NUMBER)))
fun AtomicExp_PROD_3_ACT (STRING, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markExp (FULL_SPAN, PT.ConstExp(L.Str STRING)))
fun AtomicExp_PROD_4_ACT (LP, RP, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markExp (FULL_SPAN, PT.TupleExp[]))
fun AtomicExp_PROD_5_ACT (LP, RP, Exp, optExps, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), optExps_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markExp (FULL_SPAN, optExps Exp))
fun AtomicExp_PROD_6_ACT (SR, LPB, RPB, SR_SPAN : (Lex.pos * Lex.pos), LPB_SPAN : (Lex.pos * Lex.pos), RPB_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markExp (FULL_SPAN, PT.PArrayExp(flatten SR)))
fun AtomicExp_PROD_7_ACT (SR, LCB, Pat, RCB, Exp1, Exp2, COLON, KW_in, SR_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), Pat_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), Exp1_SPAN : (Lex.pos * Lex.pos), Exp2_SPAN : (Lex.pos * Lex.pos), COLON_SPAN : (Lex.pos * Lex.pos), KW_in_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markExp (FULL_SPAN, PT.PForallExp(Exp1, Pat, Exp2, SR)))
fun optExps_PROD_1_ACT (FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( fn e => e)
fun optExps_PROD_2_ACT (SR, SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( fn e => PT.TupleExp(e :: SR))
fun optExps_PROD_3_ACT (SR, SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( fn e => PT.SeqExp(e :: SR))
fun Pat_PROD_1_ACT (NAME, TuplePat, NAME_SPAN : (Lex.pos * Lex.pos), TuplePat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markPat (FULL_SPAN, PT.ConPat(NAME, TuplePat)))
fun Pat_PROD_2_ACT (TuplePat, TuplePat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markPat (FULL_SPAN, PT.TuplePat TuplePat))
fun Pat_PROD_3_ACT (AtomicPat1, AtomicPat2, DCOLON, AtomicPat1_SPAN : (Lex.pos * Lex.pos), AtomicPat2_SPAN : (Lex.pos * Lex.pos), DCOLON_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markPat (FULL_SPAN, PT.ConPat(Op.listCons, [AtomicPat1, AtomicPat2])))
fun TuplePat_PROD_1_ACT (AtomicPat, AtomicPat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( [AtomicPat])
fun TuplePat_PROD_2_ACT (LP, RP, SR, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( case SR of NONE => [] | SOME (p, ps) => p::ps)
fun AtomicPat_PROD_1_ACT (NAME, NAME_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markAtomPat (FULL_SPAN, PT.IdPat NAME))
fun AtomicPat_PROD_2_ACT (WILD, WILD_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markAtomPat (FULL_SPAN, PT.WildPat))
fun AtomicPat_PROD_3_ACT (NUMBER, NUMBER_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markAtomPat (FULL_SPAN, PT.ConstPat(L.Int NUMBER)))
fun AtomicPat_PROD_4_ACT (STRING, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( markAtomPat (FULL_SPAN, PT.ConstPat(L.Str STRING)))

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
fun matchSTRING strm = (case (lex(strm))
 of (Tok.STRING(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchNUMBER strm = (case (lex(strm))
 of (Tok.NUMBER(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchNAME strm = (case (lex(strm))
 of (Tok.NAME(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchTYVAR strm = (case (lex(strm))
 of (Tok.TYVAR(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchWILD strm = (case (lex(strm))
 of (Tok.WILD, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDARROW strm = (case (lex(strm))
 of (Tok.DARROW, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchARROW strm = (case (lex(strm))
 of (Tok.ARROW, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchBAR strm = (case (lex(strm))
 of (Tok.BAR, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSEMI strm = (case (lex(strm))
 of (Tok.SEMI, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCOMMA strm = (case (lex(strm))
 of (Tok.COMMA, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchHASH strm = (case (lex(strm))
 of (Tok.HASH, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchTILDE strm = (case (lex(strm))
 of (Tok.TILDE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchNEQ strm = (case (lex(strm))
 of (Tok.NEQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDEQ strm = (case (lex(strm))
 of (Tok.DEQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchEQ strm = (case (lex(strm))
 of (Tok.EQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchTIMES strm = (case (lex(strm))
 of (Tok.TIMES, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchMINUS strm = (case (lex(strm))
 of (Tok.MINUS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDPLUS strm = (case (lex(strm))
 of (Tok.DPLUS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchPLUS strm = (case (lex(strm))
 of (Tok.PLUS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchAT strm = (case (lex(strm))
 of (Tok.AT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDCOLON strm = (case (lex(strm))
 of (Tok.DCOLON, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCOLON strm = (case (lex(strm))
 of (Tok.COLON, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchGT strm = (case (lex(strm))
 of (Tok.GT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchGTEQ strm = (case (lex(strm))
 of (Tok.GTEQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLT strm = (case (lex(strm))
 of (Tok.LT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLTEQ strm = (case (lex(strm))
 of (Tok.LTEQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchBANG strm = (case (lex(strm))
 of (Tok.BANG, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRPB strm = (case (lex(strm))
 of (Tok.RPB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLPB strm = (case (lex(strm))
 of (Tok.LPB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRCB strm = (case (lex(strm))
 of (Tok.RCB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLCB strm = (case (lex(strm))
 of (Tok.LCB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRB strm = (case (lex(strm))
 of (Tok.RB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLB strm = (case (lex(strm))
 of (Tok.LB, span, strm') => ((), span, strm')
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
fun matchKW_val strm = (case (lex(strm))
 of (Tok.KW_val, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_type strm = (case (lex(strm))
 of (Tok.KW_type, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_then strm = (case (lex(strm))
 of (Tok.KW_then, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_orelse strm = (case (lex(strm))
 of (Tok.KW_orelse, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_of strm = (case (lex(strm))
 of (Tok.KW_of, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_mod strm = (case (lex(strm))
 of (Tok.KW_mod, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_let strm = (case (lex(strm))
 of (Tok.KW_let, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_in strm = (case (lex(strm))
 of (Tok.KW_in, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_if strm = (case (lex(strm))
 of (Tok.KW_if, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_fun strm = (case (lex(strm))
 of (Tok.KW_fun, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_end strm = (case (lex(strm))
 of (Tok.KW_end, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_else strm = (case (lex(strm))
 of (Tok.KW_else, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_div strm = (case (lex(strm))
 of (Tok.KW_div, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_datatype strm = (case (lex(strm))
 of (Tok.KW_datatype, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_case strm = (case (lex(strm))
 of (Tok.KW_case, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_andalso strm = (case (lex(strm))
 of (Tok.KW_andalso, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_and strm = (case (lex(strm))
 of (Tok.KW_and, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))

val (Program_NT) = 
let
fun AtomicPat_NT (strm) = let
      fun AtomicPat_PROD_1 (strm) = let
            val (NAME_RES, NAME_SPAN, strm') = matchNAME(strm)
            val FULL_SPAN = (#1(NAME_SPAN), #2(NAME_SPAN))
            in
              (UserCode.AtomicPat_PROD_1_ACT (NAME_RES, NAME_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicPat_PROD_2 (strm) = let
            val (WILD_RES, WILD_SPAN, strm') = matchWILD(strm)
            val FULL_SPAN = (#1(WILD_SPAN), #2(WILD_SPAN))
            in
              (UserCode.AtomicPat_PROD_2_ACT (WILD_RES, WILD_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicPat_PROD_3 (strm) = let
            val (NUMBER_RES, NUMBER_SPAN, strm') = matchNUMBER(strm)
            val FULL_SPAN = (#1(NUMBER_SPAN), #2(NUMBER_SPAN))
            in
              (UserCode.AtomicPat_PROD_3_ACT (NUMBER_RES, NUMBER_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicPat_PROD_4 (strm) = let
            val (STRING_RES, STRING_SPAN, strm') = matchSTRING(strm)
            val FULL_SPAN = (#1(STRING_SPAN), #2(STRING_SPAN))
            in
              (UserCode.AtomicPat_PROD_4_ACT (STRING_RES, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.STRING(_), _, strm') => AtomicPat_PROD_4(strm)
          | (Tok.WILD, _, strm') => AtomicPat_PROD_2(strm)
          | (Tok.NAME(_), _, strm') => AtomicPat_PROD_1(strm)
          | (Tok.NUMBER(_), _, strm') => AtomicPat_PROD_3(strm)
          | _ => fail()
        (* end case *))
      end
fun TuplePat_NT (strm) = let
      fun TuplePat_PROD_1 (strm) = let
            val (AtomicPat_RES, AtomicPat_SPAN, strm') = AtomicPat_NT(strm)
            val FULL_SPAN = (#1(AtomicPat_SPAN), #2(AtomicPat_SPAN))
            in
              (UserCode.TuplePat_PROD_1_ACT (AtomicPat_RES, AtomicPat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun TuplePat_PROD_2 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            fun TuplePat_PROD_2_SUBRULE_1_NT (strm) = let
                  val (AtomicPat_RES, AtomicPat_SPAN, strm') = AtomicPat_NT(strm)
                  fun TuplePat_PROD_2_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                        val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                        val (AtomicPat_RES, AtomicPat_SPAN, strm') = AtomicPat_NT(strm')
                        val FULL_SPAN = (#1(COMMA_SPAN), #2(AtomicPat_SPAN))
                        in
                          ((AtomicPat_RES), FULL_SPAN, strm')
                        end
                  fun TuplePat_PROD_2_SUBRULE_1_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                         of (Tok.COMMA, _, strm') => true
                          | _ => false
                        (* end case *))
                  val (SR_RES, SR_SPAN, strm') = EBNF.closure(TuplePat_PROD_2_SUBRULE_1_PROD_1_SUBRULE_1_PRED, TuplePat_PROD_2_SUBRULE_1_PROD_1_SUBRULE_1_NT, strm')
                  val FULL_SPAN = (#1(AtomicPat_SPAN), #2(SR_SPAN))
                  in
                    ((AtomicPat_RES, SR_RES), FULL_SPAN, strm')
                  end
            fun TuplePat_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.WILD, _, strm') => true
                    | (Tok.NAME(_), _, strm') => true
                    | (Tok.NUMBER(_), _, strm') => true
                    | (Tok.STRING(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.optional(TuplePat_PROD_2_SUBRULE_1_PRED, TuplePat_PROD_2_SUBRULE_1_NT, strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.TuplePat_PROD_2_ACT (LP_RES, RP_RES, SR_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LP, _, strm') => TuplePat_PROD_2(strm)
          | (Tok.WILD, _, strm') => TuplePat_PROD_1(strm)
          | (Tok.NAME(_), _, strm') => TuplePat_PROD_1(strm)
          | (Tok.NUMBER(_), _, strm') => TuplePat_PROD_1(strm)
          | (Tok.STRING(_), _, strm') => TuplePat_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun Pat_NT (strm) = let
      fun Pat_PROD_1 (strm) = let
            val (NAME_RES, NAME_SPAN, strm') = matchNAME(strm)
            val (TuplePat_RES, TuplePat_SPAN, strm') = TuplePat_NT(strm')
            val FULL_SPAN = (#1(NAME_SPAN), #2(TuplePat_SPAN))
            in
              (UserCode.Pat_PROD_1_ACT (NAME_RES, TuplePat_RES, NAME_SPAN : (Lex.pos * Lex.pos), TuplePat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Pat_PROD_2 (strm) = let
            val (TuplePat_RES, TuplePat_SPAN, strm') = TuplePat_NT(strm)
            val FULL_SPAN = (#1(TuplePat_SPAN), #2(TuplePat_SPAN))
            in
              (UserCode.Pat_PROD_2_ACT (TuplePat_RES, TuplePat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Pat_PROD_3 (strm) = let
            val (AtomicPat1_RES, AtomicPat1_SPAN, strm') = AtomicPat_NT(strm)
            val (DCOLON_RES, DCOLON_SPAN, strm') = matchDCOLON(strm')
            val (AtomicPat2_RES, AtomicPat2_SPAN, strm') = AtomicPat_NT(strm')
            val FULL_SPAN = (#1(AtomicPat1_SPAN), #2(AtomicPat2_SPAN))
            in
              (UserCode.Pat_PROD_3_ACT (AtomicPat1_RES, AtomicPat2_RES, DCOLON_RES, AtomicPat1_SPAN : (Lex.pos * Lex.pos), AtomicPat2_SPAN : (Lex.pos * Lex.pos), DCOLON_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.NAME(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.LP, _, strm') => Pat_PROD_1(strm)
                | (Tok.WILD, _, strm') => Pat_PROD_1(strm)
                | (Tok.NAME(_), _, strm') => Pat_PROD_1(strm)
                | (Tok.NUMBER(_), _, strm') => Pat_PROD_1(strm)
                | (Tok.STRING(_), _, strm') => Pat_PROD_1(strm)
                | (Tok.DCOLON, _, strm') => Pat_PROD_3(strm)
                | (Tok.KW_in, _, strm') => Pat_PROD_2(strm)
                | (Tok.DARROW, _, strm') => Pat_PROD_2(strm)
                | _ => fail()
              (* end case *))
          | (Tok.WILD, _, strm') =>
              (case (lex(strm'))
               of (Tok.KW_in, _, strm') => Pat_PROD_2(strm)
                | (Tok.DARROW, _, strm') => Pat_PROD_2(strm)
                | (Tok.DCOLON, _, strm') => Pat_PROD_3(strm)
                | _ => fail()
              (* end case *))
          | (Tok.NUMBER(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.KW_in, _, strm') => Pat_PROD_2(strm)
                | (Tok.DARROW, _, strm') => Pat_PROD_2(strm)
                | (Tok.DCOLON, _, strm') => Pat_PROD_3(strm)
                | _ => fail()
              (* end case *))
          | (Tok.STRING(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.KW_in, _, strm') => Pat_PROD_2(strm)
                | (Tok.DARROW, _, strm') => Pat_PROD_2(strm)
                | (Tok.DCOLON, _, strm') => Pat_PROD_3(strm)
                | _ => fail()
              (* end case *))
          | (Tok.LP, _, strm') => Pat_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
fun RelOp_NT (strm) = let
      fun RelOp_PROD_1 (strm) = let
            val (LT_RES, LT_SPAN, strm') = matchLT(strm)
            val FULL_SPAN = (#1(LT_SPAN), #2(LT_SPAN))
            in
              (UserCode.RelOp_PROD_1_ACT (LT_RES, LT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun RelOp_PROD_2 (strm) = let
            val (LTEQ_RES, LTEQ_SPAN, strm') = matchLTEQ(strm)
            val FULL_SPAN = (#1(LTEQ_SPAN), #2(LTEQ_SPAN))
            in
              (UserCode.RelOp_PROD_2_ACT (LTEQ_RES, LTEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun RelOp_PROD_3 (strm) = let
            val (DEQ_RES, DEQ_SPAN, strm') = matchDEQ(strm)
            val FULL_SPAN = (#1(DEQ_SPAN), #2(DEQ_SPAN))
            in
              (UserCode.RelOp_PROD_3_ACT (DEQ_RES, DEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun RelOp_PROD_4 (strm) = let
            val (NEQ_RES, NEQ_SPAN, strm') = matchNEQ(strm)
            val FULL_SPAN = (#1(NEQ_SPAN), #2(NEQ_SPAN))
            in
              (UserCode.RelOp_PROD_4_ACT (NEQ_RES, NEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun RelOp_PROD_5 (strm) = let
            val (GTEQ_RES, GTEQ_SPAN, strm') = matchGTEQ(strm)
            val FULL_SPAN = (#1(GTEQ_SPAN), #2(GTEQ_SPAN))
            in
              (UserCode.RelOp_PROD_5_ACT (GTEQ_RES, GTEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun RelOp_PROD_6 (strm) = let
            val (GT_RES, GT_SPAN, strm') = matchGT(strm)
            val FULL_SPAN = (#1(GT_SPAN), #2(GT_SPAN))
            in
              (UserCode.RelOp_PROD_6_ACT (GT_RES, GT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.GT, _, strm') => RelOp_PROD_6(strm)
          | (Tok.NEQ, _, strm') => RelOp_PROD_4(strm)
          | (Tok.LTEQ, _, strm') => RelOp_PROD_2(strm)
          | (Tok.LT, _, strm') => RelOp_PROD_1(strm)
          | (Tok.DEQ, _, strm') => RelOp_PROD_3(strm)
          | (Tok.GTEQ, _, strm') => RelOp_PROD_5(strm)
          | _ => fail()
        (* end case *))
      end
fun Exp_NT (strm) = let
      fun Exp_PROD_1 (strm) = let
            val (AndalsoExp_RES, AndalsoExp_SPAN, strm') = AndalsoExp_NT(strm)
            val FULL_SPAN = (#1(AndalsoExp_SPAN), #2(AndalsoExp_SPAN))
            in
              (UserCode.Exp_PROD_1_ACT (AndalsoExp_RES, AndalsoExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Exp_PROD_2 (strm) = let
            val (KW_let_RES, KW_let_SPAN, strm') = matchKW_let(strm)
            fun Exp_PROD_2_SUBRULE_1_NT (strm) = let
                  val (ValueDecl_RES, ValueDecl_SPAN, strm') = ValueDecl_NT(strm)
                  val FULL_SPAN = (#1(ValueDecl_SPAN), #2(ValueDecl_SPAN))
                  in
                    ((ValueDecl_RES), FULL_SPAN, strm')
                  end
            fun Exp_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.KW_fun, _, strm') => true
                    | (Tok.KW_val, _, strm') => true
                    | _ => false
                  (* end case *))
            val (ValueDecl_RES, ValueDecl_SPAN, strm') = EBNF.posclos(Exp_PROD_2_SUBRULE_1_PRED, Exp_PROD_2_SUBRULE_1_NT, strm')
            val (KW_in_RES, KW_in_SPAN, strm') = matchKW_in(strm')
            val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm')
            fun Exp_PROD_2_SUBRULE_2_NT (strm) = let
                  val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm)
                  val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm')
                  val FULL_SPAN = (#1(SEMI_SPAN), #2(Exp_SPAN))
                  in
                    ((Exp_RES), FULL_SPAN, strm')
                  end
            fun Exp_PROD_2_SUBRULE_2_PRED (strm) = (case (lex(strm))
                   of (Tok.SEMI, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(Exp_PROD_2_SUBRULE_2_PRED, Exp_PROD_2_SUBRULE_2_NT, strm')
            val (KW_end_RES, KW_end_SPAN, strm') = matchKW_end(strm')
            val FULL_SPAN = (#1(KW_let_SPAN), #2(KW_end_SPAN))
            in
              (UserCode.Exp_PROD_2_ACT (SR_RES, Exp_RES, ValueDecl_RES, KW_in_RES, KW_end_RES, KW_let_RES, SR_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), ValueDecl_SPAN : (Lex.pos * Lex.pos), KW_in_SPAN : (Lex.pos * Lex.pos), KW_end_SPAN : (Lex.pos * Lex.pos), KW_let_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Exp_PROD_3 (strm) = let
            val (KW_if_RES, KW_if_SPAN, strm') = matchKW_if(strm)
            val (Exp1_RES, Exp1_SPAN, strm') = Exp_NT(strm')
            val (KW_then_RES, KW_then_SPAN, strm') = matchKW_then(strm')
            val (Exp2_RES, Exp2_SPAN, strm') = Exp_NT(strm')
            val (KW_else_RES, KW_else_SPAN, strm') = matchKW_else(strm')
            val (Exp3_RES, Exp3_SPAN, strm') = Exp_NT(strm')
            val FULL_SPAN = (#1(KW_if_SPAN), #2(Exp3_SPAN))
            in
              (UserCode.Exp_PROD_3_ACT (Exp1_RES, Exp2_RES, Exp3_RES, KW_else_RES, KW_then_RES, KW_if_RES, Exp1_SPAN : (Lex.pos * Lex.pos), Exp2_SPAN : (Lex.pos * Lex.pos), Exp3_SPAN : (Lex.pos * Lex.pos), KW_else_SPAN : (Lex.pos * Lex.pos), KW_then_SPAN : (Lex.pos * Lex.pos), KW_if_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Exp_PROD_4 (strm) = let
            val (KW_case_RES, KW_case_SPAN, strm') = matchKW_case(strm)
            val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm')
            val (KW_of_RES, KW_of_SPAN, strm') = matchKW_of(strm')
            val (MatchRules_RES, MatchRules_SPAN, strm') = MatchRules_NT(strm')
            val FULL_SPAN = (#1(KW_case_SPAN), #2(MatchRules_SPAN))
            in
              (UserCode.Exp_PROD_4_ACT (Exp_RES, KW_case_RES, KW_of_RES, MatchRules_RES, Exp_SPAN : (Lex.pos * Lex.pos), KW_case_SPAN : (Lex.pos * Lex.pos), KW_of_SPAN : (Lex.pos * Lex.pos), MatchRules_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_case, _, strm') => Exp_PROD_4(strm)
          | (Tok.KW_let, _, strm') => Exp_PROD_2(strm)
          | (Tok.LP, _, strm') => Exp_PROD_1(strm)
          | (Tok.LCB, _, strm') => Exp_PROD_1(strm)
          | (Tok.LPB, _, strm') => Exp_PROD_1(strm)
          | (Tok.TILDE, _, strm') => Exp_PROD_1(strm)
          | (Tok.HASH, _, strm') => Exp_PROD_1(strm)
          | (Tok.NAME(_), _, strm') => Exp_PROD_1(strm)
          | (Tok.NUMBER(_), _, strm') => Exp_PROD_1(strm)
          | (Tok.STRING(_), _, strm') => Exp_PROD_1(strm)
          | (Tok.KW_if, _, strm') => Exp_PROD_3(strm)
          | _ => fail()
        (* end case *))
      end
and MatchRules_NT (strm) = let
      fun MatchRules_PROD_1 (strm) = let
            val (Pat_RES, Pat_SPAN, strm') = Pat_NT(strm)
            val (DARROW_RES, DARROW_SPAN, strm') = matchDARROW(strm')
            val (ClosedExp_RES, ClosedExp_SPAN, strm') = ClosedExp_NT(strm')
            fun MatchRules_PROD_1_SUBRULE_1_NT (strm) = let
                  val (BAR_RES, BAR_SPAN, strm') = matchBAR(strm)
                  val (MatchRules_RES, MatchRules_SPAN, strm') = MatchRules_NT(strm')
                  val FULL_SPAN = (#1(BAR_SPAN), #2(MatchRules_SPAN))
                  in
                    ((MatchRules_RES), FULL_SPAN, strm')
                  end
            fun MatchRules_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.BAR, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.optional(MatchRules_PROD_1_SUBRULE_1_PRED, MatchRules_PROD_1_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(Pat_SPAN), #2(SR_SPAN))
            in
              (UserCode.MatchRules_PROD_1_ACT (SR_RES, Pat_RES, DARROW_RES, ClosedExp_RES, SR_SPAN : (Lex.pos * Lex.pos), Pat_SPAN : (Lex.pos * Lex.pos), DARROW_SPAN : (Lex.pos * Lex.pos), ClosedExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MatchRules_PROD_2 (strm) = let
            val (Pat_RES, Pat_SPAN, strm') = Pat_NT(strm)
            val (DARROW_RES, DARROW_SPAN, strm') = matchDARROW(strm')
            val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm')
            val FULL_SPAN = (#1(Pat_SPAN), #2(Exp_SPAN))
            in
              (UserCode.MatchRules_PROD_2_ACT (Exp_RES, Pat_RES, DARROW_RES, Exp_SPAN : (Lex.pos * Lex.pos), Pat_SPAN : (Lex.pos * Lex.pos), DARROW_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LP, _, strm') =>
              tryProds(strm, [MatchRules_PROD_1, MatchRules_PROD_2])
          | (Tok.WILD, _, strm') =>
              tryProds(strm, [MatchRules_PROD_1, MatchRules_PROD_2])
          | (Tok.NAME(_), _, strm') =>
              tryProds(strm, [MatchRules_PROD_1, MatchRules_PROD_2])
          | (Tok.NUMBER(_), _, strm') =>
              tryProds(strm, [MatchRules_PROD_1, MatchRules_PROD_2])
          | (Tok.STRING(_), _, strm') =>
              tryProds(strm, [MatchRules_PROD_1, MatchRules_PROD_2])
          | _ => fail()
        (* end case *))
      end
and ClosedExp_NT (strm) = let
      fun ClosedExp_PROD_1 (strm) = let
            val (AndalsoExp_RES, AndalsoExp_SPAN, strm') = AndalsoExp_NT(strm)
            val FULL_SPAN = (#1(AndalsoExp_SPAN), #2(AndalsoExp_SPAN))
            in
              (UserCode.ClosedExp_PROD_1_ACT (AndalsoExp_RES, AndalsoExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun ClosedExp_PROD_2 (strm) = let
            val (KW_let_RES, KW_let_SPAN, strm') = matchKW_let(strm)
            fun ClosedExp_PROD_2_SUBRULE_1_NT (strm) = let
                  val (ValueDecl_RES, ValueDecl_SPAN, strm') = ValueDecl_NT(strm)
                  val FULL_SPAN = (#1(ValueDecl_SPAN), #2(ValueDecl_SPAN))
                  in
                    ((ValueDecl_RES), FULL_SPAN, strm')
                  end
            fun ClosedExp_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.KW_fun, _, strm') => true
                    | (Tok.KW_val, _, strm') => true
                    | _ => false
                  (* end case *))
            val (ValueDecl_RES, ValueDecl_SPAN, strm') = EBNF.posclos(ClosedExp_PROD_2_SUBRULE_1_PRED, ClosedExp_PROD_2_SUBRULE_1_NT, strm')
            val (KW_in_RES, KW_in_SPAN, strm') = matchKW_in(strm')
            val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm')
            fun ClosedExp_PROD_2_SUBRULE_2_NT (strm) = let
                  val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm)
                  val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm')
                  val FULL_SPAN = (#1(SEMI_SPAN), #2(Exp_SPAN))
                  in
                    ((Exp_RES), FULL_SPAN, strm')
                  end
            fun ClosedExp_PROD_2_SUBRULE_2_PRED (strm) = (case (lex(strm))
                   of (Tok.SEMI, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(ClosedExp_PROD_2_SUBRULE_2_PRED, ClosedExp_PROD_2_SUBRULE_2_NT, strm')
            val (KW_end_RES, KW_end_SPAN, strm') = matchKW_end(strm')
            val FULL_SPAN = (#1(KW_let_SPAN), #2(KW_end_SPAN))
            in
              (UserCode.ClosedExp_PROD_2_ACT (SR_RES, Exp_RES, ValueDecl_RES, KW_in_RES, KW_end_RES, KW_let_RES, SR_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), ValueDecl_SPAN : (Lex.pos * Lex.pos), KW_in_SPAN : (Lex.pos * Lex.pos), KW_end_SPAN : (Lex.pos * Lex.pos), KW_let_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun ClosedExp_PROD_3 (strm) = let
            val (KW_if_RES, KW_if_SPAN, strm') = matchKW_if(strm)
            val (Exp1_RES, Exp1_SPAN, strm') = Exp_NT(strm')
            val (KW_then_RES, KW_then_SPAN, strm') = matchKW_then(strm')
            val (Exp2_RES, Exp2_SPAN, strm') = Exp_NT(strm')
            val (KW_else_RES, KW_else_SPAN, strm') = matchKW_else(strm')
            val (ClosedExp_RES, ClosedExp_SPAN, strm') = ClosedExp_NT(strm')
            val FULL_SPAN = (#1(KW_if_SPAN), #2(ClosedExp_SPAN))
            in
              (UserCode.ClosedExp_PROD_3_ACT (Exp1_RES, Exp2_RES, KW_else_RES, KW_then_RES, KW_if_RES, ClosedExp_RES, Exp1_SPAN : (Lex.pos * Lex.pos), Exp2_SPAN : (Lex.pos * Lex.pos), KW_else_SPAN : (Lex.pos * Lex.pos), KW_then_SPAN : (Lex.pos * Lex.pos), KW_if_SPAN : (Lex.pos * Lex.pos), ClosedExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_if, _, strm') => ClosedExp_PROD_3(strm)
          | (Tok.LP, _, strm') => ClosedExp_PROD_1(strm)
          | (Tok.LCB, _, strm') => ClosedExp_PROD_1(strm)
          | (Tok.LPB, _, strm') => ClosedExp_PROD_1(strm)
          | (Tok.TILDE, _, strm') => ClosedExp_PROD_1(strm)
          | (Tok.HASH, _, strm') => ClosedExp_PROD_1(strm)
          | (Tok.NAME(_), _, strm') => ClosedExp_PROD_1(strm)
          | (Tok.NUMBER(_), _, strm') => ClosedExp_PROD_1(strm)
          | (Tok.STRING(_), _, strm') => ClosedExp_PROD_1(strm)
          | (Tok.KW_let, _, strm') => ClosedExp_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
and ValueDecl_NT (strm) = let
      fun ValueDecl_PROD_1 (strm) = let
            val (KW_val_RES, KW_val_SPAN, strm') = matchKW_val(strm)
            val (TuplePat_RES, TuplePat_SPAN, strm') = TuplePat_NT(strm')
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
            val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm')
            val FULL_SPAN = (#1(KW_val_SPAN), #2(Exp_SPAN))
            in
              (UserCode.ValueDecl_PROD_1_ACT (EQ_RES, Exp_RES, KW_val_RES, TuplePat_RES, EQ_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), KW_val_SPAN : (Lex.pos * Lex.pos), TuplePat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun ValueDecl_PROD_2 (strm) = let
            val (KW_fun_RES, KW_fun_SPAN, strm') = matchKW_fun(strm)
            val (FunDef_RES, FunDef_SPAN, strm') = FunDef_NT(strm')
            fun ValueDecl_PROD_2_SUBRULE_1_NT (strm) = let
                  val (KW_and_RES, KW_and_SPAN, strm') = matchKW_and(strm)
                  val (FunDef_RES, FunDef_SPAN, strm') = FunDef_NT(strm')
                  val FULL_SPAN = (#1(KW_and_SPAN), #2(FunDef_SPAN))
                  in
                    ((FunDef_RES), FULL_SPAN, strm')
                  end
            fun ValueDecl_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.KW_and, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(ValueDecl_PROD_2_SUBRULE_1_PRED, ValueDecl_PROD_2_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(KW_fun_SPAN), #2(SR_SPAN))
            in
              (UserCode.ValueDecl_PROD_2_ACT (SR_RES, FunDef_RES, KW_fun_RES, SR_SPAN : (Lex.pos * Lex.pos), FunDef_SPAN : (Lex.pos * Lex.pos), KW_fun_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_fun, _, strm') => ValueDecl_PROD_2(strm)
          | (Tok.KW_val, _, strm') => ValueDecl_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and FunDef_NT (strm) = let
      val (NAME_RES, NAME_SPAN, strm') = matchNAME(strm)
      val (TuplePat_RES, TuplePat_SPAN, strm') = TuplePat_NT(strm')
      val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
      val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm')
      val FULL_SPAN = (#1(NAME_SPAN), #2(Exp_SPAN))
      in
        (UserCode.FunDef_PROD_1_ACT (EQ_RES, Exp_RES, NAME_RES, TuplePat_RES, EQ_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), NAME_SPAN : (Lex.pos * Lex.pos), TuplePat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and AndalsoExp_NT (strm) = let
      val (OrelseExp_RES, OrelseExp_SPAN, strm') = OrelseExp_NT(strm)
      fun AndalsoExp_PROD_1_SUBRULE_1_NT (strm) = let
            val (KW_andalso_RES, KW_andalso_SPAN, strm') = matchKW_andalso(strm)
            val (OrelseExp_RES, OrelseExp_SPAN, strm') = OrelseExp_NT(strm')
            val FULL_SPAN = (#1(KW_andalso_SPAN), #2(OrelseExp_SPAN))
            in
              ((OrelseExp_RES), FULL_SPAN, strm')
            end
      fun AndalsoExp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_andalso, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(AndalsoExp_PROD_1_SUBRULE_1_PRED, AndalsoExp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(OrelseExp_SPAN), #2(SR_SPAN))
      in
        (UserCode.AndalsoExp_PROD_1_ACT (SR_RES, OrelseExp_RES, SR_SPAN : (Lex.pos * Lex.pos), OrelseExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and OrelseExp_NT (strm) = let
      val (RExp_RES, RExp_SPAN, strm') = RExp_NT(strm)
      fun OrelseExp_PROD_1_SUBRULE_1_NT (strm) = let
            val (KW_orelse_RES, KW_orelse_SPAN, strm') = matchKW_orelse(strm)
            val (RExp_RES, RExp_SPAN, strm') = RExp_NT(strm')
            val FULL_SPAN = (#1(KW_orelse_SPAN), #2(RExp_SPAN))
            in
              ((RExp_RES), FULL_SPAN, strm')
            end
      fun OrelseExp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_orelse, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(OrelseExp_PROD_1_SUBRULE_1_PRED, OrelseExp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(RExp_SPAN), #2(SR_SPAN))
      in
        (UserCode.OrelseExp_PROD_1_ACT (SR_RES, RExp_RES, SR_SPAN : (Lex.pos * Lex.pos), RExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and RExp_NT (strm) = let
      val (LExp_RES, LExp_SPAN, strm') = LExp_NT(strm)
      fun RExp_PROD_1_SUBRULE_1_NT (strm) = let
            val (RelOp_RES, RelOp_SPAN, strm') = RelOp_NT(strm)
            val (LExp_RES, LExp_SPAN, strm') = LExp_NT(strm')
            val FULL_SPAN = (#1(RelOp_SPAN), #2(LExp_SPAN))
            in
              ((RelOp_RES, LExp_RES), FULL_SPAN, strm')
            end
      fun RExp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.LTEQ, _, strm') => true
              | (Tok.LT, _, strm') => true
              | (Tok.GTEQ, _, strm') => true
              | (Tok.GT, _, strm') => true
              | (Tok.DEQ, _, strm') => true
              | (Tok.NEQ, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(RExp_PROD_1_SUBRULE_1_PRED, RExp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(LExp_SPAN), #2(SR_SPAN))
      in
        (UserCode.RExp_PROD_1_ACT (SR_RES, LExp_RES, SR_SPAN : (Lex.pos * Lex.pos), LExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and LExp_NT (strm) = let
      val (AExp_RES, AExp_SPAN, strm') = AExp_NT(strm)
      fun LExp_PROD_1_SUBRULE_1_NT (strm) = let
            val (SR_RES, SR_SPAN, strm') = let
            fun LExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                  fun LExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                        val (DCOLON_RES, DCOLON_SPAN, strm') = matchDCOLON(strm)
                        val FULL_SPAN = (#1(DCOLON_SPAN), #2(DCOLON_SPAN))
                        in
                          (UserCode.LExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1_ACT (AExp_RES, DCOLON_RES, AExp_SPAN : (Lex.pos * Lex.pos), DCOLON_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  fun LExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                        val (AT_RES, AT_SPAN, strm') = matchAT(strm)
                        val FULL_SPAN = (#1(AT_SPAN), #2(AT_SPAN))
                        in
                          (UserCode.LExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2_ACT (AT_RES, AExp_RES, AT_SPAN : (Lex.pos * Lex.pos), AExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  fun LExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_3 (strm) = let
                        val (DPLUS_RES, DPLUS_SPAN, strm') = matchDPLUS(strm)
                        val FULL_SPAN = (#1(DPLUS_SPAN), #2(DPLUS_SPAN))
                        in
                          (UserCode.LExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_3_ACT (AExp_RES, DPLUS_RES, AExp_SPAN : (Lex.pos * Lex.pos), DPLUS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  in
                    (case (lex(strm))
                     of (Tok.DPLUS, _, strm') =>
                          LExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_3(strm)
                      | (Tok.DCOLON, _, strm') =>
                          LExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1(strm)
                      | (Tok.AT, _, strm') =>
                          LExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2(strm)
                      | _ => fail()
                    (* end case *))
                  end
            in
              LExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT(strm)
            end
            val (AExp_RES, AExp_SPAN, strm') = AExp_NT(strm')
            val FULL_SPAN = (#1(SR_SPAN), #2(AExp_SPAN))
            in
              (UserCode.LExp_PROD_1_SUBRULE_1_PROD_1_ACT (SR_RES, AExp_RES, SR_SPAN : (Lex.pos * Lex.pos), AExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun LExp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.DCOLON, _, strm') => true
              | (Tok.AT, _, strm') => true
              | (Tok.DPLUS, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(LExp_PROD_1_SUBRULE_1_PRED, LExp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(AExp_SPAN), #2(SR_SPAN))
      in
        (UserCode.LExp_PROD_1_ACT (SR_RES, AExp_RES, SR_SPAN : (Lex.pos * Lex.pos), AExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and AExp_NT (strm) = let
      val (MExp_RES, MExp_SPAN, strm') = MExp_NT(strm)
      fun AExp_PROD_1_SUBRULE_1_NT (strm) = let
            val (SR_RES, SR_SPAN, strm') = let
            fun AExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                  fun AExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                        val (PLUS_RES, PLUS_SPAN, strm') = matchPLUS(strm)
                        val FULL_SPAN = (#1(PLUS_SPAN), #2(PLUS_SPAN))
                        in
                          (UserCode.AExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1_ACT (MExp_RES, PLUS_RES, MExp_SPAN : (Lex.pos * Lex.pos), PLUS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  fun AExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                        val (MINUS_RES, MINUS_SPAN, strm') = matchMINUS(strm)
                        val FULL_SPAN = (#1(MINUS_SPAN), #2(MINUS_SPAN))
                        in
                          (UserCode.AExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2_ACT (MExp_RES, MINUS_RES, MExp_SPAN : (Lex.pos * Lex.pos), MINUS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  in
                    (case (lex(strm))
                     of (Tok.MINUS, _, strm') =>
                          AExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2(strm)
                      | (Tok.PLUS, _, strm') =>
                          AExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1(strm)
                      | _ => fail()
                    (* end case *))
                  end
            in
              AExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT(strm)
            end
            val (MExp_RES, MExp_SPAN, strm') = MExp_NT(strm')
            val FULL_SPAN = (#1(SR_SPAN), #2(MExp_SPAN))
            in
              (UserCode.AExp_PROD_1_SUBRULE_1_PROD_1_ACT (SR_RES, MExp_RES, SR_SPAN : (Lex.pos * Lex.pos), MExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AExp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.PLUS, _, strm') => true
              | (Tok.MINUS, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(AExp_PROD_1_SUBRULE_1_PRED, AExp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(MExp_SPAN), #2(SR_SPAN))
      in
        (UserCode.AExp_PROD_1_ACT (SR_RES, MExp_RES, SR_SPAN : (Lex.pos * Lex.pos), MExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and MExp_NT (strm) = let
      val (ApplyExp_RES, ApplyExp_SPAN, strm') = ApplyExp_NT(strm)
      fun MExp_PROD_1_SUBRULE_1_NT (strm) = let
            val (SR_RES, SR_SPAN, strm') = let
            fun MExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                  fun MExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1 (strm) = let
                        val (TIMES_RES, TIMES_SPAN, strm') = matchTIMES(strm)
                        val FULL_SPAN = (#1(TIMES_SPAN), #2(TIMES_SPAN))
                        in
                          (UserCode.MExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1_ACT (TIMES_RES, ApplyExp_RES, TIMES_SPAN : (Lex.pos * Lex.pos), ApplyExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  fun MExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2 (strm) = let
                        val (KW_div_RES, KW_div_SPAN, strm') = matchKW_div(strm)
                        val FULL_SPAN = (#1(KW_div_SPAN), #2(KW_div_SPAN))
                        in
                          (UserCode.MExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2_ACT (ApplyExp_RES, KW_div_RES, ApplyExp_SPAN : (Lex.pos * Lex.pos), KW_div_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  fun MExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_3 (strm) = let
                        val (KW_mod_RES, KW_mod_SPAN, strm') = matchKW_mod(strm)
                        val FULL_SPAN = (#1(KW_mod_SPAN), #2(KW_mod_SPAN))
                        in
                          (UserCode.MExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_3_ACT (ApplyExp_RES, KW_mod_RES, ApplyExp_SPAN : (Lex.pos * Lex.pos), KW_mod_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                            FULL_SPAN, strm')
                        end
                  in
                    (case (lex(strm))
                     of (Tok.KW_mod, _, strm') =>
                          MExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_3(strm)
                      | (Tok.TIMES, _, strm') =>
                          MExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_1(strm)
                      | (Tok.KW_div, _, strm') =>
                          MExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PROD_2(strm)
                      | _ => fail()
                    (* end case *))
                  end
            in
              MExp_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT(strm)
            end
            val (ApplyExp_RES, ApplyExp_SPAN, strm') = ApplyExp_NT(strm')
            val FULL_SPAN = (#1(SR_SPAN), #2(ApplyExp_SPAN))
            in
              (UserCode.MExp_PROD_1_SUBRULE_1_PROD_1_ACT (SR_RES, ApplyExp_RES, SR_SPAN : (Lex.pos * Lex.pos), ApplyExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MExp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_div, _, strm') => true
              | (Tok.KW_mod, _, strm') => true
              | (Tok.TIMES, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(MExp_PROD_1_SUBRULE_1_PRED, MExp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(ApplyExp_SPAN), #2(SR_SPAN))
      in
        (UserCode.MExp_PROD_1_ACT (SR_RES, ApplyExp_RES, SR_SPAN : (Lex.pos * Lex.pos), ApplyExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and ApplyExp_NT (strm) = let
      fun ApplyExp_PROD_1 (strm) = let
            val (SubscriptExp1_RES, SubscriptExp1_SPAN, strm') = SubscriptExp_NT(strm)
            fun ApplyExp_PROD_1_SUBRULE_1_NT (strm) = let
                  val (SubscriptExp_RES, SubscriptExp_SPAN, strm') = SubscriptExp_NT(strm)
                  val FULL_SPAN = (#1(SubscriptExp_SPAN),
                    #2(SubscriptExp_SPAN))
                  in
                    ((SubscriptExp_RES), FULL_SPAN, strm')
                  end
            fun ApplyExp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.LP, _, strm') => true
                    | (Tok.LCB, _, strm') => true
                    | (Tok.LPB, _, strm') => true
                    | (Tok.NAME(_), _, strm') => true
                    | (Tok.NUMBER(_), _, strm') => true
                    | (Tok.STRING(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (SubscriptExp2_RES, SubscriptExp2_SPAN, strm') = EBNF.closure(ApplyExp_PROD_1_SUBRULE_1_PRED, ApplyExp_PROD_1_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(SubscriptExp1_SPAN), #2(SubscriptExp2_SPAN))
            in
              (UserCode.ApplyExp_PROD_1_ACT (SubscriptExp1_RES, SubscriptExp2_RES, SubscriptExp1_SPAN : (Lex.pos * Lex.pos), SubscriptExp2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun ApplyExp_PROD_2 (strm) = let
            val (TILDE_RES, TILDE_SPAN, strm') = matchTILDE(strm)
            val (SubscriptExp_RES, SubscriptExp_SPAN, strm') = SubscriptExp_NT(strm')
            val FULL_SPAN = (#1(TILDE_SPAN), #2(SubscriptExp_SPAN))
            in
              (UserCode.ApplyExp_PROD_2_ACT (TILDE_RES, SubscriptExp_RES, TILDE_SPAN : (Lex.pos * Lex.pos), SubscriptExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun ApplyExp_PROD_3 (strm) = let
            val (HASH_RES, HASH_SPAN, strm') = matchHASH(strm)
            val (SubscriptExp_RES, SubscriptExp_SPAN, strm') = SubscriptExp_NT(strm')
            val FULL_SPAN = (#1(HASH_SPAN), #2(SubscriptExp_SPAN))
            in
              (UserCode.ApplyExp_PROD_3_ACT (HASH_RES, SubscriptExp_RES, HASH_SPAN : (Lex.pos * Lex.pos), SubscriptExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.HASH, _, strm') => ApplyExp_PROD_3(strm)
          | (Tok.LP, _, strm') => ApplyExp_PROD_1(strm)
          | (Tok.LCB, _, strm') => ApplyExp_PROD_1(strm)
          | (Tok.LPB, _, strm') => ApplyExp_PROD_1(strm)
          | (Tok.NAME(_), _, strm') => ApplyExp_PROD_1(strm)
          | (Tok.NUMBER(_), _, strm') => ApplyExp_PROD_1(strm)
          | (Tok.STRING(_), _, strm') => ApplyExp_PROD_1(strm)
          | (Tok.TILDE, _, strm') => ApplyExp_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
and SubscriptExp_NT (strm) = let
      val (AtomicExp_RES, AtomicExp_SPAN, strm') = AtomicExp_NT(strm)
      fun SubscriptExp_PROD_1_SUBRULE_1_NT (strm) = let
            val (BANG_RES, BANG_SPAN, strm') = matchBANG(strm)
            val (AtomicExp_RES, AtomicExp_SPAN, strm') = AtomicExp_NT(strm')
            val FULL_SPAN = (#1(BANG_SPAN), #2(AtomicExp_SPAN))
            in
              (UserCode.SubscriptExp_PROD_1_SUBRULE_1_PROD_1_ACT (BANG_RES, AtomicExp_RES, BANG_SPAN : (Lex.pos * Lex.pos), AtomicExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun SubscriptExp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.BANG, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(SubscriptExp_PROD_1_SUBRULE_1_PRED, SubscriptExp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(AtomicExp_SPAN), #2(SR_SPAN))
      in
        (UserCode.SubscriptExp_PROD_1_ACT (SR_RES, AtomicExp_RES, SR_SPAN : (Lex.pos * Lex.pos), AtomicExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and AtomicExp_NT (strm) = let
      fun AtomicExp_PROD_1 (strm) = let
            val (NAME_RES, NAME_SPAN, strm') = matchNAME(strm)
            val FULL_SPAN = (#1(NAME_SPAN), #2(NAME_SPAN))
            in
              (UserCode.AtomicExp_PROD_1_ACT (NAME_RES, NAME_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExp_PROD_2 (strm) = let
            val (NUMBER_RES, NUMBER_SPAN, strm') = matchNUMBER(strm)
            val FULL_SPAN = (#1(NUMBER_SPAN), #2(NUMBER_SPAN))
            in
              (UserCode.AtomicExp_PROD_2_ACT (NUMBER_RES, NUMBER_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExp_PROD_3 (strm) = let
            val (STRING_RES, STRING_SPAN, strm') = matchSTRING(strm)
            val FULL_SPAN = (#1(STRING_SPAN), #2(STRING_SPAN))
            in
              (UserCode.AtomicExp_PROD_3_ACT (STRING_RES, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExp_PROD_4 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.AtomicExp_PROD_4_ACT (LP_RES, RP_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExp_PROD_5 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm')
            val (optExps_RES, optExps_SPAN, strm') = optExps_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.AtomicExp_PROD_5_ACT (LP_RES, RP_RES, Exp_RES, optExps_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), optExps_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExp_PROD_6 (strm) = let
            val (LPB_RES, LPB_SPAN, strm') = matchLPB(strm)
            fun AtomicExp_PROD_6_SUBRULE_1_NT (strm) = let
                  val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm)
                  fun AtomicExp_PROD_6_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                        val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                        val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm')
                        val FULL_SPAN = (#1(COMMA_SPAN), #2(Exp_SPAN))
                        in
                          ((Exp_RES), FULL_SPAN, strm')
                        end
                  fun AtomicExp_PROD_6_SUBRULE_1_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                         of (Tok.COMMA, _, strm') => true
                          | _ => false
                        (* end case *))
                  val (SR_RES, SR_SPAN, strm') = EBNF.closure(AtomicExp_PROD_6_SUBRULE_1_PROD_1_SUBRULE_1_PRED, AtomicExp_PROD_6_SUBRULE_1_PROD_1_SUBRULE_1_NT, strm')
                  val FULL_SPAN = (#1(Exp_SPAN), #2(SR_SPAN))
                  in
                    ((Exp_RES, SR_RES), FULL_SPAN, strm')
                  end
            fun AtomicExp_PROD_6_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.KW_case, _, strm') => true
                    | (Tok.KW_if, _, strm') => true
                    | (Tok.KW_let, _, strm') => true
                    | (Tok.LP, _, strm') => true
                    | (Tok.LCB, _, strm') => true
                    | (Tok.LPB, _, strm') => true
                    | (Tok.TILDE, _, strm') => true
                    | (Tok.HASH, _, strm') => true
                    | (Tok.NAME(_), _, strm') => true
                    | (Tok.NUMBER(_), _, strm') => true
                    | (Tok.STRING(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.optional(AtomicExp_PROD_6_SUBRULE_1_PRED, AtomicExp_PROD_6_SUBRULE_1_NT, strm')
            val (RPB_RES, RPB_SPAN, strm') = matchRPB(strm')
            val FULL_SPAN = (#1(LPB_SPAN), #2(RPB_SPAN))
            in
              (UserCode.AtomicExp_PROD_6_ACT (SR_RES, LPB_RES, RPB_RES, SR_SPAN : (Lex.pos * Lex.pos), LPB_SPAN : (Lex.pos * Lex.pos), RPB_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExp_PROD_7 (strm) = let
            val (LCB_RES, LCB_SPAN, strm') = matchLCB(strm)
            val (Exp1_RES, Exp1_SPAN, strm') = Exp_NT(strm')
            val (COLON_RES, COLON_SPAN, strm') = matchCOLON(strm')
            val (Pat_RES, Pat_SPAN, strm') = Pat_NT(strm')
            val (KW_in_RES, KW_in_SPAN, strm') = matchKW_in(strm')
            val (Exp2_RES, Exp2_SPAN, strm') = Exp_NT(strm')
            fun AtomicExp_PROD_7_SUBRULE_1_NT (strm) = let
                  val (COLON_RES, COLON_SPAN, strm') = matchCOLON(strm)
                  val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm')
                  val FULL_SPAN = (#1(COLON_SPAN), #2(Exp_SPAN))
                  in
                    ((Exp_RES), FULL_SPAN, strm')
                  end
            fun AtomicExp_PROD_7_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.COLON, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.optional(AtomicExp_PROD_7_SUBRULE_1_PRED, AtomicExp_PROD_7_SUBRULE_1_NT, strm')
            val (RCB_RES, RCB_SPAN, strm') = matchRCB(strm')
            val FULL_SPAN = (#1(LCB_SPAN), #2(RCB_SPAN))
            in
              (UserCode.AtomicExp_PROD_7_ACT (SR_RES, LCB_RES, Pat_RES, RCB_RES, Exp1_RES, Exp2_RES, COLON_RES, KW_in_RES, SR_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), Pat_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), Exp1_SPAN : (Lex.pos * Lex.pos), Exp2_SPAN : (Lex.pos * Lex.pos), COLON_SPAN : (Lex.pos * Lex.pos), KW_in_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LCB, _, strm') => AtomicExp_PROD_7(strm)
          | (Tok.STRING(_), _, strm') => AtomicExp_PROD_3(strm)
          | (Tok.NAME(_), _, strm') => AtomicExp_PROD_1(strm)
          | (Tok.NUMBER(_), _, strm') => AtomicExp_PROD_2(strm)
          | (Tok.LP, _, strm') =>
              (case (lex(strm'))
               of (Tok.RP, _, strm') => AtomicExp_PROD_4(strm)
                | (Tok.KW_case, _, strm') => AtomicExp_PROD_5(strm)
                | (Tok.KW_if, _, strm') => AtomicExp_PROD_5(strm)
                | (Tok.KW_let, _, strm') => AtomicExp_PROD_5(strm)
                | (Tok.LP, _, strm') => AtomicExp_PROD_5(strm)
                | (Tok.LCB, _, strm') => AtomicExp_PROD_5(strm)
                | (Tok.LPB, _, strm') => AtomicExp_PROD_5(strm)
                | (Tok.TILDE, _, strm') => AtomicExp_PROD_5(strm)
                | (Tok.HASH, _, strm') => AtomicExp_PROD_5(strm)
                | (Tok.NAME(_), _, strm') => AtomicExp_PROD_5(strm)
                | (Tok.NUMBER(_), _, strm') => AtomicExp_PROD_5(strm)
                | (Tok.STRING(_), _, strm') => AtomicExp_PROD_5(strm)
                | _ => fail()
              (* end case *))
          | (Tok.LPB, _, strm') => AtomicExp_PROD_6(strm)
          | _ => fail()
        (* end case *))
      end
and optExps_NT (strm) = let
      fun optExps_PROD_1 (strm) = let
            val FULL_SPAN = (Err.getPos(strm), Err.getPos(strm))
            in
              (UserCode.optExps_PROD_1_ACT (FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm)
            end
      fun optExps_PROD_2 (strm) = let
            fun optExps_PROD_2_SUBRULE_1_NT (strm) = let
                  val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                  val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm')
                  val FULL_SPAN = (#1(COMMA_SPAN), #2(Exp_SPAN))
                  in
                    ((Exp_RES), FULL_SPAN, strm')
                  end
            fun optExps_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.posclos(optExps_PROD_2_SUBRULE_1_PRED, optExps_PROD_2_SUBRULE_1_NT, strm)
            val FULL_SPAN = (#1(SR_SPAN), #2(SR_SPAN))
            in
              (UserCode.optExps_PROD_2_ACT (SR_RES, SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun optExps_PROD_3 (strm) = let
            fun optExps_PROD_3_SUBRULE_1_NT (strm) = let
                  val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm)
                  val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm')
                  val FULL_SPAN = (#1(SEMI_SPAN), #2(Exp_SPAN))
                  in
                    ((Exp_RES), FULL_SPAN, strm')
                  end
            fun optExps_PROD_3_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.SEMI, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.posclos(optExps_PROD_3_SUBRULE_1_PRED, optExps_PROD_3_SUBRULE_1_NT, strm)
            val FULL_SPAN = (#1(SR_SPAN), #2(SR_SPAN))
            in
              (UserCode.optExps_PROD_3_ACT (SR_RES, SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.SEMI, _, strm') => optExps_PROD_3(strm)
          | (Tok.RP, _, strm') => optExps_PROD_1(strm)
          | (Tok.COMMA, _, strm') => optExps_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
fun Type_NT (strm) = let
      val (TupleType_RES, TupleType_SPAN, strm') = TupleType_NT(strm)
      fun Type_PROD_1_SUBRULE_1_NT (strm) = let
            val (ARROW_RES, ARROW_SPAN, strm') = matchARROW(strm)
            val (Type_RES, Type_SPAN, strm') = Type_NT(strm')
            val FULL_SPAN = (#1(ARROW_SPAN), #2(Type_SPAN))
            in
              ((Type_RES), FULL_SPAN, strm')
            end
      fun Type_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.ARROW, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(Type_PROD_1_SUBRULE_1_PRED, Type_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(TupleType_SPAN), #2(SR_SPAN))
      in
        (UserCode.Type_PROD_1_ACT (SR_RES, TupleType_RES, SR_SPAN : (Lex.pos * Lex.pos), TupleType_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and TupleType_NT (strm) = let
      val (TyApp_RES, TyApp_SPAN, strm') = TyApp_NT(strm)
      fun TupleType_PROD_1_SUBRULE_1_NT (strm) = let
            val (TIMES_RES, TIMES_SPAN, strm') = matchTIMES(strm)
            val (TyApp_RES, TyApp_SPAN, strm') = TyApp_NT(strm')
            val FULL_SPAN = (#1(TIMES_SPAN), #2(TyApp_SPAN))
            in
              ((TyApp_RES), FULL_SPAN, strm')
            end
      fun TupleType_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.TIMES, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(TupleType_PROD_1_SUBRULE_1_PRED, TupleType_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(TyApp_SPAN), #2(SR_SPAN))
      in
        (UserCode.TupleType_PROD_1_ACT (SR_RES, TyApp_RES, SR_SPAN : (Lex.pos * Lex.pos), TyApp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and TyApp_NT (strm) = let
      fun TyApp_PROD_1 (strm) = let
            val (NAME_RES, NAME_SPAN, strm') = matchNAME(strm)
            fun TyApp_PROD_1_SUBRULE_1_NT (strm) = let
                  val (NAME_RES, NAME_SPAN, strm') = matchNAME(strm)
                  val FULL_SPAN = (#1(NAME_SPAN), #2(NAME_SPAN))
                  in
                    (UserCode.TyApp_PROD_1_SUBRULE_1_PROD_1_ACT (NAME_RES, NAME_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun TyApp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.NAME(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(TyApp_PROD_1_SUBRULE_1_PRED, TyApp_PROD_1_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(NAME_SPAN), #2(SR_SPAN))
            in
              (UserCode.TyApp_PROD_1_ACT (SR_RES, NAME_RES, SR_SPAN : (Lex.pos * Lex.pos), NAME_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun TyApp_PROD_2 (strm) = let
            val (TYVAR_RES, TYVAR_SPAN, strm') = matchTYVAR(strm)
            fun TyApp_PROD_2_SUBRULE_1_NT (strm) = let
                  val (NAME_RES, NAME_SPAN, strm') = matchNAME(strm)
                  val FULL_SPAN = (#1(NAME_SPAN), #2(NAME_SPAN))
                  in
                    (UserCode.TyApp_PROD_2_SUBRULE_1_PROD_1_ACT (NAME_RES, TYVAR_RES, NAME_SPAN : (Lex.pos * Lex.pos), TYVAR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun TyApp_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.NAME(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(TyApp_PROD_2_SUBRULE_1_PRED, TyApp_PROD_2_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(TYVAR_SPAN), #2(SR_SPAN))
            in
              (UserCode.TyApp_PROD_2_ACT (SR_RES, TYVAR_RES, SR_SPAN : (Lex.pos * Lex.pos), TYVAR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun TyApp_PROD_3 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (Type_RES, Type_SPAN, strm') = Type_NT(strm')
            val (optTypes_RES, optTypes_SPAN, strm') = optTypes_NT(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(optTypes_SPAN))
            in
              (UserCode.TyApp_PROD_3_ACT (LP_RES, Type_RES, optTypes_RES, LP_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), optTypes_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LP, _, strm') => TyApp_PROD_3(strm)
          | (Tok.NAME(_), _, strm') => TyApp_PROD_1(strm)
          | (Tok.TYVAR(_), _, strm') => TyApp_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
and optTypes_NT (strm) = let
      fun optTypes_PROD_1 (strm) = let
            val (RP_RES, RP_SPAN, strm') = matchRP(strm)
            fun optTypes_PROD_1_SUBRULE_1_NT (strm) = let
                  val (NAME_RES, NAME_SPAN, strm') = matchNAME(strm)
                  val FULL_SPAN = (#1(NAME_SPAN), #2(NAME_SPAN))
                  in
                    ((NAME_RES), FULL_SPAN, strm')
                  end
            fun optTypes_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.NAME(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (NAME_RES, NAME_SPAN, strm') = EBNF.optional(optTypes_PROD_1_SUBRULE_1_PRED, optTypes_PROD_1_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(RP_SPAN), #2(NAME_SPAN))
            in
              (UserCode.optTypes_PROD_1_ACT (RP_RES, NAME_RES, RP_SPAN : (Lex.pos * Lex.pos), NAME_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun optTypes_PROD_2 (strm) = let
            fun optTypes_PROD_2_SUBRULE_1_NT (strm) = let
                  val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                  val (Type_RES, Type_SPAN, strm') = Type_NT(strm')
                  val FULL_SPAN = (#1(COMMA_SPAN), #2(Type_SPAN))
                  in
                    ((Type_RES), FULL_SPAN, strm')
                  end
            fun optTypes_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.posclos(optTypes_PROD_2_SUBRULE_1_PRED, optTypes_PROD_2_SUBRULE_1_NT, strm)
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val (NAME_RES, NAME_SPAN, strm') = matchNAME(strm')
            val FULL_SPAN = (#1(SR_SPAN), #2(NAME_SPAN))
            in
              (UserCode.optTypes_PROD_2_ACT (RP_RES, SR_RES, NAME_RES, RP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), NAME_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.COMMA, _, strm') => optTypes_PROD_2(strm)
          | (Tok.RP, _, strm') => optTypes_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun ConsDecl_NT (strm) = let
      val (NAME_RES, NAME_SPAN, strm') = matchNAME(strm)
      fun ConsDecl_PROD_1_SUBRULE_1_NT (strm) = let
            val (KW_of_RES, KW_of_SPAN, strm') = matchKW_of(strm)
            val (Type_RES, Type_SPAN, strm') = Type_NT(strm')
            val FULL_SPAN = (#1(KW_of_SPAN), #2(Type_SPAN))
            in
              ((Type_RES), FULL_SPAN, strm')
            end
      fun ConsDecl_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_of, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(ConsDecl_PROD_1_SUBRULE_1_PRED, ConsDecl_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(NAME_SPAN), #2(SR_SPAN))
      in
        (UserCode.ConsDecl_PROD_1_ACT (SR_RES, NAME_RES, SR_SPAN : (Lex.pos * Lex.pos), NAME_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun TyParams_NT (strm) = let
      fun TyParams_PROD_1 (strm) = let
            val FULL_SPAN = (Err.getPos(strm), Err.getPos(strm))
            in
              (UserCode.TyParams_PROD_1_ACT (FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm)
            end
      fun TyParams_PROD_2 (strm) = let
            val (TYVAR_RES, TYVAR_SPAN, strm') = matchTYVAR(strm)
            val FULL_SPAN = (#1(TYVAR_SPAN), #2(TYVAR_SPAN))
            in
              (UserCode.TyParams_PROD_2_ACT (TYVAR_RES, TYVAR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun TyParams_PROD_3 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (TYVAR_RES, TYVAR_SPAN, strm') = matchTYVAR(strm')
            fun TyParams_PROD_3_SUBRULE_1_NT (strm) = let
                  val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                  val (TYVAR_RES, TYVAR_SPAN, strm') = matchTYVAR(strm')
                  val FULL_SPAN = (#1(COMMA_SPAN), #2(TYVAR_SPAN))
                  in
                    ((TYVAR_RES), FULL_SPAN, strm')
                  end
            fun TyParams_PROD_3_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(TyParams_PROD_3_SUBRULE_1_PRED, TyParams_PROD_3_SUBRULE_1_NT, strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.TyParams_PROD_3_ACT (LP_RES, RP_RES, SR_RES, TYVAR_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), TYVAR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LP, _, strm') => TyParams_PROD_3(strm)
          | (Tok.NAME(_), _, strm') => TyParams_PROD_1(strm)
          | (Tok.TYVAR(_), _, strm') => TyParams_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
fun TopDecl_NT (strm) = let
      fun TopDecl_PROD_1 (strm) = let
            val (KW_type_RES, KW_type_SPAN, strm') = matchKW_type(strm)
            val (TyParams_RES, TyParams_SPAN, strm') = TyParams_NT(strm')
            val (NAME_RES, NAME_SPAN, strm') = matchNAME(strm')
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
            val (Type_RES, Type_SPAN, strm') = Type_NT(strm')
            val FULL_SPAN = (#1(KW_type_SPAN), #2(Type_SPAN))
            in
              (UserCode.TopDecl_PROD_1_ACT (EQ_RES, NAME_RES, Type_RES, KW_type_RES, TyParams_RES, EQ_SPAN : (Lex.pos * Lex.pos), NAME_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), KW_type_SPAN : (Lex.pos * Lex.pos), TyParams_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun TopDecl_PROD_2 (strm) = let
            val (KW_datatype_RES, KW_datatype_SPAN, strm') = matchKW_datatype(strm)
            val (TyParams_RES, TyParams_SPAN, strm') = TyParams_NT(strm')
            val (NAME_RES, NAME_SPAN, strm') = matchNAME(strm')
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
            val (ConsDecl_RES, ConsDecl_SPAN, strm') = ConsDecl_NT(strm')
            fun TopDecl_PROD_2_SUBRULE_1_NT (strm) = let
                  val (BAR_RES, BAR_SPAN, strm') = matchBAR(strm)
                  val (ConsDecl_RES, ConsDecl_SPAN, strm') = ConsDecl_NT(strm')
                  val FULL_SPAN = (#1(BAR_SPAN), #2(ConsDecl_SPAN))
                  in
                    ((ConsDecl_RES), FULL_SPAN, strm')
                  end
            fun TopDecl_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.BAR, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(TopDecl_PROD_2_SUBRULE_1_PRED, TopDecl_PROD_2_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(KW_datatype_SPAN), #2(SR_SPAN))
            in
              (UserCode.TopDecl_PROD_2_ACT (EQ_RES, SR_RES, NAME_RES, KW_datatype_RES, TyParams_RES, ConsDecl_RES, EQ_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), NAME_SPAN : (Lex.pos * Lex.pos), KW_datatype_SPAN : (Lex.pos * Lex.pos), TyParams_SPAN : (Lex.pos * Lex.pos), ConsDecl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun TopDecl_PROD_3 (strm) = let
            val (ValueDecl_RES, ValueDecl_SPAN, strm') = ValueDecl_NT(strm)
            val FULL_SPAN = (#1(ValueDecl_SPAN), #2(ValueDecl_SPAN))
            in
              (UserCode.TopDecl_PROD_3_ACT (ValueDecl_RES, ValueDecl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_fun, _, strm') => TopDecl_PROD_3(strm)
          | (Tok.KW_val, _, strm') => TopDecl_PROD_3(strm)
          | (Tok.KW_type, _, strm') => TopDecl_PROD_1(strm)
          | (Tok.KW_datatype, _, strm') => TopDecl_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
fun Program_NT (strm) = let
      fun Program_PROD_1_SUBRULE_1_NT (strm) = let
            val (TopDecl_RES, TopDecl_SPAN, strm') = TopDecl_NT(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(TopDecl_SPAN), #2(SEMI_SPAN))
            in
              ((TopDecl_RES), FULL_SPAN, strm')
            end
      fun Program_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_datatype, _, strm') => true
              | (Tok.KW_fun, _, strm') => true
              | (Tok.KW_type, _, strm') => true
              | (Tok.KW_val, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(Program_PROD_1_SUBRULE_1_PRED, Program_PROD_1_SUBRULE_1_NT, strm)
      val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm')
      val FULL_SPAN = (#1(SR_SPAN), #2(Exp_SPAN))
      in
        (UserCode.Program_PROD_1_ACT (SR_RES, Exp_RES, SR_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
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
