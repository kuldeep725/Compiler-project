structure Translate =
struct
    
	structure A = Ast 
	structure J = JavaAst 

	exception TranslationError

	val cmps = [A.BinopCompare A.NE, A.BinopCompare A.EQ, A.BinopCompare A.GT, A.BinopCompare A.GTE, A.BinopCompare A.LT, A.BinopCompare A.LTE]
	val logical = [A.BinopLogical A.AND, A.BinopLogical A.OR]
	
	val isCompareOp = fn x => List.exists (fn y => y=x) cmps
	val isLogicalOp = fn x => List.exists (fn y => y=x) logical 

	fun transPrim (A.VOID) = J.VOID 
		| transPrim (A.INT) = J.INT 
		| transPrim (A.CHAR) = J.CHAR 
		| transPrim (A.DOUBLE) = J.DOUBLE 
		| transPrim (A.LONG) = J.LONG 
		| transPrim (A.FLOAT) = J.FLOAT 

	fun transComp (A.BinopCompare A.NE) = J.NE 
		| transComp (A.BinopCompare A.EQ) = J.EQ 
		| transComp (A.BinopCompare A.GT) = J.GT 
		| transComp (A.BinopCompare A.GTE) = J.GTE 
		| transComp (A.BinopCompare A.LT) = J.LT 
		| transComp (A.BinopCompare A.LTE) = J.LTE 
		| transComp _ = raise TranslationError
	
	fun transBitwise (A.BITAND) = J.BITAND 
		| transBitwise (A.BITOR) = J.BITOR 
		| transBitwise (A.XOR) = J.XOR 
		| transBitwise (A.LEFTSHIFT) = J.LEFTSHIFT
		| transBitwise (A.RIGHTSHIFT) = J.RIGHTSHIFT

	fun transLogical (A.BinopLogical A.AND) = J.AND 
		| transLogical (A.BinopLogical A.OR) = J.OR 
		| transLogical _ = raise TranslationError

	fun transOp (A.PLUS) = J.PLUS 
		| transOp (A.MINUS) = J.MINUS
		| transOp (A.MUL) = J.MUL 
		| transOp (A.DIV) = J.DIV 
		| transOp (A.MODULUS) = J.MODULUS
		| transOp (A.BinopBitwise x) = J.BinopBitwise(transBitwise x) 
		| transOp _ = raise TranslationError

	fun transIdValues (A.IdValuesVar id) = J.IdValuesVar id
		| transIdValues (A.IntVal x) = J.IntVal x 
		| transIdValues (A.FloatVal x) = J.FloatVal x 
		| transIdValues (A.CharVal x) = J.CharVal x 
		| transIdValues (A.StringVal x) = J.StringVal x

	fun transAssignment (A.ASSIGN) = J.ASSIGN
		| transAssignment (A.PLUSA) = J.PLUSA 
		| transAssignment (A.MINUSA) = J.MINUSA 
		| transAssignment (A.TIMESA) = J.TIMESA 
		| transAssignment (A.DIVA) = J.DIVA 
		| transAssignment (A.MODULUSA) = J.MODULUSA 
		| transAssignment (A.LEFTSHIFTA) = J.LEFTSHIFTA
		| transAssignment (A.RIGHTSHIFTA) = J.RIGHTSHIFTA
		| transAssignment (A.BITORA) = J.BITORA
		| transAssignment (A.BITANDA) = J.BITANDA 
		| transAssignment (A.XORA) = J.XORA 

	fun transIncDec (A.INC) = J.INC 
		| transIncDec (A.DEC) = J.DEC 

	fun transExps [] = [] 
		| transExps (x::xs) = (transExp x) :: (transExps xs)

	and transCondExp (A.IdValuesCons x) = J.BinComp (J.IdValuesCons (transIdValues x), J.NE, J.IdValuesCons (J.IntVal 0)) 
		| transCondExp (A.ExpOpCons x) = transCondExpOp x 
		| transCondExp (A.AssignCons (id, asn, e)) = J.BinComp (J.Paren(J.AssignCons(id,transAssignment asn,transExp e)), J.NE, J.IdValuesCons (J.IntVal 0))
		| transCondExp (A.TernaryOp (e1, e2, e3)) = J.BinComp (J.Paren(J.TernaryOp(transCondExp e1, transExp e2, transExp e3)), J.NE, J.IdValuesCons (J.IntVal 0))
		| transCondExp (A.Paren (e)) = J.CondParen (transCondExp e)
		| transCondExp (A.ExpFuncCall (A.FuncCallCons (id, x))) = J.BinComp (J.ExpFuncCall (J.FuncCallCons(id, transExps x)), J.NE, J.IdValuesCons (J.IntVal 0))

	and transCondExpOp (A.BinaryOp (e1, oper,e2)) = 
				if isCompareOp oper 
				then J.BinComp (transExp e1, transComp oper, transExp e2)
				else (
					if isLogicalOp oper 
					then J.BinLog (transCondExp e1, transLogical oper, transCondExp e2)
					else J.BinComp (J.ExpOpCons (J.BinaryOp (transExp e1, transOp oper, transExp e2)), J.NE, J.IdValuesCons (J.IntVal 0))
				)
		| transCondExpOp (A.IdIncDec (id, x)) = J.BinComp (J.ExpOpCons(J.IdIncDec (id, transIncDec x)), J.NE, J.IdValuesCons (J.IntVal 0))
		| transCondExpOp (A.IncDecId (id, x)) = J.BinComp (J.ExpOpCons(J.IncDecId (id, transIncDec x)), J.NE, J.IdValuesCons (J.IntVal 0)) 
		| transCondExpOp (A.UnaryOp (unop, e)) =  
					case unop of 
							A.NOT 	=> J.NotCons(J.CondParen(transCondExp e))
						| A.COMPLEMENT => J.BinComp (J.ExpOpCons(J.ComplementCons (transExp e)), J.NE, J.IdValuesCons (J.IntVal 0))

	and transExp (A.IdValuesCons x) = J.IdValuesCons (transIdValues x) 
		| transExp (A.ExpOpCons x) = (transExpOp x) 
		| transExp (A.AssignCons (id, asn, e)) = J.AssignCons (id, transAssignment asn, transExp e) 
		| transExp (A.TernaryOp (e1, e2, e3)) = J.TernaryOp (transCondExp e1, transExp e2, transExp e3)
		| transExp (A.Paren(e)) = J.Paren(transExp e) 
		| transExp (A.ExpFuncCall (A.FuncCallCons (id, x))) = J.ExpFuncCall(J.FuncCallCons (id, transExps x))

	and transExpOp (A.BinaryOp (e1, oper, e2)) = 
				if isCompareOp oper 
				then J.TernaryOp (J.BinComp (transExp e1, transComp oper, transExp e2), J.IdValuesCons (J.IntVal 1), J.IdValuesCons (J.IntVal 0))
				else (
					if isLogicalOp oper 
					then J.TernaryOp (J.BinLog (J.BinComp(transExp e1, J.NE, J.IdValuesCons (J.IntVal 0)), transLogical oper, J.BinComp(transExp e2, J.NE, J.IdValuesCons (J.IntVal 0))), J.IdValuesCons (J.IntVal 1), J.IdValuesCons (J.IntVal 0))
					else J.ExpOpCons (J.BinaryOp (transExp e1, transOp oper, transExp e2))
				) 
		| transExpOp (A.IdIncDec (id, x)) = J.ExpOpCons (J.IdIncDec(id, transIncDec x))
		| transExpOp (A.IncDecId (id, x)) = J.ExpOpCons (J.IncDecId(id, transIncDec x))
		| transExpOp (A.UnaryOp (unop, e)) = 
				if unop = A.COMPLEMENT 
				then J.ExpOpCons(J.ComplementCons (transExp e))
				else J.TernaryOp(J.NotCons (transCondExp e), J.IdValuesCons (J.IntVal 1), J.IdValuesCons (J.IntVal 0))

	fun transDec (A.Variable id) = J.Variable id 
		| transDec (A.DecVar(id, e)) = J.DecVar (id, transExp e)

	fun transDecs [] = [] 
		| transDecs (x::xs) = (transDec x) :: transDecs xs 

	fun transParams [] = [] 
		| transParams ((A.ParamsCons(prim, dec))::xs) = (J.ParamsCons(transPrim prim, transDec dec)) :: transParams xs

	fun transReturnStmt (A.RETURN) = J.RETURN 
		| transReturnStmt (A.ReturnCons e) = J.ReturnCons (transExp e) 

	fun transComment (A.SingleComment x) = (J.SingleComment x) 
		| transComment (A.MultiComment x) = (J.MultiComment x) 

	fun transStatements [] = []
		| transStatements (x::xs) = (transStatement x) :: transStatements xs 

	and transStatement (A.IF(e, x)) = J.IF(transCondExp e, transStatement x) 
		| transStatement (A.IFELSE(e, x1, x2)) = J.IFELSE (transCondExp e, transStatement x1, transStatement x2) 
		| transStatement (A.StmtDecStmts(A.DecStmtCons(prim, decs))) = J.StmtDecStmts(transPrim prim, transDecs decs)
		| transStatement (A.ReturnStmtCons(x)) = J.ReturnStmtCons (transReturnStmt x) 
		| transStatement (A.BREAK) = J.BREAK 
		| transStatement (A.CONTINUE) = J.CONTINUE 
		| transStatement (A.Block sList) = J.Block (transStatements sList)
		| transStatement (A.While(e,x)) = J.While (transCondExp e, transStatement x) 
		| transStatement (A.StmtCommentCons x) = J.StmtCommentCons (transComment x) 
		| transStatement (A.ExpCons e) = J.ExpCons (transExp e) 

	fun translate [] = [] 
		| translate (x::xs) = 
				case x of 
						(A.ProgItemDirectives _) => translate xs 
					| (A.ProgItemComments _) => translate xs 
					| (A.FuncCons (prim, id, pList, sList)) => (J.FuncStaticAMCons (J.PUBLIC, transPrim prim, id, transParams pList, transStatement sList)) :: translate xs 
					| (A.DecStmtsCons (A.DecStmtCons(prim, dec))) => (J.ProgStaticDecStmts(J.PRIVATE, transPrim prim, transDecs dec)) :: translate xs 

end