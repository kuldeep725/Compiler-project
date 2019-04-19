structure Parse = struct 

    val NORMAL = 0
    val COMMENT_CONST = 1
    val KEYWORD_CONST = 2
    val STRING_CONST = 3
    val VALUES_CONST = 4
    val EOF_CONST = 5
    val COMMENT_STR = "\027[33m"
    val KEYWORD_STR = "\027[1;34m"
    val STRING_STR  = "\027[31m"
    val VALUES_STR  = "\027[35m"
    val FORMAT_END_STR = "\027[0m"


	(* This three structure definitions are what the lexer and parser *)

	structure ExprLrVals = ExprLrValsFun(structure Token = LrParser.Token) (* Generate the LR values structure *)
	structure ExprLex    = CLexFun(structure Tokens = ExprLrVals.Tokens)
	structure ExprParser = Join( structure ParserData = ExprLrVals.ParserData
			     structure Lex        = ExprLex
			     structure LrParser   = LrParser
			   )

	(* to print error line number during parsing *)
	fun print_error (s,i:int,_) = TextIO.output(TextIO.stdErr,
					    "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")

	(* to convert given list to string with delimiter <delim> *)
	fun listToString delim xs = String.concatWith delim xs;

	fun getIndent d flag = if flag then d else 0

	val newline  = ["\n"]
	val space = [" "]
	val scolon = [";"]
	val comma = [","]

	(* function to indent *)
	fun indent 0 = []
    | indent i =  "   " :: indent(i-1)


	fun compileId (Ast.Id s)   	        = [s] 

	fun compileIdValues (Ast.IdValuesVar id) = (compileId id)
		| compileIdValues (Ast.IntVal x) = [Int.toString x]
		| compileIdValues (Ast.FloatVal x) = [Real.toString x]
		| compileIdValues (Ast.CharVal x) = ["'" ^ Char.toString x ^ "'"]
		| compileIdValues (Ast.StringVal x) = [x]

	fun compilePrim (Ast.INT) = ["int"]
		| compilePrim (Ast.VOID) = ["void"]
		| compilePrim (Ast.CHAR) = ["char"]
		| compilePrim (Ast.FLOAT) = ["float"]
		| compilePrim (Ast.DOUBLE) = ["double"]
		| compilePrim (Ast.LONG) = ["long"]

	fun compilePrims [] = [] 
		| compilePrims (x :: xs) = (compilePrim x) @ (if List.null xs then [""] else comma @ [" "]) @ compilePrims xs 

	fun compileDirectives (Ast.Include s) = ["#include <" ^ s ^ ">"]
		| compileDirectives (Ast.Includeh s) = ["#include \"" ^ s ^ "\""]
		| compileDirectives (Ast.Define (id, t)) = ["#define "] @ (compileId id) @ space @ (compileIdValues t)

	fun compileCompare Ast.NE = ["!="]
		| compileCompare Ast.EQ = ["=="]
		| compileCompare Ast.GT = [">"]
		| compileCompare Ast.GTE = [">="]
		| compileCompare Ast.LT = ["<"]
		| compileCompare Ast.LTE = ["<="]

	fun compileBitwise Ast.BITAND = ["&"]
		| compileBitwise Ast.BITOR = ["|"]
		| compileBitwise Ast.XOR = ["^"]
		| compileBitwise Ast.LEFTSHIFT = ["<<"]
		| compileBitwise Ast.RIGHTSHIFT = [">>"]

	fun compileLogical Ast.AND = ["&&"]
		| compileLogical Ast.OR = ["||"]

	fun compileAssignment Ast.ASSIGN = ["="]
		| compileAssignment Ast.PLUSA = ["+="]
		| compileAssignment Ast.MINUSA = ["-="]
		| compileAssignment Ast.TIMESA = ["*="]
		| compileAssignment Ast.DIVA = ["/="]
		| compileAssignment Ast.MODULUSA = ["%="]
		| compileAssignment Ast.LEFTSHIFTA = ["<<="]
		| compileAssignment Ast.RIGHTSHIFTA = [">>="]
		| compileAssignment Ast.BITORA = ["|="]
		| compileAssignment Ast.BITANDA = ["&="]
		| compileAssignment Ast.XORA = ["^="]

	fun compileIncDec Ast.INC = ["++"]
		| compileIncDec Ast.DEC = ["--"]

	fun compileUnop Ast.NOT = ["!"]
		| compileUnop Ast.COMPLEMENT = ["~"]

	fun compileComment (Ast.SingleComment x) = [x]
		| compileComment (Ast.MultiComment x) = [x] 

	fun compileBinop Ast.PLUS = ["+"]
		| compileBinop Ast.MINUS = ["-"]
		| compileBinop Ast.MUL = ["*"]
		| compileBinop Ast.DIV = ["/"]
		| compileBinop Ast.MODULUS = ["%"]
		| compileBinop (Ast.BinopCompare cmp) = compileCompare cmp 
		| compileBinop (Ast.BinopBitwise bit) = compileBitwise bit 
		| compileBinop (Ast.BinopLogical lgc) = compileLogical lgc 

	fun compileExps (x :: xs) = (compileExp x) @ (if List.null xs then [] else comma @ [" "]) @ compileExps xs 
		| compileExps _ = []

	and compileExpOp (Ast.BinaryOp (e1, oper, e2)) = (compileExp e1) @ space @ (compileBinop oper) @ space 
																									@ (compileExp e2) 
		| compileExpOp (Ast.IdIncDec (id, x)) = (compileId id) @ compileIncDec x
		| compileExpOp (Ast.IncDecId (id, x)) = (compileIncDec x) @ compileId id
		| compileExpOp (Ast.UnaryOp (unop, e)) = (compileUnop unop) @ compileExp e
	
	and compileExp (Ast.IdValuesCons x) = (compileIdValues x) 
		| compileExp (Ast.ExpOpCons x) = (compileExpOp x)
		| compileExp (Ast.AssignCons (id, asn, e)) = (compileId id) @ space @ (compileAssignment asn) @ space 
																							@ (compileExp e) 
		| compileExp (Ast.TernaryOp (e1, e2, e3)) = (compileExp e1) @ [" ? "] @ (compileExp e2) @ [" : "] 
																							@ (compileExp e3) 
		| compileExp (Ast.Paren (e)) = ["("] @ (compileExp e) @ [")"]
		| compileExp (Ast.ExpFuncCall (Ast.FuncCallCons (id, x))) = (compileId id) @ ["("] @ (compileExps x) @ [")"] 

	fun compileDec (Ast.Variable id) = compileId id 
	  | compileDec (Ast.DecVar (id, e)) = (compileId id) @ [" = "] @ (compileExp e)

	fun compileDecs [] = [] 
		| compileDecs (x :: xs) = (compileDec x) @ (if List.null xs then scolon else comma @ [" "]) @ compileDecs xs 

	fun compileDecStmt (Ast.DecStmtCons (prim, decList)) d = (indent d) @ (compilePrim prim) @ space @ compileDecs decList

	fun compileParam (Ast.ParamsCons (prim, dec)) = (compilePrim prim) @ space @ (compileDec dec) 

	fun compileParams [] = [] 
		| compileParams (x :: xs) = (compileParam x) @ (if List.null xs then [""] else comma @ [" "]) @ compileParams xs 

	fun compileReturnStmt (Ast.RETURN) d = (indent d) @ ["return;"]
		| compileReturnStmt (Ast.ReturnCons e) d = (indent d) @ ["return "] @ (compileExp e) @ scolon

	fun compileStatement (Ast.IF (e, x)) d = (indent d) @ ["if"] @ (compileExp e) @ space @ (compileStatement x d) 
		| compileStatement (Ast.IFELSE (e, x1, x2)) d = (indent d) @ ["if "] @ (compileExp e) @ space @ (compileStatement x1 d) 
																								@ (indent d) @ ["else "] @ (compileStatement x2 d)
		| compileStatement (Ast.StmtDecStmts x) d = (compileDecStmt x d) @ newline 
		| compileStatement (Ast.ReturnStmtCons x) d = (compileReturnStmt x d) @ newline 
		| compileStatement (Ast.BREAK) d = (indent d) @ ["break;\n"]
		| compileStatement (Ast.CONTINUE) d = (indent d) @ ["continue;\n"]
		| compileStatement (Ast.Block sList) d = ["{\n"] @ (compileStatements sList (d+1)) @ (indent d) @ ["}"] @ newline 
		| compileStatement (Ast.While (e, x)) d = (indent d) @ ["while "] @ (compileExp e) @ space @ (compileStatement x d)
		| compileStatement (Ast.StmtCommentCons x) d = (indent d) @ (compileComment x)
		| compileStatement (Ast.ExpCons e) d = (indent d) @ (compileExp e) @ scolon @ newline

	and compileStatements [] _ = [] 
		| compileStatements (x :: xs) d = (compileStatement x d) @ compileStatements xs d 

	fun compileProgItem (Ast.ProgItemDirectives x) = (compileDirectives x) @ newline
		| compileProgItem (Ast.DecStmtsCons x) = (compileDecStmt x 0) @ newline 
		| compileProgItem (Ast.FuncCons (prim, id, pList, sList)) = newline @ (compilePrim prim) @ space @ (compileId id)
																					@ ["("] @ (compileParams pList) @ [")"] @ space @ (compileStatement sList 0) 
		| compileProgItem (Ast.ProgItemComments x) = newline @ (compileComment x) 



	fun compiles [] 				= []
	  | compiles (x :: xs) 	= (compileProgItem x) @ compiles xs

  fun parse filename = let 
		val file = TextIO.openIn filename
		fun get _ = TextIO.input file
		val lexer = ExprParser.makeLexer get
		val (program,_) = ExprParser.parse (0, lexer, print_error,())
		val output = compiles program
	in 
		print (listToString "" (output))
	end
        

	fun read []        = print ("\n")
		| read (x :: xs)   = (print("\n\n=============== OPENING " ^ x ^ " ================\n\n");
													parse x; read xs)

	fun readFile xs = if xs = [] 
										then read ["test1.c"]
										else read xs

	val _ = readFile (CommandLine.arguments ())

end