(* Pretty printing *)
structure Driver = struct
  
  open Symbol

  structure J = JavaAst 
  structure Tr = Translate 

  structure ExprLrVals = ExprLrValsFun(structure Token = LrParser.Token) (* Generate the LR values structure *)
	structure ExprLex    = CLexFun(structure Tokens = ExprLrVals.Tokens)
	structure ExprParser = Join( structure ParserData = ExprLrVals.ParserData
			     structure Lex        = ExprLex
			     structure LrParser   = LrParser
			   )

  	(* to print error line number during parsing *)
	fun print_error (s,i:int,_) = TextIO.output(TextIO.stdErr,
					    "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")

  val NEWLINE  = "\n"
	val SPACE = " "
	val SCOLON = ";"
	val COMMA = ","
  val STATIC = "static"
  val LPAREN = "("
  val RPAREN = ")"

  val isMain = ref false 

	(* function to indent *)
	fun indent 0 = ""
    | indent i =  "   " ^ indent(i-1)

  fun compileAM (J.PUBLIC) = "public"
    | compileAM (J.PRIVATE) = "private"
    | compileAM (J.PROTECTED) = "protected"
    | compileAM (J.PACKAGE) = "package"

  fun compileIdValues (J.IdValuesVar id) = (name id)
		| compileIdValues (J.IntVal x) = Int.toString x
		| compileIdValues (J.FloatVal x) = Real.toString x
		| compileIdValues (J.CharVal x) = "'" ^ Char.toString x ^ "'"
		| compileIdValues (J.StringVal x) = "\"" ^ x ^ "\""

	fun compilePrim (J.INT) = "int"
		| compilePrim (J.VOID) = "void"
		| compilePrim (J.CHAR) = "char"
		| compilePrim (J.FLOAT) = "float"
		| compilePrim (J.DOUBLE) = "double"
		| compilePrim (J.LONG) = "long"
    | compilePrim (J.BOOL) = "boolean"

	fun compilePrims [] = ""
		| compilePrims (x :: xs) = (compilePrim x) ^ (if List.null xs then "" else COMMA ^ " ") ^ compilePrims xs 

  	fun compileCompare J.NE = "!="
		| compileCompare J.EQ = "=="
		| compileCompare J.GT = ">"
		| compileCompare J.GTE = ">="
		| compileCompare J.LT = "<"
		| compileCompare J.LTE = "<="

	fun compileBitwise J.BITAND = "&"
		| compileBitwise J.BITOR = "|"
		| compileBitwise J.XOR = "^"
		| compileBitwise J.LEFTSHIFT = "<<"
		| compileBitwise J.RIGHTSHIFT = ">>"

	fun compileLogical J.AND = "&&"
		| compileLogical J.OR = "||"

	fun compileAssignment J.ASSIGN = "="
		| compileAssignment J.PLUSA = "+="
		| compileAssignment J.MINUSA = "-="
		| compileAssignment J.TIMESA = "*="
		| compileAssignment J.DIVA = "/="
		| compileAssignment J.MODULUSA = "%="
		| compileAssignment J.LEFTSHIFTA = "<<="
		| compileAssignment J.RIGHTSHIFTA = ">>="
		| compileAssignment J.BITORA = "|="
		| compileAssignment J.BITANDA = "&="
		| compileAssignment J.XORA = "^="

	fun compileIncDec J.INC = "++"
		| compileIncDec J.DEC = "--"

  fun compileComment (J.SingleComment _) = ""
		| compileComment (J.MultiComment _) = ""

	fun compileBinop J.PLUS = "+"
		| compileBinop J.MINUS = "-"
		| compileBinop J.MUL = "*"
		| compileBinop J.DIV = "/"
		| compileBinop J.MODULUS = "%"
		| compileBinop (J.BinopBitwise bit) = compileBitwise bit 

  fun compileCondAsn J.CASSIGN = "="
    | compileCondAsn J.CBITORA = "|="
    | compileCondAsn J.CBITANDA = "&="
    | compileCondAsn J.CXORA = "^="

  fun compileCondExp (J.BinComp(e1, cmp, e2)) = (compileExp e1) ^ SPACE ^ (compileCompare cmp) ^ SPACE ^ (compileExp e2) 
    | compileCondExp (J.BinLog(e1,log,e2)) = (compileCondExp e1) ^ SPACE ^ (compileLogical log) ^ SPACE ^ (compileCondExp e2) 
    | compileCondExp (J.NotCons(e)) = "!" ^ (compileCondExp e) 
    | compileCondExp (J.TRUE) = "true"
    | compileCondExp (J.FALSE) = "false"
    | compileCondExp (J.BoolId id) = name id 
    | compileCondExp (J.CondParen x) = LPAREN ^ (compileCondExp x) ^ RPAREN 

  and compileExp (J.IdValuesCons x) = compileIdValues x
    | compileExp (J.ExpOpCons x) = compileExpOp x 
    | compileExp (J.AssignCons(id, asn, e)) = (name id) ^ SPACE ^ (compileAssignment asn) ^ SPACE ^ (compileExp e)
    | compileExp (J.AssignCond(id, asn, e)) = (name id)^ SPACE ^ (compileCondAsn asn) ^ SPACE ^ (compileCondExp e) 
    | compileExp (J.TernaryOp(e, e1, e2)) = (compileCondExp e) ^ " ? " ^ (compileExp e1) ^ " : " ^ (compileExp e2)
    | compileExp (J.Paren(e)) = LPAREN ^ (compileExp e) ^ RPAREN 
    | compileExp (J.ExpFuncCall (J.FuncCallCons(id, es))) = (if name id = "printf" then "System.out.printf" else (if name id = "toupper" then "Character.toUpperCase" else name id)) ^
                                                               LPAREN ^ (compileExps es) ^ RPAREN 

  and compileExps [] = ""
    | compileExps (x::xs) = (compileExp x) ^ (if List.null xs then "" else COMMA ^ " ") ^ compileExps xs 

  and compileExpOp (J.BinaryOp(e1, oper, e2)) = (compileExp e1) ^ SPACE ^ (compileBinop oper) ^ SPACE ^ (compileExp e2) 
    | compileExpOp (J.IdIncDec(id, x)) = (name id) ^ (compileIncDec x)
    | compileExpOp (J.IncDecId(id, x)) = (compileIncDec x) ^ (name id)
    | compileExpOp (J.ComplementCons e) = "~" ^ (compileExp e) 


  fun compileDec (J.Variable id) = name id 
  | compileDec (J.DecVar (id, e)) = (name id) ^ " = " ^ (compileExp e)  

	fun compileDecs [] = ""
		| compileDecs (x :: xs) = (compileDec x) ^ (if List.null xs then SCOLON else COMMA ^ " ") ^ compileDecs xs 

  fun compileParam (J.ParamsCons (prim, dec)) = (compilePrim prim) ^ SPACE ^ (compileDec dec) 

	fun compileParams [] = ""
		| compileParams (x :: xs) = (compileParam x) ^ (if List.null xs then "" else COMMA ^ " ") ^ compileParams xs 

	fun compileReturnStmt (J.RETURN) d = (indent d) ^ "return;"
		| compileReturnStmt (J.ReturnCons e) d = (indent d) ^ "return " ^ (compileExp e) ^ SCOLON 
 

  fun compileStatement (J.IF(e,x)) d = (indent d) ^ "if" ^ (compileCondExp e) ^ SPACE ^ (compileStatement x d)
    | compileStatement (J.IFELSE (e, x1, x2)) d = (indent d) ^ "if " ^ (compileCondExp e) ^ SPACE ^ (compileStatement x1 d) 
																								^ (indent d) ^ "else " ^ (compileStatement x2 d)
		| compileStatement (J.StmtDecStmts (prim, decList)) d = (indent d) ^ (compilePrim prim) ^ SPACE ^ (compileDecs decList) ^ NEWLINE 
		| compileStatement (J.ReturnStmtCons x) d = (if !isMain then "" else compileReturnStmt x d) ^ NEWLINE 
		| compileStatement (J.BREAK) d = (indent d) ^ "break;\n"
		| compileStatement (J.CONTINUE) d = (indent d) ^ "continue;\n"
		| compileStatement (J.Block sList) d = "{\n" ^ (compileStatements sList (d+1)) ^ (indent d) ^ "}" ^ NEWLINE 
		| compileStatement (J.While (e, x)) d = (indent d) ^ "while " ^ (compileCondExp e) ^ SPACE ^ (compileStatement x d)
		| compileStatement (J.StmtCommentCons x) d = ""
		| compileStatement (J.ExpCons e) d = (indent d) ^ (compileExp e) ^ SCOLON ^ NEWLINE

  and compileStatements [] _ = "" 
    | compileStatements (x::xs) d = (compileStatement x d) ^ compileStatements xs d


  fun compileProgItem (J.ProgStaticDecStmts (am, prim, decs)) = (indent 1) ^ (compileAM am) ^ SPACE ^ STATIC ^ SPACE ^ (compilePrim prim) ^ SPACE 
                                                ^ (compileDecs decs) ^ NEWLINE 
    | compileProgItem (J.ProgDecStmts(am, prim, decs)) =  (indent 1) ^ (compileAM am) ^ SPACE ^ (compilePrim prim) ^ SPACE ^ (compileDecs decs) ^ NEWLINE 
    | compileProgItem (J.FuncAMCons(am, prim, id, plist, stmt)) =  (indent 1) ^ (compileAM am) ^ SPACE ^ (compilePrim prim) ^ SPACE ^ (name id) ^ SPACE   
                                                ^ LPAREN ^ (compileParams plist) ^ RPAREN ^ (compileStatement stmt 1) 
    | compileProgItem (J.FuncStaticAMCons(am, prim, id, plist, stmt)) = 
    ( 
      if name id = "main" then isMain := true
      else isMain := false;
      let 
        val s = (indent 1) ^ (compileAM am) ^ SPACE ^ STATIC ^ SPACE ^ (if !isMain then "void" else compilePrim prim) ^ SPACE ^ (name id) ^ SPACE  
                                                ^ LPAREN ^ (if !isMain then "String[] args" else compileParams plist) ^ RPAREN ^ SPACE ^ (compileStatement stmt 1)
      in 
        (isMain := false; s)
      end 
    )
    | compileProgItem (J.FuncStaticCons(prim, id, plist, stmt)) = (indent 1) ^ STATIC ^ SPACE ^ (compilePrim prim) ^ SPACE ^ (name id) ^ SPACE  
                                                ^ LPAREN ^ (compileParams plist) ^ RPAREN ^ (compileStatement stmt 1) 
    | compileProgItem (J.FuncCons(prim, id, plist, stmt)) = (indent 1) ^ (compilePrim prim) ^ SPACE ^ (name id) ^ SPACE 
                                                ^ LPAREN ^ (compileParams plist) ^ RPAREN ^ (compileStatement stmt 1) 

  
    
  fun compiles [] = ""
    | compiles (x::xs) = (compileProgItem x) ^ compiles xs 

  fun compileAst ast classname = 
          let 
            val imports = "import java.util.*;\n\n"
            val startText = "public class " ^ classname ^ " {\n\n"
            val endText = "\n}"
          in 
            imports ^ startText ^ (compiles ast) ^ endText
          end 

  fun parse filename = 
          let 
            val file = TextIO.openIn ("Tests/" ^ filename)
            fun get _ = TextIO.input file
            val lexer = ExprParser.makeLexer get
            val (ast,_) = ExprParser.parse (0, lexer, print_error,())
          in 
            ast
          end  

	fun read []         = print ("\n")
		| read (x :: xs)  = 
          let
            val oldfile = hd (String.tokens (fn y => y = #".") x)
            val firstChar = String.sub(oldfile, 0)
            val classname = str(Char.toUpper firstChar) ^ (substring (oldfile, 1,(size oldfile)-1))
            val newfile = classname ^ ".java"
            val ast = parse x
            val _ = Semant.semant ast 
            val javaAst = Tr.translate ast 
            val output = compileAst javaAst classname 
            fun writeFile filename content =
                  let val fd = TextIO.openOut ("Outputs/" ^ filename)
                      val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
                      val _ = TextIO.closeOut fd
                  in () end
          in 
            (print("\n\n=============== TARGET " ^ newfile ^ " ================\n\n");
             print(output);
             writeFile newfile output;
             read xs) 
          end 

	fun readFile xs = if xs = [] 
										then read ["test1.c"]
										else read xs

	val _ = readFile (CommandLine.arguments ())

end