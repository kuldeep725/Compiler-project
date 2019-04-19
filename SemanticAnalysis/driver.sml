structure Driver =
struct
  
  structure ExprLrVals = ExprLrValsFun(structure Token = LrParser.Token) (* Generate the LR values structure *)
	structure ExprLex    = CLexFun(structure Tokens = ExprLrVals.Tokens)
	structure ExprParser = Join( structure ParserData = ExprLrVals.ParserData
			     structure Lex        = ExprLex
			     structure LrParser   = LrParser
			   )

  	(* to print error line number during parsing *)
	fun print_error (s,i:int,_) = TextIO.output(TextIO.stdErr,
					    "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")

  fun parse filename = let 
		val file = TextIO.openIn filename
		fun get _ = TextIO.input file
		val lexer = ExprParser.makeLexer get
		val (ast,_) = ExprParser.parse (0, lexer, print_error,())
	in 
		ast
	end
        

	fun read []         = print ("\n")
		| read (x :: xs)  = 
          let 
            val ast = parse x
          in 
            (print("\n\n=============== OPENING " ^ x ^ " ================\n\n");
            Semant.semant ast;
            print("Semantic Analysis done successfully...");
            read xs) 
          end 

	fun readFile xs = if xs = [] 
										then read ["test1.c"]
										else read xs

	val _ = readFile (CommandLine.arguments ())

end