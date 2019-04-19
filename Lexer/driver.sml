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
    fun parse filename = 
        let 
            val file = TextIO.openIn filename
            fun get _ = TextIO.input file
            val lexer = CLex.makeLexer get
            fun go () = 
                let 
                    val t = lexer()
                    fun eat (Tokens.MULTICOMMENT (s, _, _)) = (s, COMMENT_CONST)
                    | eat (Tokens.SINGLECOMMENT (s, _, _)) = (s, COMMENT_CONST)
                    (* KEYWORDS *)
                    | eat (Tokens.BREAK (_, _)) = ("break", KEYWORD_CONST)
                    | eat (Tokens.CASE  (_, _)) = ("case", KEYWORD_CONST)
                    | eat (Tokens.CHAR (_, _)) = ("char", KEYWORD_CONST)
                    | eat (Tokens.CONST (_, _)) = ("const", KEYWORD_CONST)
                    | eat (Tokens.CONTINUE (_, _)) = ("continue", KEYWORD_CONST)
                    | eat (Tokens.DEFAULT (_, _)) = ("default", KEYWORD_CONST)
                    | eat (Tokens.DO (_, _)) = ("do", KEYWORD_CONST)
                    | eat (Tokens.DOUBLE (_, _)) = ("double", KEYWORD_CONST)
                    | eat (Tokens.ELSE (_, _)) = ("else", KEYWORD_CONST)
                    | eat (Tokens.FLOAT (_, _)) = ("float", KEYWORD_CONST)
                    | eat (Tokens.FOR (_, _)) = ("for", KEYWORD_CONST)
                    | eat (Tokens.IF (_, _)) = ("if", KEYWORD_CONST)
                    | eat (Tokens.INT (_, _)) = ("int", KEYWORD_CONST)
                    | eat (Tokens.LONG (_, _)) = ("long", KEYWORD_CONST)
                    | eat (Tokens.REGISTER (_, _)) = ("register", KEYWORD_CONST)
                    | eat (Tokens.RETURN (_, _)) = ("return", KEYWORD_CONST)
                    | eat (Tokens.SHORT (_, _)) = ("short", KEYWORD_CONST)
                    | eat (Tokens.SIGNED (_, _)) = ("signed", KEYWORD_CONST)
                    | eat (Tokens.SIZEOF (_, _)) = ("sizeof", KEYWORD_CONST)
                    | eat (Tokens.STATIC (_, _)) = ("static", KEYWORD_CONST)
                    | eat (Tokens.STRUCT (_, _)) = ("struct", KEYWORD_CONST)
                    | eat (Tokens.SWITCH (_, _)) = ("switch", KEYWORD_CONST)
                    | eat (Tokens.TYPEDEF (_, _)) = ("typedef", KEYWORD_CONST)
                    | eat (Tokens.UNSIGNED (_, _)) = ("unsigned", KEYWORD_CONST)
                    | eat (Tokens.VOID (_, _)) = ("void", KEYWORD_CONST)
                    | eat (Tokens.WHILE (_, _)) = ("while", KEYWORD_CONST)
                    | eat (Tokens.NULL (_, _)) = ("NULL", KEYWORD_CONST)
                    | eat (Tokens.PRINTF (_, _)) = ("printf", KEYWORD_CONST)
                    (* directives *)
                    | eat (Tokens.INCLUDE (s, _, _)) = (KEYWORD_STR ^ "#include " ^ STRING_STR ^ "<" ^ s ^ 
                                                        ">" ^ FORMAT_END_STR, NORMAL)
                    | eat (Tokens.INCLUDEH (s, _, _)) = (KEYWORD_STR ^ "#include " ^ STRING_STR ^ "\"" ^ s ^ 
                                                        "\"" ^ FORMAT_END_STR, NORMAL)
                    | eat (Tokens.DEFINE (_, _)) = ("#define", KEYWORD_CONST)
                    (* values and identifier *)
                    | eat (Tokens.INTVAL (s, _, _)) = (Int.toString s, VALUES_CONST)
                    | eat (Tokens.FLOATVAL (s, _, _)) = (Real.toString s, VALUES_CONST)
                    | eat (Tokens.CHARVAL (s, _, _)) = ("'" ^ Char.toString s ^ "'", VALUES_CONST)
                    | eat (Tokens.ID (s, _, _)) = (s, NORMAL)
                    (* seperators *)
                    | eat (Tokens.LPAREN (_, _)) = ("(", NORMAL)
                    | eat (Tokens.RPAREN (_, _)) = (")", NORMAL)
                    | eat (Tokens.SEMICOLON (_, _)) = (";", NORMAL)
                    | eat (Tokens.COMMA (_, _)) = (",", NORMAL)
                    | eat (Tokens.LBRACE (_, _)) = ("{", NORMAL)
                    | eat (Tokens.RBRACE (_, _)) = ("}", NORMAL)
                    | eat (Tokens.LBRACKET (_, _)) = ("[", NORMAL)
                    | eat (Tokens.RBRACKET (_, _)) = ("]", NORMAL)
                    | eat (Tokens.QMARK (_, _)) = ("?", NORMAL)
                    | eat (Tokens.COLON (_, _)) = (":", NORMAL)
                    | eat (Tokens.DOT (_, _)) = (".", NORMAL)
                    (* operators *)
                    | eat (Tokens.PLUS (_, _)) = ("+", NORMAL)
                    | eat (Tokens.MINUS (_, _)) = ("-", NORMAL)
                    | eat (Tokens.MUL (_, _)) = ("*", NORMAL)
                    | eat (Tokens.DIV (_, _)) = ("/", NORMAL)
                    | eat (Tokens.MODULUS (_, _)) = ("%", NORMAL)
                    | eat (Tokens.INC (_, _)) = ("++", NORMAL)
                    | eat (Tokens.DEC (_, _)) = ("--", NORMAL)
                    (* comparisions *)
                    | eat (Tokens.NE (_, _)) = ("!=", NORMAL)
                    | eat (Tokens.EQ (_, _)) = ("==", NORMAL)
                    | eat (Tokens.GT (_, _)) = (">", NORMAL)
                    | eat (Tokens.GTE (_, _)) = (">=", NORMAL)
                    | eat (Tokens.LT (_, _)) = ("<", NORMAL)
                    | eat (Tokens.LTE (_, _)) = ("<=", NORMAL)
                    (* logical *)
                    | eat (Tokens.AND (_, _)) = ("and", NORMAL)
                    | eat (Tokens.OR (_, _)) = ("or", NORMAL)
                    | eat (Tokens.NOT (_, _)) = ("not", NORMAL)
                    (* bitwise *)
                    | eat (Tokens.BITAND (_, _)) = ("&", NORMAL)
                    | eat (Tokens.BITOR (_, _)) = ("|", NORMAL)
                    | eat (Tokens.XOR (_, _)) = ("^", NORMAL)
                    | eat (Tokens.COMPLEMENT (_, _)) = ("~", NORMAL)
                    | eat (Tokens.LEFTSHIFT (_, _)) = ("<<", NORMAL)
                    | eat (Tokens.RIGHTSHIFT (_, _)) = (">>", NORMAL)
                    (* assignment *)
                    | eat (Tokens.ASSIGN (_, _)) = ("=", NORMAL)
                    | eat (Tokens.PLUSA (_, _)) = ("+=", NORMAL)
                    | eat (Tokens.MINUSA (_, _)) = ("-=", NORMAL)
                    | eat (Tokens.TIMESA (_, _)) = ("*=", NORMAL)
                    | eat (Tokens.DIVA (_, _)) = ("/=", NORMAL)
                    | eat (Tokens.MODULUSA (_, _)) = ("%=", NORMAL)
                    | eat (Tokens.LEFTSHIFTA (_, _)) = ("<<=", NORMAL)
                    | eat (Tokens.RIGHTSHIFTA (_, _)) = (">>=", NORMAL)
                    | eat (Tokens.BITORA (_, _)) = ("|=", NORMAL)
                    | eat (Tokens.BITANDA (_, _)) = ("&=", NORMAL)
                    | eat (Tokens.XORA (_, _)) = ("^=", NORMAL)
                    (* string *)
                    | eat (Tokens.STRING (s, _, _)) = (s, STRING_CONST)
                    (* spaces *)
                    | eat (Tokens.SPACE (_, _)) = (" ", NORMAL)
                    | eat (Tokens.TAB (_, _))   = ("\t", NORMAL)
                    | eat (Tokens.NEWLINE (_, _)) = ("\n", NORMAL)
                    (* EOF *)
                    | eat _ = ("EOF", EOF_CONST)
                    val (s, color) = eat t 
                in 
                    if color = COMMENT_CONST
                    then (print (COMMENT_STR ^ s ^ FORMAT_END_STR); go())
                    else if color = KEYWORD_CONST
                    then (print (KEYWORD_STR ^ s ^ FORMAT_END_STR); go())
                    else if color = STRING_CONST
                    then (print (STRING_STR ^ s ^ FORMAT_END_STR); go())
                    else if color = VALUES_CONST
                    then (print (VALUES_STR ^ s ^ FORMAT_END_STR); go())
                    else if color = NORMAL 
                    then (print (s); go())
                    else ()

                end
        in 
            go ();
            TextIO.closeIn file
        end

    fun read []        = print ("\n")
      | read (x :: xs)   = (print("\n\n=============== OPENING " ^ x ^ " ================\n\n");
                            parse x; read xs)

    fun readFile xs = if xs = [] 
                      then read ["test1.c"]
                      else read xs

    val _ = readFile (CommandLine.arguments ())

end