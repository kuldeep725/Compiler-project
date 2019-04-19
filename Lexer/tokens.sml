structure Tokens = struct 

    type pos = int
    datatype token = EOF of pos * pos
                    | MULTICOMMENT of string * pos * pos
                    | SINGLECOMMENT of string * pos * pos
                    (* keywords *)
                    | BREAK of pos * pos
                    | CASE of pos * pos
                    | CHAR of pos * pos
                    | CONST of pos * pos
                    | CONTINUE of pos * pos
                    | DEFAULT of pos * pos
                    | DO of pos * pos
                    | DOUBLE of pos * pos
                    | ELSE of pos * pos
                    | FLOAT of pos * pos
                    | FOR of pos * pos
                    | IF of pos * pos
                    | INT of pos * pos
                    | LONG of pos * pos
                    | REGISTER of pos * pos
                    | RETURN of pos * pos
                    | SHORT of pos * pos
                    | SIGNED of pos * pos
                    | SIZEOF of pos * pos
                    | STATIC of pos * pos
                    | STRUCT of pos * pos
                    | SWITCH of pos * pos
                    | TYPEDEF of pos * pos
                    | UNSIGNED of pos * pos
                    | VOID of pos * pos
                    | WHILE of pos * pos
                    | NULL of pos * pos
                    | PRINTF of pos * pos
                    (* directives *)
                    | INCLUDE of string * pos * pos
                    | INCLUDEH of string * pos * pos
                    | DEFINE of pos * pos
                    (* values and identifier *)
                    | INTVAL of int * pos * pos
                    | FLOATVAL of real * pos * pos
                    | CHARVAL of char * pos * pos
                    | ID of string * pos * pos
                    (* seperators *)
                    | LPAREN of pos * pos
                    | RPAREN of pos * pos
                    | SEMICOLON of pos * pos
                    | COMMA of pos * pos
                    | LBRACE of pos * pos
                    | RBRACE of pos * pos
                    | LBRACKET of pos * pos
                    | RBRACKET of pos * pos
                    | QMARK of pos * pos
                    | COLON of pos * pos
                    | DOT of pos * pos
                    (* operators *)
                    | PLUS of pos * pos
                    | MINUS of pos * pos
                    | MUL of pos * pos
                    | DIV of pos * pos
                    | MODULUS of pos * pos
                    | INC of pos * pos
                    | DEC of pos * pos
                    (* comparisions *)
                    | NE of pos * pos
                    | EQ of pos * pos
                    | GT of pos * pos
                    | GTE of pos * pos
                    | LT of pos * pos
                    | LTE of pos * pos
                    (* logical *)
                    | AND of pos * pos
                    | OR of pos * pos
                    | NOT of pos * pos
                    (* bitwise *)
                    | BITAND of pos * pos
                    | BITOR of pos * pos
                    | XOR of pos * pos
                    | COMPLEMENT of pos * pos
                    | LEFTSHIFT of pos * pos
                    | RIGHTSHIFT of pos * pos
                    (* assignment *)
                    | ASSIGN of pos * pos
                    | PLUSA of pos * pos
                    | MINUSA of pos * pos
                    | TIMESA of pos * pos
                    | DIVA of pos * pos
                    | MODULUSA of pos * pos
                    | LEFTSHIFTA of pos * pos
                    | RIGHTSHIFTA of pos * pos
                    | BITORA of pos * pos
                    | BITANDA of pos * pos
                    | XORA of pos * pos
                    (* quotes *)
                    | STRING of string * pos * pos
                    (* whitespaces *)
                    | SPACE of pos * pos
                    | TAB   of pos * pos
                    | NEWLINE of pos * pos 

end