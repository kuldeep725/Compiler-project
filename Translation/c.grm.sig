signature Expr_TOKENS =
sig
type ('a,'b) token
type svalue
val LOWER:  'a * 'a -> (svalue,'a) token
val HIGHER:  'a * 'a -> (svalue,'a) token
val STRING: (string) *  'a * 'a -> (svalue,'a) token
val XORA:  'a * 'a -> (svalue,'a) token
val BITANDA:  'a * 'a -> (svalue,'a) token
val BITORA:  'a * 'a -> (svalue,'a) token
val RIGHTSHIFTA:  'a * 'a -> (svalue,'a) token
val LEFTSHIFTA:  'a * 'a -> (svalue,'a) token
val MODULUSA:  'a * 'a -> (svalue,'a) token
val DIVA:  'a * 'a -> (svalue,'a) token
val TIMESA:  'a * 'a -> (svalue,'a) token
val MINUSA:  'a * 'a -> (svalue,'a) token
val PLUSA:  'a * 'a -> (svalue,'a) token
val ASSIGN:  'a * 'a -> (svalue,'a) token
val RIGHTSHIFT:  'a * 'a -> (svalue,'a) token
val LEFTSHIFT:  'a * 'a -> (svalue,'a) token
val COMPLEMENT:  'a * 'a -> (svalue,'a) token
val XOR:  'a * 'a -> (svalue,'a) token
val BITOR:  'a * 'a -> (svalue,'a) token
val BITAND:  'a * 'a -> (svalue,'a) token
val NOT:  'a * 'a -> (svalue,'a) token
val OR:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val LTE:  'a * 'a -> (svalue,'a) token
val LT:  'a * 'a -> (svalue,'a) token
val GTE:  'a * 'a -> (svalue,'a) token
val GT:  'a * 'a -> (svalue,'a) token
val EQ:  'a * 'a -> (svalue,'a) token
val NE:  'a * 'a -> (svalue,'a) token
val DEC:  'a * 'a -> (svalue,'a) token
val INC:  'a * 'a -> (svalue,'a) token
val MODULUS:  'a * 'a -> (svalue,'a) token
val DIV:  'a * 'a -> (svalue,'a) token
val MUL:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val DOT:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val QMARK:  'a * 'a -> (svalue,'a) token
val RBRACKET:  'a * 'a -> (svalue,'a) token
val LBRACKET:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
val CHARVAL: (char) *  'a * 'a -> (svalue,'a) token
val FLOATVAL: (real) *  'a * 'a -> (svalue,'a) token
val INTVAL: (int) *  'a * 'a -> (svalue,'a) token
val DEFINE:  'a * 'a -> (svalue,'a) token
val INCLUDEH: (string) *  'a * 'a -> (svalue,'a) token
val INCLUDE: (string) *  'a * 'a -> (svalue,'a) token
val PRINTF:  'a * 'a -> (svalue,'a) token
val NULL:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val VOID:  'a * 'a -> (svalue,'a) token
val UNSIGNED:  'a * 'a -> (svalue,'a) token
val TYPEDEF:  'a * 'a -> (svalue,'a) token
val SWITCH:  'a * 'a -> (svalue,'a) token
val STRUCT:  'a * 'a -> (svalue,'a) token
val STATIC:  'a * 'a -> (svalue,'a) token
val SIZEOF:  'a * 'a -> (svalue,'a) token
val SIGNED:  'a * 'a -> (svalue,'a) token
val SHORT:  'a * 'a -> (svalue,'a) token
val RETURN:  'a * 'a -> (svalue,'a) token
val REGISTER:  'a * 'a -> (svalue,'a) token
val LONG:  'a * 'a -> (svalue,'a) token
val INT:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val FOR:  'a * 'a -> (svalue,'a) token
val FLOAT:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val DOUBLE:  'a * 'a -> (svalue,'a) token
val DO:  'a * 'a -> (svalue,'a) token
val DEFAULT:  'a * 'a -> (svalue,'a) token
val CONTINUE:  'a * 'a -> (svalue,'a) token
val CONST:  'a * 'a -> (svalue,'a) token
val CHAR:  'a * 'a -> (svalue,'a) token
val CASE:  'a * 'a -> (svalue,'a) token
val BREAK:  'a * 'a -> (svalue,'a) token
val SINGLECOMMENT: (string) *  'a * 'a -> (svalue,'a) token
val MULTICOMMENT: (string) *  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature Expr_LRVALS=
sig
structure Tokens : Expr_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
