type pos = int

type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult  = (svalue,pos) token

val  lineRef : pos ref = ref 0   (* reference variable to keep track of position.*)

fun eof   ()      = Tokens.EOF (!lineRef,!lineRef)

fun getChar [#"'", c, #"'"] = c
  | getChar _ = #"_"

exception IllegalHeader

fun getname1 (#">" :: xs) ys = ys 
  | getname1 (x :: xs) ys    = getname1 xs (x :: ys)
  | getname1 _ _               = raise IllegalHeader

fun getHeader1 (#"<" :: xs) = List.rev (getname1 xs [])
  | getHeader1 (x :: xs)    = getHeader1 xs 
  | getHeader1 _            = raise IllegalHeader

fun getname2 (#"\"" :: xs) ys = ys 
  | getname2 (x :: xs) ys    = getname2 xs (x :: ys)
  | getname2 _ _               = raise IllegalHeader

fun getHeader2 (#"\"" :: xs) = List.rev (getname2 xs [])
  | getHeader2 (x :: xs)    = getHeader2 xs 
  | getHeader2 _            = raise IllegalHeader

fun getString s = substring (s, 1, size(s)-2)

%%
%header (functor CLexFun (structure Tokens : Expr_TOKENS));
ws    = [\ \t];
digit = [0-9];
frac  = \.{digit}*;
exp   = [eE][-+]?{digit}+;
newline = [\n];
identifier = [_a-zA-Z][_a-zA-Z0-9]*;
multicomment = "\/\*"([^*/] | ("\/\*"[^*/]*"\*\/")*)*"\*\/";
singlecomment = "//"[^\n]*"\n";

intval   = {digit}+;
floatval = {digit}+{frac}?{exp}?;
charval  = '.';

%%

{multicomment}   => ( Tokens.MULTICOMMENT (yytext, !lineRef, !lineRef) );
{singlecomment}  => ( Tokens.SINGLECOMMENT (yytext, !lineRef, !lineRef) );
{ws}+            => ( lex() );
\n({ws}*\n)*     => ( lex() );

break            => ( Tokens.BREAK (!lineRef, !lineRef) );
case             => ( Tokens.CASE (!lineRef, !lineRef) );
char             => ( Tokens.CHAR (!lineRef, !lineRef) );
const            => ( Tokens.CONST (!lineRef, !lineRef) );
continue         => ( Tokens.CONTINUE (!lineRef, !lineRef) );
default          => ( Tokens.DEFAULT (!lineRef, !lineRef) );
do               => ( Tokens.DO (!lineRef, !lineRef) );
double           => ( Tokens.DOUBLE (!lineRef, !lineRef) );
else             => ( Tokens.ELSE (!lineRef, !lineRef) );
float            => ( Tokens.FLOAT (!lineRef, !lineRef) );
for              => ( Tokens.FOR (!lineRef, !lineRef) );
if               => ( Tokens.IF (!lineRef, !lineRef) );
int              => ( Tokens.INT (!lineRef, !lineRef) );
long             => ( Tokens.LONG (!lineRef, !lineRef) );
register         => ( Tokens.REGISTER (!lineRef, !lineRef) );
return           => ( Tokens.RETURN (!lineRef, !lineRef) );
short            => ( Tokens.SHORT (!lineRef, !lineRef) );
signed           => ( Tokens.SIGNED (!lineRef, !lineRef) );
sizeof           => ( Tokens.SIZEOF (!lineRef, !lineRef) );
static           => ( Tokens.STATIC (!lineRef, !lineRef) );
struct           => ( Tokens.STRUCT (!lineRef, !lineRef) );
switch           => ( Tokens.SWITCH (!lineRef, !lineRef) );
typedef          => ( Tokens.TYPEDEF (!lineRef, !lineRef) );
unsigned         => ( Tokens.UNSIGNED (!lineRef, !lineRef) );
void             => ( Tokens.VOID (!lineRef, !lineRef) );
while            => ( Tokens.WHILE (!lineRef, !lineRef) );
NULL             => ( Tokens.NULL (!lineRef, !lineRef) );

"#include"{ws}*"<"[^>]*">"      => ( Tokens.INCLUDE (implode(getHeader1 (explode yytext)), !lineRef, !lineRef) );
"#include"{ws}*"\""[^\"]*"\""   => ( Tokens.INCLUDEH (implode(getHeader2 (explode yytext)), !lineRef, !lineRef) );
"#define"       => ( Tokens.DEFINE (!lineRef, !lineRef) );

"("             => ( Tokens.LPAREN (!lineRef, !lineRef) );
")"             => ( Tokens.RPAREN (!lineRef, !lineRef) );
";"             => ( Tokens.SEMICOLON (!lineRef, !lineRef) );
","             => ( Tokens.COMMA (!lineRef, !lineRef) );
"{" 			      => ( Tokens.LBRACE (!lineRef, !lineRef) );
"}" 			      => ( Tokens.RBRACE (!lineRef, !lineRef) );
"[" 			      => ( Tokens.LBRACKET (!lineRef, !lineRef) );
"]" 			      => ( Tokens.RBRACKET (!lineRef, !lineRef) );
"?"             => ( Tokens.QMARK (!lineRef, !lineRef) );
":"             => ( Tokens.COLON (!lineRef, !lineRef) );
"."             => ( Tokens.DOT (!lineRef, !lineRef) );

"+"				      => ( Tokens.PLUS (!lineRef, !lineRef) );
"-"				      => ( Tokens.MINUS (!lineRef, !lineRef) );
"*"				      => ( Tokens.MUL (!lineRef, !lineRef) );
"/"				      => ( Tokens.DIV (!lineRef, !lineRef) );
"%"             => ( Tokens.MODULUS (!lineRef, !lineRef) );
"++"            => ( Tokens.INC (!lineRef, !lineRef) );
"--"            => ( Tokens.DEC (!lineRef, !lineRef) );

"!="            => ( Tokens.NE (!lineRef, !lineRef) );
"=="            => ( Tokens.EQ (!lineRef, !lineRef) );
">"				      => ( Tokens.GT (!lineRef, !lineRef) );
">="			      => ( Tokens.GTE (!lineRef, !lineRef) );
"<"				      => ( Tokens.LT (!lineRef, !lineRef) );
"<="			      => ( Tokens.LTE (!lineRef, !lineRef) );

"&&"            => ( Tokens.AND (!lineRef, !lineRef) );
"||"            => ( Tokens.OR (!lineRef, !lineRef) );
"!"			        => ( Tokens.NOT (!lineRef, !lineRef) );

"&"				      => ( Tokens.BITAND (!lineRef, !lineRef) );
"|"				      => ( Tokens.BITOR (!lineRef, !lineRef) );
"^"             => ( Tokens.XOR (!lineRef, !lineRef) );
"~"             => ( Tokens.COMPLEMENT (!lineRef, !lineRef) );
"<<"            => ( Tokens.LEFTSHIFT (!lineRef, !lineRef) );
">>"            => ( Tokens.RIGHTSHIFT (!lineRef, !lineRef) );

"="			        => ( Tokens.ASSIGN (!lineRef, !lineRef) );
"+="			      => ( Tokens.PLUSA (!lineRef, !lineRef) );
"-="			      => ( Tokens.MINUSA (!lineRef, !lineRef) );
"/="			      => ( Tokens.TIMESA (!lineRef, !lineRef) );
"*="			      => ( Tokens.DIVA (!lineRef, !lineRef) );
"%="			      => ( Tokens.MODULUSA (!lineRef, !lineRef) );
"<<="			      => ( Tokens.LEFTSHIFTA (!lineRef, !lineRef) );
">>="			      => ( Tokens.RIGHTSHIFTA (!lineRef, !lineRef) );
"|="			      => ( Tokens.BITORA (!lineRef, !lineRef) );
"&="			      => ( Tokens.BITANDA (!lineRef, !lineRef) );
"^="			      => ( Tokens.XORA (!lineRef, !lineRef) );

"\""((.|\\\")*)"\""     => ( Tokens.STRING (getString yytext, !lineRef, !lineRef) );

{intval}        => ( Tokens.INTVAL (valOf (Int.fromString yytext), !lineRef, !lineRef) );
{floatval}      => ( Tokens.FLOATVAL (valOf (Real.fromString yytext), !lineRef, !lineRef) );
{charval}       => ( Tokens.CHARVAL (getChar (explode yytext), !lineRef, !lineRef) );
{identifier}    => ( Tokens.ID (yytext, !lineRef, !lineRef) );