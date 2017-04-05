%{
#include "ast/ast.h"

#include "mlang.tab.h"

const char *gc_dup(const char *s)
{
	size_t sz = strlen(s);
	char *buffer = (char*)GC_MALLOC(sz + 1);
	strcpy_s(buffer, sz + 1, s);
	return buffer;
}

%}

%option yylineno noyywrap

%x BLOCK_COMMENT
%x BLOCK_DISABLE

D			[0-9]
L			[a-zA-Z_]
I			[0-9a-zA-Z_]
H			[a-fA-F0-9]
B			[01]
O			[0-7]
E			[Ee][+-]?{D}+

%%

[ \t\r\n]	;

pragma		{ return PRAGMA; }

module		{ return MODULE; }
import		{ return IMPORT; }

def			{ return DEF; }
var			{ return VAR; }

fn			{ return FN; }
struct		{ return STRUCT; }

static		{ return STATIC; }
const		{ return CONST; }

if			{ return IF; }
else		{ return ELSE;   }
for			{ return FOR; }
foreach		{ return FOREACH; }
do			{ return DO; }
while		{ return WHILE; }
match		{ return MATCH; }
return		{ return RETURN; }
break		{ return BREAK; }

cast		{ return CAST; }

void		{ return VOID; }
bool		{ return U1; }

byte		{ return I8; }
ubyte		{ return U8; }
short		{ return I16; }
ushort		{ return U16; }
int			{ return I32; }
uint		{ return U32; }
long		{ return I64; }
ulong		{ return U64; }
cent		{ return I128; }
ucent		{ return U128; }
ssize_t		{ return IZ; }
size_t		{ return UZ; }
wchar_t		{ return CZ; }

char		{ return C8; }
wchar		{ return C16; }
dchar		{ return C32; }

half		{ return F16; }
float		{ return F32; }
double		{ return F64; }
extended	{ return F128; }

"..."		{ return ELIPSIS; }

!is			{ return ISNOT; }
is			{ return IS; }

"->"		{ return ARROW; }
"=>"		{ return IMPLY; }

".."		{ return SLICE; }
"++"		{ return INCOP; }
"--"		{ return DECOP; }

"<<"		{ return SHL; }
">>"		{ return ASR; }
">>>"		{ return LSR; }

"=="		{ return EQ; }
"!="		{ return NEQ; }
">="		{ return GEQ; }
"<="		{ return LEQ; }

"&&"		{ return AND; }
"||"		{ return OR; }

"^^"		{ return POW; }

":="		{ return BIND; }

"^^="		{ return POWEQ; }
"*="		{ return MULEQ; }
"/="		{ return DIVEQ; }
"%="		{ return MODEQ; }
"+="		{ return ADDEQ; }
"-="		{ return SUBEQ; }
"~="		{ return CONCATEQ; }
"|="		{ return BITOREQ; }
"&="		{ return BITANDEQ; }
"^="		{ return BITXOREQ; }
"||="		{ return OREQ; }
"&&="		{ return ANDEQ; }
"<<="		{ return SHLEQ; }
">>="		{ return ASREQ; }
">>>="		{ return LSREQ; }

\"(\\.|[^\n"])*\"[wd]? { yylval.sval = gc_dup(yytext); return STRING; }
\`(\\.|[^\n`])*\`[wd]? { yylval.sval = gc_dup(yytext); return STRING; }
\'(\\.|[^\n'])*\'[wd]? { yylval.sval = gc_dup(yytext); return CHARACTER; }

null			{ yylval.ival = 0; return NUL; }

true			{ yylval.ival = 1; return BOOL_T; }
false			{ yylval.ival = 0; return BOOL_T; }

{D}+\.{D}+{E}?		{ yylval.fval = atof(yytext); return FLOATING; }
{D}+{E}				{ yylval.fval = atof(yytext); return FLOATING; }
{D}+				{ yylval.ival = String(yytext).parse_int(); return INTEGER; }
"0x"{H}({H}|_{H})*	{ yylval.ival = String(yytext).drop_front(2).parse_int<16>(); return INTEGER; }
"0b"{B}({B}|_{B})*	{ yylval.ival = String(yytext).drop_front(2).parse_int<2>(); return INTEGER; }
"0o"{O}({O}|_{O})*	{ yylval.ival = String(yytext).drop_front(2).parse_int<8>(); return INTEGER; }

{L}{I}*			{ yylval.sval = gc_dup(yytext); return IDENTIFIER; }

"/*"			BEGIN(BLOCK_COMMENT);
<BLOCK_COMMENT>{
	"\*/"		BEGIN(INITIAL);
	[^\*]+		// eat comment in chunks
	"*"			// eat the lone star
}

"/+"			BEGIN(BLOCK_DISABLE);
<BLOCK_DISABLE>{
	"\+/"		BEGIN(INITIAL);
	[^\+]+		// eat comment in chunks
	"\+"		// eat the lone PLUS
}

"//".*			;

";"				{ return(';'); }
"{"				{ return('{'); }
"}"				{ return('}'); }
","				{ return(','); }
":"				{ return(':'); }
"="				{ return('='); }
"("				{ return('('); }
")"				{ return(')'); }
"["				{ return('['); }
"]"				{ return(']'); }
"."				{ return('.'); }
"&"				{ return('&'); }
"!"				{ return('!'); }
"~"				{ return('~'); }
"-"				{ return('-'); }
"+"				{ return('+'); }
"*"				{ return('*'); }
"/"				{ return('/'); }
"%"				{ return('%'); }
"<"				{ return('<'); }
">"				{ return('>'); }
"^"				{ return('^'); }
"|"				{ return('|'); }
"?"				{ return('?'); }
"#"				{ return('#'); }
"@"				{ return('@'); }
"$"				{ return('$'); }

.				;

%%
