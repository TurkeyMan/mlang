%{
#include "src/ast.h"

#include "mlang.tab.h"

long long htoi(const char *text)
{
	if(text[0] == '0' && text[1] == 'x')
		text += 2;
	long long v = 0;
	while(*text)
	{
		if(*text != '_')
		{
			v <<= 4;
			if(*text >= '0' && *text <= '9')
				v |= *text - '0';
			else
				v |= 10 + (*text | 0x20) - 'a';
		}
		++text;
	}
	return v;
}

long long btoi(const char *text)
{
	if(text[0] == '0' && text[1] == 'b')
		text += 2;
	long long v = 0;
	while(*text)
	{
		if(*text != '_')
		{
			v <<= 1;
			v |= *text - '0';
		}
		++text;
	}
	return v;
}

long long charval(const char *text)
{
	if(text[0] == '\'')
		++text;
	long long v = *text;
	// TODO: decode UTF8...
	//       handle char sequences? (fourcc?)
	return v;
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
E			[Ee][+-]?{D}+

%%

[ \t\r\n]	;

module		{ return MODULE; }
static		{ return STATIC; }

extern		{ return EXTERN; }

def			{ return DEF; }
var			{ return VAR; }

fn			{ return FN; }
struct		{ return STRUCT; }

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

\"(\\.|[^\n"])*\"[cwd]? { yylval.sval = _strdup(yytext); return STRING; }
\`(\\.|[^\n`])*\`[cwd]? { yylval.sval = _strdup(yytext); return STRING; }
\'(\\.|[^\n'])*\'[cwd]? { yylval.ival = charval(yytext); return CHARACTER; }

null			{ yylval.ival = 0; return NUL; }

true			{ yylval.ival = 1; return BOOL_T; }
false			{ yylval.ival = 0; return BOOL_T; }

{D}+\.{D}+{E}		{ yylval.fval = atof(yytext); return FLOATING; }
{D}+\.{D}+			{ yylval.fval = atof(yytext); return FLOATING; }
{D}+{E}				{ yylval.fval = atof(yytext); return FLOATING; }
{D}+				{ yylval.ival = atoi(yytext); return INTEGER; }
"0x"{H}({H}|_{H})*	{ yylval.ival = htoi(yytext); return INTEGER; }
"0b"{B}({B}|_{B})*	{ yylval.ival = btoi(yytext); return INTEGER; }

{L}{I}*			{ yylval.sval = _strdup(yytext); return IDENTIFIER; }

"/*"			BEGIN(BLOCK_COMMENT);
<BLOCK_COMMENT>{
	"*/"		BEGIN(INITIAL);
	[^*\n]+		// eat comment in chunks
	"*"			// eat the lone star
	\n			yylineno++;
}

"/+"			BEGIN(BLOCK_DISABLE);
<BLOCK_DISABLE>{
	"\+/"		BEGIN(INITIAL);
	[^\+\n]+	// eat comment in chunks
	"\+"		// eat the lone star
	\n			yylineno++;
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
