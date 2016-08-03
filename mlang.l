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
		v <<= 4;
		if(*text >= '0' && *text <= '9')
			v |= *text - '0';
		else
			v |= 10 + (*text | 0x20) - 'a';
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
E			[Ee][+-]?{D}+

%%

[ \t\r\n]	;

module		{ return MODULE; }
static		{ return STATIC; }

def			{ return DEF; }
var			{ return VAR; }

const		{ return CONST; }

if			{ return IF; }
else		{ return ELSE;   }
for			{ return FOR; }
foreach		{ return FOREACH; }
while		{ return WHILE; }
match		{ return MATCH; }
return		{ return RETURN; }
break		{ return BREAK; }

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
half		{ return F16; }
float		{ return F32; }
double		{ return F64; }
extended	{ return F128; }

"..."		{ return ELIPSIS; }

"->"		{ return ARROW; }

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

":="		{ return BINDEQ; }
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
"^^="		{ return POWEQ; }
"<<="		{ return SHLEQ; }
">>="		{ return ASREQ; }
">>>="		{ return LSREQ; }

\"(\\.|[^\\"])*\" { yylval.sval = _strdup(yytext); return STRING; }
\`(\\.|[^\\`])*\` { yylval.sval = _strdup(yytext); return STRING; }
\'(\\.|[^\\'])*\' { yylval.ival = charval(yytext); return CHAR; }

{D}+\.{D}+{E}	{ yylval.fval = atof(yytext); return FLOAT; }
{D}+\.{D}+		{ yylval.fval = atof(yytext); return FLOAT; }
{D}+{E}			{ yylval.fval = atof(yytext); return FLOAT; }
{D}+			{ yylval.ival = atoi(yytext); return INT; }
"0x"{H}+		{ yylval.ival = htoi(yytext); return INT; }

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

.				;

%%
