%{
#include "mlang.tab.h"
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

[ \t\r\n]      ;

module         { return MODULE; }
static         { return STATIC; }

def            { return DEF; }
var            { return VAR; }

if             { return IF; }
else           { return ELSE;   }
for            { return FOR; }
foreach        { return FOREACH; }
while          { return WHILE; }
match          { return MATCH; }

"..."          { return ELIPSIS; }
".."           { return SLICE; }
"++"           { return INCOP; }
"--"           { return DECOP; }

"<<"           { return SHL; }
">>"           { return ASR; }
">>>"          { return LSR; }

"=="           { return EQ; }
"!="           { return NEQ; }
">="           { return GEQ; }
"<="           { return LEQ; }

"&&"           { return AND; }
"||"           { return OR; }
"^^"           { return XOR; }

":="           { return BINDEQ; }
"*="           { return MULEQ; }
"/="           { return DIVEQ; }
"%="           { return MODEQ; }
"+="           { return ADDEQ; }
"-="           { return SUBEQ; }
"|="           { return BITOREQ; }
"&="           { return BITANDEQ; }
"^="           { return BITXOREQ; }
"||="          { return OREQ; }
"&&="          { return ANDEQ; }
"^^="          { return XOREQ; }
"<<="          { return SHLEQ; }
">>="          { return ASREQ; }
">>>="         { return LSREQ; }

\"(\\.|[^\\"])*\" { yylval.sval = _strdup(yytext); return STRING; }

{D}+\.{D}+{E}  { yylval.fval = atof(yytext); return FLOAT; }
{D}+\.{D}+     { yylval.fval = atof(yytext); return FLOAT; }
{D}+{E}        { yylval.fval = atof(yytext); return FLOAT; }
{D}+           { yylval.ival = atoi(yytext); return INT; }

{L}{I}*        { yylval.sval = _strdup(yytext); return IDENTIFIER; }

"/*"           BEGIN(BLOCK_COMMENT);
<BLOCK_COMMENT>{
	"*/"       BEGIN(INITIAL);
	[^*\n]+    // eat comment in chunks
	"*"        // eat the lone star
	\n         yylineno++;
}

"/+"           BEGIN(BLOCK_DISABLE);
<BLOCK_DISABLE>{
	"\+/"       BEGIN(INITIAL);
	[^\+\n]+    // eat comment in chunks
	"\+"        // eat the lone star
	\n         yylineno++;
}

"//".*         ;

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

.              ;

%%
