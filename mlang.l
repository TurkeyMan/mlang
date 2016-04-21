%{
#include "mlang.tab.h"
%}

%option yylineno noyywrap

%x BLOCK_COMMENT
%x BLOCK_DISABLE
%x LINE_COMMENT

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
"++"           { return INCOP; }
"--"           { return DECOP; }

\"(\\.|[^\\"])*\" { yylval.sval = _strdup(yytext); return STRING; }

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

/*

%%
"+"                  { return PLUS;       }
"-"                  { return MINUS;      }
"*"                  { return TIMES;      }
"/"                  { return SLASH;      }
"~"                  { return TILDE;      }
"`"                  { return GRAVE;      }
"!"                  { return BANG;       }
"@"                  { return AT;         }
"#"                  { return HASH;       }
"$"                  { return DOLLAR;     }
"%"                  { return PERCENT;    }
"^"                  { return HAT;        }
"&"                  { return AMP;        }
"'"                  { return APOS;       }
"\""                 { return QUOTE;      }
"\\"                 { return BACKSLASH;  }
"|"                  { return PIPE;       }
"?"                  { return QUESTION;   }

"->"                 { return ARROW;      }
"=="                 { return EQL;        }
"!="                 { return NEQ;        }
"<"                  { return LSS;        }
">"                  { return GTR;        }
"<="                 { return LEQ;        }
">="                 { return GEQ;        }

"begin"              { return BEGINSYM;   }
"call"               { return CALLSYM;    }
"const"              { return CONSTSYM;   }
"do"                 { return DOSYM;      }
"end"                { return ENDSYM;     }
"if"                 { return IFSYM;      }
"odd"                { return ODDSYM;     }
"procedure"          { return PROCSYM;    }
"then"               { return THENSYM;    }
"var"                { return VARSYM;     }
"while"              { return WHILESYM;   }

%%


void main()
{
	while(1)
	{
		int x = yylex();
		if (x == TERMINATE)
			break;
		printf("%d\n", x);
	}
}
*/
