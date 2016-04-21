%{
#include <cstdio>
#include <iostream>

using namespace std;

// stuff from flex that bison needs to know about:
extern "C" int yyparse();
extern int yylex();
extern FILE *yyin;
extern int yylineno;
 
void yyerror(const char *s);
%}

// Bison fundamentally works by asking flex to get the next token, which it
// returns as an object of type "yystype".  But tokens could be of any
// arbitrary data type!  So we deal with that in Bison by defining a C union
// holding each of the types of tokens that Flex could return, and have Bison
// use that union instead of "int" for the definition of "yystype":
%union {
	long long ival;
	double fval;
	char *sval;
}

// define the constant-string tokens:
%token MODULE STATIC
%token DEF VAR
%token IF ELSE FOR FOREACH WHILE MATCH
%token ELIPSIS INCOP DECOP


// define the "terminal symbol" token types I'm going to use (in CAPS
// by convention), and associate each with a field of the union:
%token <ival> INT
%token <fval> FLOAT
%token <sval> STRING
%token <sval> IDENTIFIER

%%
mlang:
	module module_statements { cout << "done with a mlang file!" << endl; }
	;

module:
	MODULE scoped_identifier ';' { cout << " module" << endl; }
	| /* empty */
	;

module_statements:
	module_statement
	| module_statements module_statement
	;

struct_statements:
	struct_statement
	| struct_statements struct_statement
	;

code_statements:
	code_statement
	| code_statements code_statement
	;

module_statement:
	def_statement
	| var_statement
	| empty_statement
	;

struct_statement:
	def_statement
	| var_statement
	| empty_statement
	;

code_statement:
	def_statement
	| var_statement
	| control_statement
	| empty_statement
	;

empty_statement:
	';' { cout << " empty statement" << endl; }
	;

def_statement:
	DEF def_identifier ':' type_expression ';' { cout << " : def" << endl; }
	| DEF def_identifier ':' type_expression '=' value_expression ';' { cout << "def literal " << endl; }
	| DEF def_identifier '=' value_expression ';' { cout << "def literal " << endl; }
	| DEF def_identifier ':' type_expression function_arguments '{' '}' { cout << " : def" << endl; }
	| DEF def_identifier ':' type_expression function_arguments '{' code_statements '}' { cout << " : def" << endl; }
	;

def_identifier:
	IDENTIFIER template_arguments
	| IDENTIFIER
	;

var_statement:
	VAR IDENTIFIER ':' type_expression ';' { cout << " : var " << $2 << endl; }
	| VAR IDENTIFIER ':' type_expression '=' value_expression ';' { cout << "typed var" << $2 << endl; }
	| VAR IDENTIFIER '=' value_expression ';' { cout << "def literal " << $2 << endl; }
	;

template_arguments:
	'(' ')'
	| '(' template_arg_list ')'
	;

function_arguments:
	'(' ')'
	| '(' function_arg_list ')'
	;

function_arg_types:
	'(' ')'
	| '(' type_expression_list ')'
	;

function_call:
	'(' ')'
	| '(' value_expression_list ')'
	;

array_index:
	'[' ']'
	| '[' value_expression_list ']'
	;

control_statement:
	IF body ELSE body
	| IF body
	| FOR body
	| FOREACH body
	| WHILE body
	| MATCH body
	;

body:
	'{' '}'
	'{' code_statements '}'
	| code_statement
	;

scoped_identifier:
	scoped_identifier '.' IDENTIFIER { cout << "." << $3; }
	| IDENTIFIER { cout << $1; }
	;

template_arg:
	IDENTIFIER ':' type_expression '=' value_expression
	| IDENTIFIER ':' type_expression
	| IDENTIFIER '=' type_expression
	| IDENTIFIER ELIPSIS
	| IDENTIFIER
	;

function_arg:
	IDENTIFIER ':' type_expression '=' value_expression
	| IDENTIFIER ':' type_expression
	| IDENTIFIER '=' type_expression
	| IDENTIFIER
	;

type_expression:
	IDENTIFIER
	| type_expression array_index
	| type_expression function_arg_types
	| struct_defintion
	| tuple_definition
	;

struct_defintion:
	| '{' '}'
	| '{' struct_statements '}'
	;

tuple_definition:
	| '[' ']'
	| '[' type_expression_list ']'
	;

primary_value_expression:
	INT
	| FLOAT
	| STRING
	| IDENTIFIER
	| '(' value_expression ')'
	| function_literal
	;

postfix_value_expression:
	primary_value_expression
	| postfix_value_expression '[' ']'
	| postfix_value_expression '[' value_expression_list ']'
	| postfix_value_expression function_call
	| postfix_value_expression INCOP
	| postfix_value_expression DECOP
	;

value_expression:
	postfix_value_expression
	;

function_literal:
	| function_arguments '{' '}'
	| function_arguments '{' code_statements '}'
	;	

value_expression_list:
	value_expression
	| value_expression_list ',' value_expression
	;

type_expression_list:
	type_expression
	| type_expression_list ',' type_expression
	;

template_arg_list:
	template_arg
	| template_arg_list ',' template_arg
	;

function_arg_list:
	function_arg
	| function_arg_list ',' function_arg
	;

%%

int main(int, char**) {
	// open a file handle to a particular file:
	FILE *myfile;
	fopen_s(&myfile, "test.me", "r");
	// make sure it's valid:
	if (!myfile) {
		cout << "I can't open a.snazzle.file!" << endl;
		return -1;
	}
	// set lex to read from it instead of defaulting to STDIN:
	yyin = myfile;

	// parse through the input until there is no more:
	
	do {
		yyparse();
	} while (!feof(yyin));
	while(1){} // block
}

void yyerror(const char *s) {
	cout << "EEK, parse error on line " << yylineno << "!  Message: " << s << endl;
	// might as well halt now:
	while(1){} // block
	exit(-1);
}
