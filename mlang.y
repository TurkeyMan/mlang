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

enum class Type
{
	Int,
	UInt,
	Double,
	String,

	List,

	Id,
	TempalteId,
	Type,
	FunctionType,

	TypedId,

	Struct,
	Tuple,

	Array,

	ArrayLiteral,
	FunctionLiteral,

	Module,
	DefType,
	DefConst,
	Var,

	OpAssign,

	Elipsis,

	MemberLookup,
	OpIndex,
};

struct Node;

extern "C" {
Node* Push(Node *n);
Node* Pop();
Node* Top();

Node* String(const char* s);
Node* Identifier(const char* i);
Node* TypeId(const char* i);
Node* Int(__int64 i);
Node* UInt(unsigned __int64 i);
Node* Float(double f);

Node* Add(Type type, Node *l = nullptr, Node *r = nullptr);
Node* SetLeft(Node *node, Node *l);
Node* SetRight(Node *node, Node *r);

void Print(Node *n);
}

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
%token ELIPSIS SLICE INCOP DECOP SHL ASR LSR EQ NEQ GEQ LEQ AND OR XOR
%token BINDEQ MULEQ DIVEQ MODEQ ADDEQ SUBEQ BITOREQ BITANDEQ BITXOREQ OREQ ANDEQ XOREQ


// define the "terminal symbol" token types I'm going to use (in CAPS
// by convention), and associate each with a field of the union:
%token <ival> INT
%token <fval> FLOAT
%token <sval> STRING
%token <sval> IDENTIFIER

%%
mlang:
	module_statements                    { cout << "Pased successfully!" << endl; }
	;

module_statements:
	module_statement                     { Push(Add(Type::List, Pop())); }
	| module_statements module_statement { Push(Add(Type::List, Pop(), Pop())); }
	;
struct_statements:
	struct_statement                     { Push(Add(Type::List, Pop())); }
	| struct_statements struct_statement { Push(Add(Type::List, Pop(), Pop())); }
	;
code_statements:
	code_statement                       { Push(Add(Type::List, Pop())); }
	| code_statements code_statement     { Push(Add(Type::List, Pop(), Pop())); }
	;

module:
	MODULE scoped_identifier ';'         { Push(Add(Type::Module, Pop())); Print(Top()); }
	;

module_statement:
	module
	| def_statement
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
	| expression_statement
	;

empty_statement:
	';'                                  { cout << "empty statement" << endl; }
	;

def_identifier:
	IDENTIFIER                           { Push(Identifier($1)); }
	| IDENTIFIER template_arguments      { Push(Add(Type::TempalteId, Identifier($1), Pop())); }
	;
def_statement:
	DEF def_identifier ':' type_expression ';'                                          { Push(Add(Type::DefType, Pop(), Pop())); Print(Top()); }
	| DEF def_identifier ':' type_expression '=' value_expression ';'                   { Push(Add(Type::DefConst, Add(Type::TypedId, Pop(), Pop()), Pop())); Print(Top()); }
	| DEF def_identifier '=' value_expression ';'                                       { Push(Add(Type::DefConst, Pop(), Pop())); Print(Top()); }
//	| DEF def_identifier ':' type_expression function_arguments '{' '}'                 { /* TODO */ Print(Top()); }
//	| DEF def_identifier ':' type_expression function_arguments '{' code_statements '}' { /* TODO */ Print(Top()); }
	;

var_statement:
	VAR IDENTIFIER ';'                                              { Push(Add(Type::Var, Identifier($2))); Print(Top()); }
	| VAR IDENTIFIER ':' type_expression ';'                        { Push(Add(Type::Var, Add(Type::TypedId, Identifier($2), Pop()))); Print(Top()); }
	| VAR IDENTIFIER ':' type_expression '=' value_expression ';'   { Push(Add(Type::Var, Add(Type::OpAssign, Add(Type::TypedId, Identifier($2), Pop()), Pop()))); Print(Top()); }
	| VAR IDENTIFIER '=' value_expression ';'                       { Push(Add(Type::Var, Add(Type::OpAssign, Identifier($2), Pop()))); Print(Top()); }
	;

expression_statement:
	value_expression ';'            { Print(Top()); }
	;

template_arguments:
	'(' ')'                         { Push(Add(Type::List)); }
	| '(' template_arg_list ')' 
	;

function_arguments:
	'(' ')'                         { Push(Add(Type::List)); }
	| '(' function_arg_list ')'
	;

function_arg_types:
	'(' ')'                         { Push(Add(Type::List)); }
	| '(' type_expression_list ')'
	;

function_call:
	'(' ')'                         { Push(Add(Type::List)); }
	| '(' value_expression_list ')' 
	;

array_index:
	'[' ']'                         { Push(Add(Type::List)); }
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
	'{' '}'                   { Push(Add(Type::List)); }
	| '{' code_statements '}'
	| code_statement		  { Push(Add(Type::List, Pop())); }
	;

scoped_identifier:
	IDENTIFIER						    { Push(Identifier($1)); }
	| scoped_identifier '.' IDENTIFIER  { Push(Add(Type::MemberLookup, Pop(), Identifier($3))); }
	;

template_arg:
	IDENTIFIER                                            { Push(Identifier($1)); }
	| IDENTIFIER ELIPSIS								  { Push(Add(Type::Elipsis, Identifier($1))); }
	| IDENTIFIER ':' type_expression					  { Push(Add(Type::TypedId, Identifier($1), Pop())); }
	| IDENTIFIER '=' type_expression					  { Push(Add(Type::OpAssign, Identifier($1), Pop())); }
	| IDENTIFIER ':' type_expression '=' value_expression { Push(Add(Type::OpAssign, Add(Type::TypedId, Identifier($1), Pop()), Pop())); }
	;

function_arg:
	IDENTIFIER											  { Push(Identifier($1)); }
	| IDENTIFIER ':' type_expression                      { Push(Add(Type::TypedId, Identifier($1), Pop())); }
	| IDENTIFIER '=' type_expression					  { Push(Add(Type::OpAssign, Identifier($1), Pop())); }
	| IDENTIFIER ':' type_expression '=' value_expression { Push(Add(Type::OpAssign, Add(Type::TypedId, Identifier($1), Pop()), Pop())); }
	;

struct_defintion:
	'{' '}'                                      { Push(Add(Type::Struct, Add(Type::List))); }
	| '{' struct_statements '}'                  { Push(Add(Type::Struct, Pop())); }
	;

tuple_definition:
	'[' ']'                                      { Push(Add(Type::Tuple, Add(Type::List))); }
	| '[' type_expression_list ']'               { Push(Add(Type::Tuple, Pop())); }
	;

function_literal:
	function_arguments '{' '}'                   { Push(Add(Type::FunctionLiteral, Pop(), Add(Type::List))); }
	| function_arguments '{' code_statements '}' { Push(Add(Type::FunctionLiteral, Pop(), Pop())); }
	;

value_expression_list:
	value_expression                             { Push(Add(Type::List, Pop())); }
	| value_expression_list ',' value_expression { Push(Add(Type::List, Pop(), Pop())); }
	;

type_expression_list:
	type_expression                              { Push(Add(Type::List, Pop())); }
	| type_expression_list ',' type_expression	 { Push(Add(Type::List, Pop(), Pop())); }
	;

template_arg_list:
	template_arg                                 { Push(Add(Type::List, Pop())); }
	| template_arg_list ',' template_arg		 { Push(Add(Type::List, Pop(), Pop())); }
	;

function_arg_list:
	function_arg                                 { Push(Add(Type::List, Pop())); }
	| function_arg_list ',' function_arg		 { Push(Add(Type::List, Pop(), Pop())); }
	;



/**** VALUE EXPRESSION ****/

literal:
	INT              { Push(Int($1)); }
	| FLOAT			 { Push(Float($1)); }
	| STRING		 { Push(String($1)); }
	;

array_literal:
	'[' ']'                          { Push(Add(Type::ArrayLiteral, Add(Type::List))); }
	| '[' value_expression_list ']'	 { Push(Add(Type::ArrayLiteral, Pop())); }
	;

primary_value_expression:
	literal
	| array_literal
	| function_literal
	| IDENTIFIER                 { Push(Identifier($1)); }
	| '(' value_expression ')'
	;

postfix_value_expression:
	primary_value_expression
	| postfix_value_expression '[' ']'
	| postfix_value_expression '[' value_expression_list ']'
	| postfix_value_expression function_call
	| type_expression function_call
	| postfix_value_expression INCOP
	| postfix_value_expression DECOP
	| postfix_value_expression '.' postfix_value_expression
	;

unary_operator:	 
 	'+'
 	| '-'
 	| '!'
 	| '~'
	;

unary_value_expression:
	postfix_value_expression
	| INCOP postfix_value_expression
	| DECOP postfix_value_expression
	| unary_operator unary_value_expression
	;

mul_operator:	 
 	'*'
 	| '/'
 	| '%'
	;

mul_value_expression:
	unary_value_expression
	| mul_value_expression mul_operator unary_value_expression
	;

add_operator:
 	'+'
 	| '-'
	;

add_value_expression:
	mul_value_expression
	| add_value_expression add_operator mul_value_expression
	;

shift_operator:
 	SHL
 	| ASR
 	| LSR
	;

shift_value_expression:
 	add_value_expression
 	| shift_value_expression shift_operator add_value_expression
	;

cmp_operator:
 	'<'
	| '>'
	| LEQ
	| GEQ
	;

cmp_value_expression:
 	shift_value_expression
 	| cmp_value_expression cmp_operator shift_value_expression
	;

eq_operator:
 	EQ
	| NEQ
	;

eq_value_expression:
 	cmp_value_expression
 	| eq_value_expression eq_operator cmp_value_expression
	;

bitand_value_expression:
 	eq_value_expression
 	| bitand_value_expression '&' eq_value_expression
	;

bitxor_value_expression:
 	bitand_value_expression
 	| bitxor_value_expression '^' bitand_value_expression
	;

bitor_value_expression:
 	bitxor_value_expression
 	| bitor_value_expression '|' bitxor_value_expression
	;

and_value_expression:
 	bitor_value_expression
 	| and_value_expression AND bitor_value_expression
	;

xor_value_expression:
 	and_value_expression
 	| xor_value_expression XOR and_value_expression
	;

or_value_expression:
 	xor_value_expression
 	| or_value_expression OR xor_value_expression
	;

assign_operator:
	'='
	| BINDEQ
	| MULEQ
	| DIVEQ
	| MODEQ
	| ADDEQ
	| SUBEQ
	| BITOREQ
	| BITANDEQ
	| BITXOREQ
	| OREQ
	| ANDEQ
	| XOREQ
	;

assign_value_expression:
	or_value_expression
	| or_value_expression assign_operator or_value_expression
	;

value_expression:
	assign_value_expression
	;


/**** TYPE EXPRESSION ****/

primary_type_expression:
	IDENTIFIER          { Push(Identifier($1)); }
	| struct_defintion
	| tuple_definition
	;

postfix_type_expression:
	primary_type_expression
	| postfix_type_expression array_index                 { Push(Add(Type::Array, Pop(), Pop())); }
	| postfix_type_expression function_arg_types		  { Push(Add(Type::FunctionType, Pop(), Pop())); }
	| postfix_type_expression '.' postfix_type_expression { Push(Add(Type::MemberLookup, Pop(), Pop())); }
	;

type_expression:
	postfix_type_expression
	;

%%


extern "C" {
int run(const char *)
{
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
}

void yyerror(const char *s)
{
	cout << "EEK, parse error on line " << yylineno << "!  Message: " << s << endl;
	// might as well halt now:
	while(1){} // block
	exit(-1);
}
