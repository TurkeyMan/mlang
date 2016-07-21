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

#include "src/ast.h"

static StatementList parseTree = StatementList::empty();

%}

// Bison fundamentally works by asking flex to get the next token, which it
// returns as an object of type "yystype".  But tokens could be of any
// arbitrary data type!  So we deal with that in Bison by defining a C union
// holding each of the types of tokens that Flex could return, and have Bison
// use that union instead of "int" for the definition of "yystype":
%union {
	int64_t ival;
	double fval;
	const char *sval;
	Expr *expr;
	ExprList exprList;
	TypeExpr *type;
	TypeExprList typeExprList;
	Statement *statement;
	StatementList statementList;
}

// define the constant-string tokens:
%token MODULE STATIC
%token DEF VAR
%token CONST
%token IF ELSE FOR FOREACH WHILE MATCH RETURN BREAK
%token ELIPSIS SLICE INCOP DECOP SHL ASR LSR EQ NEQ GEQ LEQ AND OR XOR
%token BINDEQ MULEQ DIVEQ MODEQ ADDEQ SUBEQ CONCATEQ BITOREQ BITANDEQ BITXOREQ OREQ ANDEQ XOREQ SHLEQ ASREQ LSREQ

%token VOID U1 I8 U8 I16 U16 I32 U32 I64 U64 F32 F64

// define the "terminal symbol" token types I'm going to use (in CAPS
// by convention), and associate each with a field of the union:
%token <ival> INT CHAR
%token <fval> FLOAT
%token <sval> STRING IDENTIFIER

%type <expr> literal array_literal function_literal value_expression
%type <exprList> value_expression_list

%type <type> primitive_type primary_type_expression type_expression struct_definition tuple_definition
%type <typeExprList> type_expression_list function_arg_types

%type <statement> module_statement struct_statement code_statement empty_statement module def_statement var_statement
%type <statementList> module_statements struct_statements code_statements body

%type <statement> function_arg
%type <statementList> function_arguments function_arg_list

%%
mlang:
	module_statements						{ parseTree = $1; cout << "Parsed successfully!" << endl; }
	;

module_statements:
	module_statement						{ $$ = StatementList::empty(); if ($1) $$ = $$.append($1); }
	| module_statements module_statement	{ $$ = $2 ? $1.append($2) : $1; }
	;
struct_statements:
	struct_statement						{ $$ = StatementList::empty(); if ($1) $$ = $$.append($1); }
	| struct_statements struct_statement	{ $$ = $2 ? $1.append($2) : $1; }
	;
code_statements:
	code_statement							{ $$ = StatementList::empty(); if ($1) $$ = $$.append($1); }
	| code_statements code_statement		{ $$ = $2 ? $1.append($2) : $1; }
	;

module:
	MODULE IDENTIFIER ';'					{ $$ = new ModuleStatement($2); }
	;
/*
	MODULE scoped_identifier ';'			{ Push(Add(Generic::Type::Module, Pop())); Print(Top()); }
	;
*/

module_statement:
	module						{ $$ = $1; }
	| def_statement				{ $$ = $1; }
	| var_statement				{ $$ = $1; }
//	| static_control_statement
	| empty_statement			{ $$ = $1; }
	;

struct_statement:
	def_statement				{ $$ = $1; }
	| var_statement				{ $$ = $1; }
//	| static_control_statement
	| empty_statement			{ $$ = $1; }
	;

code_statement:
	def_statement				{ $$ = $1; }
	| var_statement				{ $$ = $1; }
//	| static_control_statement
	| control_statement			{ $$ = (Statement*)Pop(); }
//	| expression_statement
	| empty_statement			{ $$ = $1; }
	;

empty_statement:
	';'							{ $$ = nullptr; }
	;

/*
def_identifier:
	IDENTIFIER							{ Push(Identifier($1)); }
	| IDENTIFIER template_arguments		{ Push(Add(Generic::Type::TempalteId, Identifier($1), Pop())); }
	;
*/
def_statement:
	DEF IDENTIFIER ':' type_expression ';'											{ $$ = new TypeDecl($2, $4); }
	| DEF IDENTIFIER ':' type_expression '=' value_expression ';'					{ $$ = new ValDecl($2, $4, $6); }
	| DEF IDENTIFIER '=' value_expression ';'										{ $$ = new ValDecl($2, nullptr, $4); }
	;
/*
	| DEF def_identifier ':' type_expression function_arguments '{' '}'					{ ... Print(Top()); }
	| DEF def_identifier ':' type_expression function_arguments '{' code_statements '}'	{ ... Print(Top()); }
	;
*/

var_statement:
	VAR IDENTIFIER ';'												{ $$ = new VarDecl($2, nullptr, nullptr); }
	| VAR IDENTIFIER ':' type_expression ';'						{ $$ = new VarDecl($2, $4, nullptr); }
	| VAR IDENTIFIER ':' type_expression '=' value_expression ';'	{ $$ = new VarDecl($2, $4, $6); }
	| VAR IDENTIFIER '=' value_expression ';'						{ $$ = new VarDecl($2, nullptr, $4); }
	;

expression_statement:
	value_expression ';'			{ Print($1); }
	;

template_arguments:
	'(' ')'							{ Push(Add(Generic::Type::List)); }
	| '(' template_arg_list ')'
	;

function_arguments:
	'(' ')'							{ $$ = StatementList::empty(); }
	| '(' function_arg_list ')'		{ $$ = $2; }
	;

function_arg_types:
	'(' ')'							{ $$ = TypeExprList::empty(); }
	| '(' type_expression_list ')'	{ $$ = $2; }
	;

function_call:
	'(' ')'							{ Push(Add(Generic::Type::List)); }
	| '(' value_expression_list ')'
	;

array_index:
	'[' ']'							{ Push(Add(Generic::Type::List)); }
	| '[' value_expression_list ']'
	;

control_statement:
	RETURN value_expression ';'		{ Push(Add(Generic::Type::Return, $2)); }
	| BREAK ';'						{ Push(Add(Generic::Type::Break)); }
//	| IF '(' value_expression ')' body ELSE body
//	| IF '(' value_expression ')' body
//	| WHILE '(' value_expression ')' body
//	| FOR body
//	| FOREACH body
//	| MATCH body
	;

static_control_statement:
	STATIC control_statement
	;

body:
	'{' '}'						{ $$ = StatementList::empty(); }
	| '{' code_statements '}'	{ $$ = $2; }
	| code_statement			{ $$ = StatementList::empty(); if ($1) $$ = $$.append($1); }
	;

scoped_identifier:
	IDENTIFIER							{ Push(Identifier($1)); }
	| scoped_identifier '.' IDENTIFIER	{ Push(Add(Generic::Type::MemberLookup, Pop(), Identifier($3))); }
	;

template_arg:
	IDENTIFIER												{ Push(Identifier($1)); }
	| IDENTIFIER ELIPSIS									{ Push(Add(Generic::Type::Elipsis, Identifier($1))); }
	| IDENTIFIER ':' type_expression						{ Push(Add(Generic::Type::TypedId, Identifier($1), $3)); }
	| IDENTIFIER '=' value_expression						{ Push(Add(Generic::Type::OpAssign, Identifier($1), $3)); }
	| IDENTIFIER ':' type_expression '=' value_expression	{ Push(Add(Generic::Type::OpAssign, Add(Generic::Type::TypedId, Identifier($1), $3), $5)); }
	;

function_arg:
	IDENTIFIER												{ $$ = new VarDecl($1, nullptr, nullptr); }
	| IDENTIFIER ':' type_expression						{ $$ = new VarDecl($1, $3, nullptr); }
	| IDENTIFIER '=' value_expression						{ $$ = new VarDecl($1, nullptr, $3); }
	| IDENTIFIER ':' type_expression '=' value_expression	{ $$ = new VarDecl($1, $3, $5); }
	;

struct_definition:
	'{' '}'											{ $$ = new Struct(); }
	| '{' struct_statements '}'						{ $$ = new Struct($2); }
	;

tuple_definition:
	'[' ']'											{ $$ = new TupleType(); }
	| '[' type_expression_list ']'					{ $$ = new TupleType($2); }
	;

function_literal:
	function_arguments '{' '}'						{ $$ = new FunctionLiteralExpr(StatementList::empty(), $1, nullptr); }
	| function_arguments '{' code_statements '}'	{ $$ = new FunctionLiteralExpr($3, $1, nullptr); }
	;

value_expression_list:
	value_expression								{ $$ = ExprList::empty().append($1); }
	| value_expression_list ',' value_expression	{ $$ = $1.append($3); }
	;

type_expression_list:
	type_expression									{ $$ = TypeExprList::empty().append($1); }
	| type_expression_list ',' type_expression		{ $$ = $1.append($3); }
	;

template_arg_list:
	template_arg									{ Push(Add(Generic::Type::List, Pop())); }
	| template_arg_list ',' template_arg			{ Push(Add(Generic::Type::List, Pop(), Pop())); }
	;

function_arg_list:
	function_arg									{ $$ = StatementList::empty().append($1); }
	| function_arg_list ',' function_arg			{ $$ = $1.append($3); }
	;



/**** VALUE EXPRESSION ****/

literal:
	INT			{ $$ = new PrimitiveLiteralExpr($1); }
	| FLOAT		{ $$ = new PrimitiveLiteralExpr($1); }
//	| STRING	{ $$ = new PrimitiveLiteralExpr($1); }
	| CHAR		{ $$ = new PrimitiveLiteralExpr((char32_t)$1); }
	;

array_literal:
	'[' ']'							{ $$ = new ArrayLiteralExpr(ExprList::empty()); }
	| '[' value_expression_list ']'	{ $$ = new ArrayLiteralExpr($2); }
	;

primary_value_expression:
	literal							{ Push($1); }
	| array_literal
	| function_literal				{ Push($1); }
	| IDENTIFIER					{ Push(Identifier($1)); }
	| '(' value_expression ')'		{ Push($2); }
	;

postfix_value_expression:
	primary_value_expression
	| postfix_value_expression '[' ']'							{ Push(Add(Generic::Type::OpIndex, Pop(), Add(Generic::Type::List))); }
	| postfix_value_expression '[' value_expression_list ']'	{ Push(Add(Generic::Type::OpIndex, Pop(), Pop())); }
	| postfix_value_expression function_call					{ Push(Add(Generic::Type::Call, Pop(), Pop())); }
	| type_expression function_call								{ Push(Add(Generic::Type::Call, $1, Pop())); }
	| postfix_value_expression INCOP							{ Push(Add(Generic::Type::OpPostInc, Pop())); }
	| postfix_value_expression DECOP							{ Push(Add(Generic::Type::OpPostDec, Pop())); }
	| postfix_value_expression '.' postfix_value_expression		{ Push(Add(Generic::Type::MemberLookup, Pop(), Pop())); }
	;

unary_operator:
 	'+'		{ Push(Add(Generic::Type::OpUnaryPlus)); }
 	| '-'	{ Push(Add(Generic::Type::OpUnaryMinus)); }
 	| '!'	{ Push(Add(Generic::Type::OpUnaryNot)); }
 	| '~'	{ Push(Add(Generic::Type::OpUnaryComp)); }
	;

unary_value_expression:
	postfix_value_expression
	| INCOP postfix_value_expression		{ Push(Add(Generic::Type::OpPreInc, Pop())); }
	| DECOP postfix_value_expression		{ Push(Add(Generic::Type::OpPreDec, Pop())); }
	| unary_operator unary_value_expression	{ Push(SetChildren((Generic*)Pop(), Pop())); }
	;

mul_operator:
 	'*'		{ Push(Add(Generic::Type::OpMul)); }
 	| '/'	{ Push(Add(Generic::Type::OpDiv)); }
 	| '%'	{ Push(Add(Generic::Type::OpMod)); }
	;

mul_value_expression:
	unary_value_expression
	| mul_value_expression mul_operator unary_value_expression		{ Node *n = Pop(), *m = Pop(); Push(SetChildren((Generic*)m, Pop(), n)); }
	;

add_operator:
 	'+'		{ Push(Add(Generic::Type::OpAdd)); }
 	| '-'	{ Push(Add(Generic::Type::OpSub)); }
	| '~'	{ Push(Add(Generic::Type::OpConcat)); }
	;

add_value_expression:
	mul_value_expression
	| add_value_expression add_operator mul_value_expression		{ Node *n = Pop(), *m = Pop(); Push(SetChildren((Generic*)m, Pop(), n)); }
	;

shift_operator:
 	SHL		{ Push(Add(Generic::Type::OpASL)); }
 	| ASR	{ Push(Add(Generic::Type::OpASR)); }
 	| LSR	{ Push(Add(Generic::Type::OpLSR)); }
	;

shift_value_expression:
 	add_value_expression
 	| shift_value_expression shift_operator add_value_expression	{ Node *n = Pop(), *m = Pop(); Push(SetChildren((Generic*)m, Pop(), n)); }
	;

cmp_operator:
 	'<'		{ Push(Add(Generic::Type::OpLt)); }
	| '>'	{ Push(Add(Generic::Type::OpGt)); }
	| LEQ	{ Push(Add(Generic::Type::OpLe)); }
	| GEQ	{ Push(Add(Generic::Type::OpGe)); }
	;

cmp_value_expression:
 	shift_value_expression
 	| cmp_value_expression cmp_operator shift_value_expression		{ Node *n = Pop(), *m = Pop(); Push(SetChildren((Generic*)m, Pop(), n)); }
	;

eq_operator:
 	EQ		{ Push(Add(Generic::Type::OpEq)); }
	| NEQ	{ Push(Add(Generic::Type::OpNe)); }
	;

eq_value_expression:
 	cmp_value_expression
 	| eq_value_expression eq_operator cmp_value_expression	{ Node *n = Pop(), *m = Pop(); Push(SetChildren((Generic*)m, Pop(), n)); }
	;

bitand_value_expression:
 	eq_value_expression
 	| bitand_value_expression '&' eq_value_expression		{ Push(Add(Generic::Type::OpBitAnd, Pop(), Pop())); }
	;

bitxor_value_expression:
 	bitand_value_expression
 	| bitxor_value_expression '^' bitand_value_expression	{ Push(Add(Generic::Type::OpBitXor, Pop(), Pop())); }
	;

bitor_value_expression:
 	bitxor_value_expression
 	| bitor_value_expression '|' bitxor_value_expression	{ Push(Add(Generic::Type::OpBitOr, Pop(), Pop())); }
	;

and_value_expression:
 	bitor_value_expression
 	| and_value_expression AND bitor_value_expression		{ Push(Add(Generic::Type::OpAnd, Pop(), Pop())); }
	;

xor_value_expression:
 	and_value_expression
 	| xor_value_expression XOR and_value_expression			{ Push(Add(Generic::Type::OpXor, Pop(), Pop())); }
	;

or_value_expression:
 	xor_value_expression
 	| or_value_expression OR xor_value_expression			{ Push(Add(Generic::Type::OpOr, Pop(), Pop())); }
	;

assign_operator:
	'='			{ Push(Add(Generic::Type::OpAssign)); }
	| BINDEQ	{ Push(Add(Generic::Type::OpBind)); }
	| MULEQ		{ Push(Add(Generic::Type::OpMulEq)); }
	| DIVEQ		{ Push(Add(Generic::Type::OpDivEq)); }
	| MODEQ		{ Push(Add(Generic::Type::OpModEq)); }
	| ADDEQ		{ Push(Add(Generic::Type::OpAddEq)); }
	| SUBEQ		{ Push(Add(Generic::Type::OpSubEq)); }
	| CONCATEQ	{ Push(Add(Generic::Type::OpConcatEq)); }
	| BITOREQ	{ Push(Add(Generic::Type::OpBitAndEq)); }
	| BITANDEQ	{ Push(Add(Generic::Type::OpBitXorEq)); }
	| BITXOREQ	{ Push(Add(Generic::Type::OpBitOrEq)); }
	| OREQ		{ Push(Add(Generic::Type::OpAndEq)); }
	| ANDEQ		{ Push(Add(Generic::Type::OpXorEq)); }
	| XOREQ		{ Push(Add(Generic::Type::OpOrEq)); }
	| SHLEQ		{ Push(Add(Generic::Type::OpASLEq)); }
	| ASREQ		{ Push(Add(Generic::Type::OpASREq)); }
	| LSREQ		{ Push(Add(Generic::Type::OpLSREq)); }
	;

assign_value_expression:
	or_value_expression
	| or_value_expression assign_operator or_value_expression	{ Node *n = Pop(), *m = Pop(); Push(SetChildren((Generic*)m, Pop(), n)); }
	;

value_expression:
	assign_value_expression		{ $$ = (Expr*)Pop(); }
	;


/**** TYPE EXPRESSION ****/

primitive_type:
	VOID	{ $$ = new PrimitiveType(PrimType::v); }
	| U1	{ $$ = new PrimitiveType(PrimType::u1); }
	| I8 	{ $$ = new PrimitiveType(PrimType::i8); }
	| U8	{ $$ = new PrimitiveType(PrimType::u8); }
	| I16	{ $$ = new PrimitiveType(PrimType::i16); }
	| U16	{ $$ = new PrimitiveType(PrimType::u16); }
	| I32	{ $$ = new PrimitiveType(PrimType::i32); }
	| U32	{ $$ = new PrimitiveType(PrimType::u32); }
	| I64	{ $$ = new PrimitiveType(PrimType::i64); }
	| U64	{ $$ = new PrimitiveType(PrimType::u64); }
	| F32	{ $$ = new PrimitiveType(PrimType::f32); }
	| F64	{ $$ = new PrimitiveType(PrimType::f64); }
	;

primary_type_expression:
	primitive_type				{ $$ = $1; }
	| IDENTIFIER				{ $$ = new TypeIdentifier($1); }
	| struct_definition			{ $$ = $1; }
	| tuple_definition			{ $$ = $1; }
	| '(' type_expression ')'	{ $$ = $2; }
	;

postfix_type_expression:
	primary_type_expression									{ Push($1); }
	| postfix_type_expression '*'							{ Push(Add(Generic::Type::Pointer, Pop())); }
	| postfix_type_expression '&'							{ Push(Add(Generic::Type::Ref, Pop())); }
	| postfix_type_expression array_index					{ Push(Add(Generic::Type::Array, Pop(), Pop())); }
	| postfix_type_expression function_arg_types			{ Push(new FunctionType((TypeExpr*)Pop(), $2)); }
	| postfix_type_expression '.' postfix_type_expression	{ Push(Add(Generic::Type::MemberLookup, Pop(), Pop())); }
//	| IDENTIFIER '!' type_expression						{ Push(Add(Generic::Type::Instantiate, Identifier($1), Add(Generic::Type::List, $3))); }
//	| IDENTIFIER '!' '(' ')'								{ Push(Add(Generic::Type::Instantiate, Identifier($1), Add(Generic::Type::List))); }
//	| IDENTIFIER '!' '(' type_expression_list ')'			{ Push(Add(Generic::Type::Instantiate, Identifier($1), Pop())); }
	;

modified_type:
	postfix_type_expression			//{ $$ = $1; }
	| CONST postfix_type_expression	{ Push(Add(Generic::Type::Const, Pop())); }
	;

type_expression:
	modified_type					{ $$ = (TypeExpr*)Pop(); }
	;

%%


void yyerror(const char *s)
{
	cout << "EEK, parse error on line " << yylineno << "! Message: " << s << endl;
	// might as well halt now:
	while(1){} // block
	exit(-1);
}


StatementList parse(FILE *file)
{
	// set lex to read from it instead of defaulting to STDIN:
	yyin = file;

	// parse through the input until there is no more:

	do {
		yyparse();
	} while (!feof(yyin));

	return parseTree;
}
