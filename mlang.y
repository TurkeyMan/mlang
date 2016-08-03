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
	UnaryOp unaryOp;
}

// define the constant-string tokens:
%token MODULE STATIC
%token DEF VAR
%token CONST
%token IF ELSE FOR FOREACH WHILE MATCH RETURN BREAK
%token ELIPSIS ARROW SLICE INCOP DECOP SHL ASR LSR EQ NEQ GEQ LEQ AND OR POW
%token BINDEQ MULEQ DIVEQ MODEQ ADDEQ SUBEQ CONCATEQ BITOREQ BITANDEQ BITXOREQ OREQ ANDEQ POWEQ SHLEQ ASREQ LSREQ

%token VOID U1 I8 U8 I16 U16 I32 U32 I64 U64 I128 U128 IZ UZ F16 F32 F64 F128

// define the "terminal symbol" token types I'm going to use (in CAPS
// by convention), and associate each with a field of the union:
%token <ival> INT CHAR
%token <fval> FLOAT
%token <sval> STRING IDENTIFIER

%type <expr> literal array_literal function_literal primary_value_expression postfix_value_expression unary_value_expression pow_value_expression mul_value_expression add_value_expression shift_value_expression cmp_value_expression eq_value_expression bitand_value_expression bitxor_value_expression bitor_value_expression and_value_expression or_value_expression assign_value_expression value_expression
%type <exprList> value_expression_list

%type <type> primitive_type primary_type_expression type_expression struct_definition tuple_definition
%type <typeExprList> type_expression_list function_arg_types

%type <statement> module_statement struct_statement code_statement empty_statement module def_statement var_statement control_statement static_control_statement
%type <statementList> module_statements struct_statements code_statements body

%type <statement> function_arg
%type <statementList> function_arguments function_arg_list

%type <unaryOp> unary_operator

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
	| control_statement			{ $$ = $1; }
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
	RETURN ';'						{ $$ = new ReturnStatement(nullptr); }
	| RETURN value_expression ';'	{ $$ = new ReturnStatement($2); }
//	| BREAK ';'						{ Push(Add(Generic::Type::Break)); }
//	| IF '(' value_expression ')' body ELSE body
//	| IF '(' value_expression ')' body
//	| WHILE '(' value_expression ')' body
//	| FOR body
//	| FOREACH body
//	| MATCH body
	;

static_control_statement:
	STATIC control_statement	{ $$ = $2; }
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
	| function_arguments ARROW type_expression '{' code_statements '}'	{ $$ = new FunctionLiteralExpr($5, $1, $3); }
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
	literal							{ $$ = $1; }
	| array_literal					{ $$ = $1; }
	| function_literal				{ $$ = $1; }
	| IDENTIFIER					{ $$ = new IdentifierExpr($1); }
	| '(' value_expression ')'		{ $$ = $2; }
	;

postfix_value_expression:
	primary_value_expression									{ $$ = $1; }
//	| postfix_value_expression '[' ']'							{ Push(Add(Generic::Type::OpIndex, Pop(), Add(Generic::Type::List))); }
//	| postfix_value_expression '[' value_expression_list ']'	{ Push(Add(Generic::Type::OpIndex, Pop(), Pop())); }
//	| postfix_value_expression function_call					{ Push(Add(Generic::Type::Call, Pop(), Pop())); }
//	| type_expression function_call								{ Push(Add(Generic::Type::Call, $1, Pop())); }
//	| postfix_value_expression INCOP							{ Push(Add(Generic::Type::OpPostInc, Pop())); }
//	| postfix_value_expression DECOP							{ Push(Add(Generic::Type::OpPostDec, Pop())); }
//	| postfix_value_expression '.' postfix_value_expression		{ Push(Add(Generic::Type::MemberLookup, Pop(), Pop())); }
	;

unary_operator:
 	'+'		{ $$ = UnaryOp::Pos; }
 	| '-'	{ $$ = UnaryOp::Neg; }
 	| '~'	{ $$ = UnaryOp::BitNot; }
 	| '!'	{ $$ = UnaryOp::LogicNot; }
 	| INCOP	{ $$ = UnaryOp::PreInc; }
 	| DECOP	{ $$ = UnaryOp::PreDec; }
	;
unary_value_expression:
	postfix_value_expression								{ $$ = $1; }
	| unary_operator postfix_value_expression				{ $$ = new UnaryExpr($1, $2); }
	;

pow_value_expression:
	unary_value_expression									{ $$ = $1; }
	| pow_value_expression POW unary_value_expression		{ $$ = new BinaryExpr(BinOp::Pow, $1, $3); }

mul_value_expression:
	pow_value_expression									{ $$ = $1; }
	| mul_value_expression '*' pow_value_expression			{ $$ = new BinaryExpr(BinOp::Mul, $1, $3); }
	| mul_value_expression '/' pow_value_expression			{ $$ = new BinaryExpr(BinOp::Div, $1, $3); }
	| mul_value_expression '%' pow_value_expression			{ $$ = new BinaryExpr(BinOp::Mod, $1, $3); }
	;
add_value_expression:
	mul_value_expression									{ $$ = $1; }
	| add_value_expression '+' mul_value_expression			{ $$ = new BinaryExpr(BinOp::Add, $1, $3); }
	| add_value_expression '-' mul_value_expression			{ $$ = new BinaryExpr(BinOp::Sub, $1, $3); }
	| add_value_expression '~' mul_value_expression			{ $$ = new BinaryExpr(BinOp::Cat, $1, $3); }
	;
shift_value_expression:
 	add_value_expression									{ $$ = $1; }
 	| shift_value_expression SHL add_value_expression		{ $$ = new BinaryExpr(BinOp::ASL, $1, $3); }
 	| shift_value_expression ASR add_value_expression		{ $$ = new BinaryExpr(BinOp::ASR, $1, $3); }
 	| shift_value_expression LSR add_value_expression		{ $$ = new BinaryExpr(BinOp::LSR, $1, $3); }
	;
cmp_value_expression:
 	shift_value_expression									{ $$ = $1; }
 	| cmp_value_expression '<' shift_value_expression		{ $$ = new BinaryExpr(BinOp::Lt, $1, $3); }
 	| cmp_value_expression '>' shift_value_expression		{ $$ = new BinaryExpr(BinOp::Gt, $1, $3); }
 	| cmp_value_expression LEQ shift_value_expression		{ $$ = new BinaryExpr(BinOp::Le, $1, $3); }
 	| cmp_value_expression GEQ shift_value_expression		{ $$ = new BinaryExpr(BinOp::Ge, $1, $3); }
	;
eq_value_expression:
 	cmp_value_expression									{ $$ = $1; }
 	| eq_value_expression EQ cmp_value_expression			{ $$ = new BinaryExpr(BinOp::Eq, $1, $3); }
 	| eq_value_expression NEQ cmp_value_expression			{ $$ = new BinaryExpr(BinOp::Ne, $1, $3); }
	;
bitand_value_expression:
 	eq_value_expression										{ $$ = $1; }
 	| bitand_value_expression '&' eq_value_expression		{ $$ = new BinaryExpr(BinOp::BitAnd, $1, $3); }
	;
bitxor_value_expression:
 	bitand_value_expression									{ $$ = $1; }
 	| bitxor_value_expression '^' bitand_value_expression	{ $$ = new BinaryExpr(BinOp::BitXor, $1, $3); }
	;
bitor_value_expression:
 	bitxor_value_expression									{ $$ = $1; }
 	| bitor_value_expression '|' bitxor_value_expression	{ $$ = new BinaryExpr(BinOp::BitOr, $1, $3); }
	;
and_value_expression:
 	bitor_value_expression									{ $$ = $1; }
 	| and_value_expression AND bitor_value_expression		{ $$ = new BinaryExpr(BinOp::LogicAnd, $1, $3); }
	;
or_value_expression:
 	and_value_expression									{ $$ = $1; }
 	| or_value_expression OR and_value_expression			{ $$ = new BinaryExpr(BinOp::LogicOr, $1, $3); }
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
	| BITOREQ	{ Push(Add(Generic::Type::OpBitOrEq)); }
	| BITANDEQ	{ Push(Add(Generic::Type::OpBitAndEq)); }
	| BITXOREQ	{ Push(Add(Generic::Type::OpBitXorEq)); }
	| OREQ		{ Push(Add(Generic::Type::OpOrEq)); }
	| ANDEQ		{ Push(Add(Generic::Type::OpAndEq)); }
	| POWEQ		{ Push(Add(Generic::Type::OpPowEq)); }
	| SHLEQ		{ Push(Add(Generic::Type::OpASLEq)); }
	| ASREQ		{ Push(Add(Generic::Type::OpASREq)); }
	| LSREQ		{ Push(Add(Generic::Type::OpLSREq)); }
	;
assign_value_expression:
	or_value_expression											{ $$ = $1; }
//	| or_value_expression assign_operator or_value_expression	{ Node *n = Pop(), *m = Pop(); Push(SetChildren((Generic*)m, Pop(), n)); }
	;

value_expression:
	assign_value_expression		{ $$ = $1; }
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
	| I128	{ $$ = new PrimitiveType(PrimType::i128); }
	| U128	{ $$ = new PrimitiveType(PrimType::u128); }
	| IZ	{ $$ = new PrimitiveType(PrimType::iz); }
	| UZ	{ $$ = new PrimitiveType(PrimType::uz); }
	| F16	{ $$ = new PrimitiveType(PrimType::f16); }
	| F32	{ $$ = new PrimitiveType(PrimType::f32); }
	| F64	{ $$ = new PrimitiveType(PrimType::f64); }
	| F128	{ $$ = new PrimitiveType(PrimType::f128); }
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
