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

%union {
	int64_t ival;
	double fval;
	const char *sval;
	Node *node;
	AmbiguousExpr *ambiguous;
	Expr *expr;
	ExprList exprList;
	TypeExpr *type;
	TypeExprList typeExprList;
	Statement *statement;
	StatementList statementList;
	ValDecl *decl;
	DeclList declList;
	UnaryOp unaryOp;
	BinOp binaryOp;
	PtrType ptrType;
}

%token MODULE STATIC
%token DEF VAR
%token FN STRUCT
%token CONST
%token IF THEN ELSE FOR FOREACH DO WHILE MATCH RETURN BREAK CAST
%token ELIPSIS IS ISNOT ARROW IMPLY SLICE INCOP DECOP SHL ASR LSR EQ NEQ GEQ LEQ AND OR POW BIND
%token MULEQ DIVEQ MODEQ ADDEQ SUBEQ CONCATEQ BITOREQ BITANDEQ BITXOREQ OREQ ANDEQ POWEQ SHLEQ ASREQ LSREQ

%token VOID U1 I8 U8 C8 I16 U16 C16 I32 U32 C32 I64 U64 I128 U128 IZ UZ F16 F32 F64 F128

%token <ival> INT CHAR BOOL NUL
%token <fval> FLOAT
%token <sval> STRING IDENTIFIER

%type <node> any_postfix

%type <ambiguous> member
%type <ambiguous> unknown unknown_primary unknown_postfix unknown_prefix unknown_infix1 unknown_infix2 unknown_infix3 unknown_infix4 unknown_infix5 unknown_infix6 unknown_infix7 unknown_infix8 unknown_infix9 unknown_infix10 unknown_infix11 unknown_infix12

%type <expr> literal function_literal_inner function_literal lambda call unary
%type <expr> known_value value known_value_primary known_value_postfix value_postfix known_value_prefix value_prefix known_value_infix1 value_infix1 known_value_infix2 value_infix2 known_value_infix3 value_infix3 known_value_infix4 value_infix4 known_value_infix5 value_infix5 known_value_infix6 value_infix6 known_value_infix7 value_infix7 known_value_infix8 value_infix8 known_value_infix9 value_infix9 known_value_infix10 value_infix10 known_value_infix11 value_infix11 known_value_infix12 value_infix12 value_assign
%type <exprList> value_list assign_value_list parameters

%type <type> primitive struct struct_def ref
%type <type> known_type type known_type_primary known_type_postfix type_postfix known_type_prefix known_type_infix1 known_type_infix2 known_type_infix3 known_type_infix4 known_type_infix5 known_type_infix6 known_type_infix7 known_type_infix8 known_type_infix9 known_type_infix10 known_type_infix11 known_type_infix12
//%type <typeExprList>

%type <statement> module_stmnt struct_stmnt code_stmnt module_statement def_statement var_statement expression_statement empty_statement control_statement
%type <statementList> module_statemtnts struct_statements code_statements body

%type <decl> var_decl var_decl_assign
%type <declList> var_decl_list var_decl_assign_list function_arguments

%type <unaryOp> unary_op
%type <binaryOp> pow_op mul_op add_op shift_op is_op cmp_op eq_op assign_op
%type <ptrType> ptr_type

%nonassoc THEN
%nonassoc ELSE

%glr-parser

%%
mlang	: module_statemtnts								{ parseTree = $1; }

module_statemtnts	: module_stmnt						{ $$ = StatementList::empty(); if ($1) $$ = $$.append($1); }
					| module_statemtnts module_stmnt	{ $$ = $2 ? $1.append($2) : $1; }
module_stmnt		: module_statement					{ $$ = $1; }
					| def_statement						{ $$ = $1; }
					| var_statement						{ $$ = $1; }
					| empty_statement					{ $$ = $1; }
struct_statements	: struct_stmnt						{ $$ = StatementList::empty(); if ($1) $$ = $$.append($1); }
					| struct_statements struct_stmnt	{ $$ = $2 ? $1.append($2) : $1; }
struct_stmnt		: def_statement						{ $$ = $1; }
					| var_statement						{ $$ = $1; }
					| empty_statement					{ $$ = $1; }
code_statements		: code_stmnt						{ $$ = StatementList::empty(); if ($1) $$ = $$.append($1); }
					| code_statements code_stmnt		{ $$ = $2 ? $1.append($2) : $1; }
code_stmnt			: def_statement						{ $$ = $1; }
					| var_statement						{ $$ = $1; }
					| expression_statement				{ $$ = $1; }
					| control_statement					{ $$ = $1; }
					| empty_statement					{ $$ = $1; }

var_decl			: IDENTIFIER								{ $$ = new VarDecl($1, nullptr, nullptr); }
					| IDENTIFIER ':' type						{ $$ = new VarDecl($1, $3, nullptr); }
var_decl_assign		: var_decl									{ $$ = $1; }
					| IDENTIFIER ':' type '=' value				{ $$ = new VarDecl($1, $3, $5); }
					| IDENTIFIER '=' value						{ $$ = new VarDecl($1, nullptr, $3); }

var_decl_list		: var_decl									{ $$ = DeclList::empty().append($1); }
					| var_decl_list ',' var_decl				{ $$ = $1.append($3); }
var_decl_assign_list: var_decl_assign							{ $$ = DeclList::empty().append($1); }
					| var_decl_assign_list ',' var_decl_assign	{ $$ = $1.append($3); }

value_list			: value										{ $$ = ExprList::empty().append($1); }
					| value_list ',' value						{ $$ = $1.append($3); }
assign_value_list	: value_assign								{ $$ = ExprList::empty().append($1); }
					| assign_value_list ',' value_assign		{ $$ = $1.append($3); }
parameters			: '(' ')'									{ $$ = ExprList::empty(); }
					| '(' value_list ')'						{ $$ = $2; }
function_arguments	: '(' ')'									{ $$ = DeclList::empty(); }
					| '(' var_decl_assign_list ')'				{ $$ = $2; }
body				: code_stmnt								{ $$ = StatementList::empty(); if ($1) $$ = $$.append($1); }
					| '{' '}'									{ $$ = StatementList::empty(); }
					| '{' code_statements '}'					{ $$ = $2; }


/**** STATEMENTS ****/

empty_statement			: ';'																	{ $$ = nullptr; }
module_statement		: MODULE IDENTIFIER ';'													{ $$ = new ModuleStatement($2); }
def_statement			: DEF IDENTIFIER ':' type ';'											{ $$ = new TypeDecl($2, $4); }
						| DEF IDENTIFIER ':' type '=' value ';'									{ $$ = new ValDecl($2, $4, $6); }
						| DEF IDENTIFIER '=' value ';'											{ $$ = new ValDecl($2, nullptr, $4); }
						| FN IDENTIFIER function_literal_inner									{ $$ = new ValDecl($2, nullptr, $3); }
						| STRUCT IDENTIFIER struct_def											{ $$ = new TypeDecl($2, $3); }
var_statement			: VAR var_decl_assign ';'												{ $$ = $2; }
expression_statement	: value_assign ';'														{ $$ = new ExpressionStatement($1); }
control_statement		: RETURN ';'															{ $$ = new ReturnStatement(nullptr); }
						| RETURN value ';'														{ $$ = new ReturnStatement($2); }
//						| BREAK ';'
						| IF '(' value ')' body ELSE body										{ $$ = new IfStatement($3, new ScopeStatement($5), new ScopeStatement($7)); }
						| IF '(' value ')' body										%prec THEN	{ $$ = new IfStatement($3, new ScopeStatement($5), nullptr); }
						| IF '(' var_decl_assign_list ';' value ')' body ELSE body				{ $$ = new IfStatement($5, new ScopeStatement($7), new ScopeStatement($9), $3); }
						| IF '(' var_decl_assign_list ';' value ')' body			%prec THEN	{ $$ = new IfStatement($5, new ScopeStatement($7), nullptr, $3); }
						| DO body																{ $$ = new LoopStatement(DeclList::empty(), nullptr, ExprList::empty(), new ScopeStatement($2)); }
						| WHILE '(' value ')' body												{ $$ = new LoopStatement(DeclList::empty(), $3, ExprList::empty(), new ScopeStatement($5)); }
						| FOR '('                      ';'       ';'                   ')' body	{ $$ = new LoopStatement(DeclList::empty(), nullptr, ExprList::empty(), new ScopeStatement($6)); }
						| FOR '('                      ';'       ';' assign_value_list ')' body	{ $$ = new LoopStatement(DeclList::empty(), nullptr, $5, new ScopeStatement($7)); }
						| FOR '('                      ';' value ';'                   ')' body	{ $$ = new LoopStatement(DeclList::empty(), $4, ExprList::empty(), new ScopeStatement($7)); }
						| FOR '('                      ';' value ';' assign_value_list ')' body	{ $$ = new LoopStatement(DeclList::empty(), $4, $6, new ScopeStatement($8)); }
						| FOR '(' var_decl_assign_list ';'       ';'                   ')' body	{ $$ = new LoopStatement($3, nullptr, ExprList::empty(), new ScopeStatement($7)); }
						| FOR '(' var_decl_assign_list ';'       ';' assign_value_list ')' body	{ $$ = new LoopStatement($3, nullptr, $6, new ScopeStatement($8)); }
						| FOR '(' var_decl_assign_list ';' value ';'                   ')' body	{ $$ = new LoopStatement($3, $5, ExprList::empty(), new ScopeStatement($8)); }
						| FOR '(' var_decl_assign_list ';' value ';' assign_value_list ')' body	{ $$ = new LoopStatement($3, $5, $7, new ScopeStatement($9)); }
						| FOREACH '(' value ')' body											{ $$ = makeForEach(DeclList::empty(), $3, new ScopeStatement($5)); }
						| FOREACH '(' var_decl_list ';' value ')' body							{ $$ = makeForEach($3, $5, new ScopeStatement($7)); }
//						| MATCH body


/**** EXPRESSION PRODUCTIONS ****/

literal		: NUL		{ $$ = new PrimitiveLiteralExpr(SizeT_Type, 0ull); }
			| INT		{ $$ = new PrimitiveLiteralExpr($1); }
			| FLOAT		{ $$ = new PrimitiveLiteralExpr($1); }
			| CHAR		{ $$ = new PrimitiveLiteralExpr((char32_t)$1); }
			| BOOL		{ $$ = new PrimitiveLiteralExpr((bool)$1); }
//			| STRING	{ $$ = new PrimitiveLiteralExpr($1); }

primitive	: VOID		{ $$ = PrimitiveType::get(PrimType::v); }
			| U1		{ $$ = PrimitiveType::get(PrimType::u1); }
			| I8		{ $$ = PrimitiveType::get(PrimType::i8); }
			| U8		{ $$ = PrimitiveType::get(PrimType::u8); }
			| I16		{ $$ = PrimitiveType::get(PrimType::i16); }
			| U16		{ $$ = PrimitiveType::get(PrimType::u16); }
			| I32		{ $$ = PrimitiveType::get(PrimType::i32); }
			| U32		{ $$ = PrimitiveType::get(PrimType::u32); }
			| I64		{ $$ = PrimitiveType::get(PrimType::i64); }
			| U64		{ $$ = PrimitiveType::get(PrimType::u64); }
			| I128		{ $$ = PrimitiveType::get(PrimType::i128); }
			| U128		{ $$ = PrimitiveType::get(PrimType::u128); }
			| IZ		{ $$ = PrimitiveType::get(SSizeT_Type); }
			| UZ		{ $$ = PrimitiveType::get(SizeT_Type); }
			| C8		{ $$ = PrimitiveType::get(PrimType::c8); }
			| C16		{ $$ = PrimitiveType::get(PrimType::c16); }
			| C32		{ $$ = PrimitiveType::get(PrimType::c32); }
			| F16		{ $$ = PrimitiveType::get(PrimType::f16); }
			| F32		{ $$ = PrimitiveType::get(PrimType::f32); }
			| F64		{ $$ = PrimitiveType::get(PrimType::f64); }
			| F128		{ $$ = PrimitiveType::get(PrimType::f128); }

unary_op	: '+'		{ $$ = UnaryOp::Pos; }
			| '-'		{ $$ = UnaryOp::Neg; }
			| '~'		{ $$ = UnaryOp::BitNot; }
			| '!'		{ $$ = UnaryOp::LogicNot; }
//			| '#'		{ $$ = UnaryOp::Length; }
//			| INCOP		{ $$ = UnaryOp::PreInc; }
//			| DECOP		{ $$ = UnaryOp::PreDec; }
pow_op		: POW		{ $$ = BinOp::Pow; }
mul_op		: '*'		{ $$ = BinOp::Mul; }
			| '/'		{ $$ = BinOp::Div; }
			| '%'		{ $$ = BinOp::Mod; }
add_op		: '+'		{ $$ = BinOp::Add; }
			| '-'		{ $$ = BinOp::Sub; }
			| '~'		{ $$ = BinOp::Cat; }
shift_op	: SHL		{ $$ = BinOp::SHL; }
			| ASR		{ $$ = BinOp::ASR; }
			| LSR		{ $$ = BinOp::LSR; }
is_op		: IS		{ $$ = BinOp::Is; }
			| ISNOT		{ $$ = BinOp::IsNot; }
cmp_op		: '<'		{ $$ = BinOp::Lt; }
			| '>'		{ $$ = BinOp::Gt; }
			| LEQ		{ $$ = BinOp::Le; }
			| GEQ		{ $$ = BinOp::Ge; }
eq_op		: EQ		{ $$ = BinOp::Eq; }
			| NEQ		{ $$ = BinOp::Ne; }
assign_op	: '='		{ $$ = BinOp::None; }
			| POWEQ		{ $$ = BinOp::Pow; }
			| MULEQ		{ $$ = BinOp::Mul; }
			| DIVEQ		{ $$ = BinOp::Div; }
			| MODEQ		{ $$ = BinOp::Mod; }
			| ADDEQ		{ $$ = BinOp::Add; }
			| SUBEQ		{ $$ = BinOp::Sub; }
			| CONCATEQ	{ $$ = BinOp::Cat; }
			| BITOREQ	{ $$ = BinOp::BitOr; }
			| BITANDEQ	{ $$ = BinOp::BitAnd; }
			| BITXOREQ	{ $$ = BinOp::BitXor; }
			| OREQ		{ $$ = BinOp::LogicOr; }
			| ANDEQ		{ $$ = BinOp::LogicAnd; }
			| SHLEQ		{ $$ = BinOp::SHL; }
			| ASREQ		{ $$ = BinOp::ASR; }
			| LSREQ		{ $$ = BinOp::LSR; }

ptr_type	: '*'		{ $$ = PtrType::RawPtr; }
			| '^'		{ $$ = PtrType::UniquePtr; }
			| '&'		{ $$ = PtrType::BorrowedPtr; }

struct_def				: '{' '}'												{ $$ = new Struct(StatementList::empty()); }
						| '{' struct_statements '}'								{ $$ = new Struct($2); }
struct					: STRUCT struct_def										{ $$ = $2; }

function_literal_inner	: function_arguments '{' '}'							{ $$ = new FunctionLiteralExpr(StatementList::empty(), $1, nullptr); }
						| function_arguments '{' code_statements '}'			{ $$ = new FunctionLiteralExpr($3, $1, nullptr); }
						| function_arguments ':' type '{' code_statements '}'	{ $$ = new FunctionLiteralExpr($5, $1, $3); }
function_literal		: FN function_literal_inner								{ $$ = $2; }

lambda	: FN IMPLY value					{ $$ = new FunctionLiteralExpr(StatementList::empty().append(new ReturnStatement($3)), DeclList::empty(), nullptr); }
		| FN var_decl_list IMPLY value		{ $$ = new FunctionLiteralExpr(StatementList::empty().append(new ReturnStatement($4)), $2, nullptr); }

member	: any_postfix '.' IDENTIFIER		{ $$ = new MemberLookup($1, $3); }

call	: value_postfix parameters			{ $$ = new CallExpr($1, $2); }

ref		: type_postfix ptr_type				{ $$ = new PointerType($2, $1); }

unary	: unary_op value_prefix				{ $$ = new UnaryExpr($1, $2); }
		| CAST '(' type ')' value_prefix	{ $$ = new TypeConvertExpr($5, $3, false); }


/*** primaries ***/

unknown_primary		: IDENTIFIER			{ $$ = new Identifier($1); }
					| '(' unknown ')'		{ $$ = $2; }
known_value_primary	: literal				{ $$ = $1; }
					| function_literal		{ $$ = $1; }
					| '(' known_value ')'	{ $$ = $2; }
known_type_primary	: primitive				{ $$ = $1; }
					| struct				{ $$ = $1; }
					| '(' known_type ')'	{ $$ = $2; }


/*** postfix ***/

unknown_postfix		: unknown_primary		{ $$ = $1; }
					| member				{ $$ = $1; }
known_value_postfix	: known_value_primary	{ $$ = $1; }
					| call					{ $$ = $1; }
known_type_postfix	: known_type_primary	{ $$ = $1; }
					| ref					{ $$ = $1; }

value_postfix		: known_value_postfix	{ $$ = $1; }
					| unknown_postfix		{ $$ = $1; /* Check/fix category */ }
type_postfix		: known_type_postfix	{ $$ = $1; }
					| unknown_postfix		{ $$ = $1; /* Check/fix category */ }
any_postfix			: known_value_postfix	{ $$ = $1; }
					| known_type_postfix	{ $$ = $1; }
					| unknown_postfix		{ $$ = $1; }


/*** prefix ***/

unknown_prefix		: unknown_postfix		{ $$ = $1; }
known_value_prefix	: known_value_postfix	{ $$ = $1; }
					| unary					{ $$ = $1; }
known_type_prefix	: known_type_postfix	{ $$ = $1; }

value_prefix		: known_value_prefix	{ $$ = $1; }
					| unknown_prefix		{ $$ = $1; /* Check/fix category */ }


/*** infix1 ***/

unknown_infix1		: unknown_prefix					{ $$ = $1; }
known_value_infix1	: known_value_prefix				{ $$ = $1; }
					| value_infix1 pow_op value_prefix	{ $$ = new BinaryExpr($2, $1, $3); }
known_type_infix1	: known_type_prefix					{ $$ = $1; }

value_infix1		: known_value_infix1				{ $$ = $1; }
					| unknown_infix1					{ $$ = $1; /* Check/fix category */ }

/*** infix2 ***/

unknown_infix2		: unknown_infix1					{ $$ = $1; }
known_value_infix2	: known_value_infix1				{ $$ = $1; }
					| value_infix2 mul_op value_infix1	{ $$ = new BinaryExpr($2, $1, $3); }
known_type_infix2	: known_type_infix1					{ $$ = $1; }

value_infix2		: known_value_infix2				{ $$ = $1; }
					| unknown_infix2					{ $$ = $1; /* Check/fix category */ }

/*** infix3 ***/

unknown_infix3		: unknown_infix2					{ $$ = $1; }
known_value_infix3	: known_value_infix2				{ $$ = $1; }
					| value_infix3 add_op value_infix2	{ $$ = new BinaryExpr($2, $1, $3); }
known_type_infix3	: known_type_infix2					{ $$ = $1; }

value_infix3		: known_value_infix3				{ $$ = $1; }
					| unknown_infix3					{ $$ = $1; /* Check/fix category */ }

/*** infix4 ***/

unknown_infix4		: unknown_infix3					{ $$ = $1; }
known_value_infix4	: known_value_infix3				{ $$ = $1; }
					| value_infix4 shift_op value_infix3{ $$ = new BinaryExpr($2, $1, $3); }
known_type_infix4	: known_type_infix3					{ $$ = $1; }

value_infix4		: known_value_infix4				{ $$ = $1; }
					| unknown_infix4					{ $$ = $1; /* Check/fix category */ }

/*** infix5 ***/

unknown_infix5		: unknown_infix4					{ $$ = $1; }
known_value_infix5	: known_value_infix4				{ $$ = $1; }
					| value_infix5 is_op value_infix4	{ $$ = new BinaryExpr($2, $1, $3); }
known_type_infix5	: known_type_infix4					{ $$ = $1; }

value_infix5		: known_value_infix5				{ $$ = $1; }
					| unknown_infix5					{ $$ = $1; /* Check/fix category */ }

/*** infix6 ***/

unknown_infix6		: unknown_infix5					{ $$ = $1; }
known_value_infix6	: known_value_infix5				{ $$ = $1; }
					| value_infix6 cmp_op value_infix5	{ $$ = new BinaryExpr($2, $1, $3); }
known_type_infix6	: known_type_infix5					{ $$ = $1; }

value_infix6		: known_value_infix6				{ $$ = $1; }
					| unknown_infix6					{ $$ = $1; /* Check/fix category */ }

/*** infix7 ***/

unknown_infix7		: unknown_infix6					{ $$ = $1; }
known_value_infix7	: known_value_infix6				{ $$ = $1; }
					| value_infix7 eq_op value_infix6	{ $$ = new BinaryExpr($2, $1, $3); }
known_type_infix7	: known_type_infix6					{ $$ = $1; }

value_infix7		: known_value_infix7				{ $$ = $1; }
					| unknown_infix7					{ $$ = $1; /* Check/fix category */ }

/*** infix8 ***/

unknown_infix8		: unknown_infix7					{ $$ = $1; }
known_value_infix8	: known_value_infix7				{ $$ = $1; }
					| value_infix8 '&' value_infix7		{ $$ = new BinaryExpr(BinOp::BitAnd, $1, $3); }
known_type_infix8	: known_type_infix7					{ $$ = $1; }

value_infix8		: known_value_infix8				{ $$ = $1; }
					| unknown_infix8					{ $$ = $1; /* Check/fix category */ }

/*** infix9 ***/

unknown_infix9		: unknown_infix8					{ $$ = $1; }
known_value_infix9	: known_value_infix8				{ $$ = $1; }
					| value_infix9 '^' value_infix8		{ $$ = new BinaryExpr(BinOp::BitXor, $1, $3); }
known_type_infix9	: known_type_infix8					{ $$ = $1; }

value_infix9		: known_value_infix9				{ $$ = $1; }
					| unknown_infix9					{ $$ = $1; /* Check/fix category */ }

/*** infix10 ***/

unknown_infix10		: unknown_infix9					{ $$ = $1; }
known_value_infix10	: known_value_infix9				{ $$ = $1; }
					| value_infix10 '|' value_infix9	{ $$ = new BinaryExpr(BinOp::BitOr, $1, $3); }
known_type_infix10	: known_type_infix9					{ $$ = $1; }

value_infix10		: known_value_infix10				{ $$ = $1; }
					| unknown_infix10					{ $$ = $1; /* Check/fix category */ }

/*** infix11 ***/

unknown_infix11		: unknown_infix10					{ $$ = $1; }
known_value_infix11	: known_value_infix10				{ $$ = $1; }
					| value_infix11 AND value_infix10	{ $$ = new BinaryExpr(BinOp::LogicAnd, $1, $3); }
known_type_infix11	: known_type_infix10				{ $$ = $1; }

value_infix11		: known_value_infix11				{ $$ = $1; }
					| unknown_infix11					{ $$ = $1; /* Check/fix category */ }

/*** infix12 ***/

unknown_infix12		: unknown_infix11					{ $$ = $1; }
known_value_infix12	: known_value_infix11				{ $$ = $1; }
					| value_infix12 OR value_infix11	{ $$ = new BinaryExpr(BinOp::LogicOr, $1, $3); }
known_type_infix12	: known_type_infix11				{ $$ = $1; }

value_infix12		: known_value_infix12				{ $$ = $1; }
					| unknown_infix12					{ $$ = $1; /* Check/fix category */ }

/*** heads ***/

unknown			: unknown_infix12				{ $$ = $1; }
known_value		: known_value_infix12			{ $$ = $1; }
				| lambda						{ $$ = $1; }
known_type		: known_type_infix12			{ $$ = $1; }

value			: known_value					{ $$ = $1; }
				| unknown						{ $$ = $1; /* Check/fix category */ }
type			: known_type					{ $$ = $1; }
				| unknown						{ $$ = $1; /* Check/fix category */ }
//any_expr		: known_value					{ $$ = $1; }
//				| known_type					{ $$ = $1; }
//				| unknown						{ $$ = $1; }

/*** assign ***/

value_assign	: value							{ $$ = $1; }
				| value_postfix assign_op value	{ $$ = new AssignExpr($1, $3, $2); }
				| value_postfix BIND value		{ $$ = new BindExpr($1, $3); }

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
