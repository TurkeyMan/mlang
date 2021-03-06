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

#include "mlang.h"

// WTF windows.h!!!
// THIS is why M needs to exist!
#undef min
#undef max
#undef CONST
#undef VOID

#include "parse.h"
#include "arrayholder.h"
#include "error.h"

using namespace m;

static Array<Statement*> parseTree;
static String filename;

%}

%union {
	int64_t ival;
	double fval;
	const char *sval;
	m::Node *node;
	NodeList nodeList;
	m::AmbiguousExpr *ambiguous;
	m::Expr *expr;
	ExprList exprList;
	m::TypeExpr *type;
	TypeExprList typeExprList;
	m::Statement *statement;
	StatementList statementList;
	m::ScopeStatement *scopeStmnt;
	m::VarDecl *decl;
	DeclList declList;
	m::UnaryOp unaryOp;
	m::BinOp binaryOp;
	m::PtrType ptrType;
}

%token PRAGMA
%token MODULE IMPORT
%token DEF VAR
%token FN STRUCT
%token STATIC CONST
%token IF THEN ELSE FOR FOREACH DO WHILE MATCH RETURN BREAK CAST
%token ELIPSIS IS ISNOT ARROW IMPLY SLICE INCOP DECOP SHL ASR LSR EQ NEQ GEQ LEQ AND OR POW BIND
%token MULEQ DIVEQ MODEQ ADDEQ SUBEQ CONCATEQ BITOREQ BITANDEQ BITXOREQ OREQ ANDEQ POWEQ SHLEQ ASREQ LSREQ

%token VOID U1 I8 U8 C8 I16 U16 C16 I32 U32 C32 I64 U64 I128 U128 IZ UZ CZ F16 F32 F64 F128

%token <ival> INTEGER BOOL_T NUL
%token <fval> FLOATING
%token <sval> STRING CHARACTER IDENTIFIER

%type <sval> module_name

%type <node> any_postfix any

%type <nodeList> unknown_list attribs attributes

%type <ambiguous> member
%type <ambiguous> unknown unknown_primary unknown_postfix unknown_prefix unknown_infix1 unknown_infix2 unknown_infix3 unknown_infix4 unknown_infix5 unknown_infix6 unknown_infix7 unknown_infix8 unknown_infix9 unknown_infix10 unknown_infix11 unknown_infix12

%type <expr> literal function_literal_inner function_literal lambda call unary
%type <expr> known_value value slice value_or_slice known_value_primary known_value_postfix value_postfix known_value_prefix value_prefix known_value_infix1 value_infix1 known_value_infix2 value_infix2 known_value_infix3 value_infix3 known_value_infix4 value_infix4 known_value_infix5 value_infix5 known_value_infix6 value_infix6 known_value_infix7 value_infix7 known_value_infix8 value_infix8 known_value_infix9 value_infix9 known_value_infix10 value_infix10 known_value_infix11 value_infix11 known_value_infix12 value_infix12 value_assign
%type <exprList> value_list value_or_slice_list known_value_list assign_value_list parameters array

%type <type> primitive struct function struct_def ref
%type <type> known_type type known_type_primary known_type_postfix type_postfix known_type_prefix known_type_infix1 known_type_infix2 known_type_infix3 known_type_infix4 known_type_infix5 known_type_infix6 known_type_infix7 known_type_infix8 known_type_infix9 known_type_infix10 known_type_infix11 known_type_infix12
%type <typeExprList> known_type_list  // type_list

%type <statement> module_stmnt struct_stmnt code_stmnt_no_scope code_stmnt module_statement import_statement def_statement var_statement expression_statement empty_statement control_statement
%type <statementList> module_statemtnts struct_statements code_statements

%type <scopeStmnt> scope_statement body

%type <decl> var_decl var_decl_assign var_decl_assign_void
%type <declList> var_decl_list var_decl_assign_list function_arguments

%type <unaryOp> unary_op
%type <binaryOp> pow_op mul_op add_op shift_op is_op cmp_op eq_op assign_op
%type <ptrType> ptr_type

%nonassoc THEN
%nonassoc ELSE

%glr-parser

%%
mlang	: module_statemtnts								{ parseTree = $1.get(); }

module_statemtnts	: module_stmnt						{ $$ = StatementList::empty(); if ($1) $$ = $$.append($1); }
					| module_statemtnts module_stmnt	{ $$ = $2 ? $1.append($2) : $1; }
module_stmnt		: module_statement					{ $$ = $1; }
					| import_statement					{ $$ = $1; }
					| def_statement						{ $$ = $1; }
					| var_statement						{ $$ = $1; }
					| empty_statement					{ $$ = $1; }
struct_statements	: struct_stmnt						{ $$ = StatementList::empty(); if ($1) $$ = $$.append($1); }
					| struct_statements struct_stmnt	{ $$ = $2 ? $1.append($2) : $1; }
struct_stmnt		: def_statement						{ $$ = $1; }
					| var_statement						{ $$ = $1; }
					| import_statement					{ $$ = $1; }
					| empty_statement					{ $$ = $1; }
code_statements		: code_stmnt						{ $$ = StatementList::empty(); if ($1) $$ = $$.append($1); }
					| code_statements code_stmnt		{ $$ = $2 ? $1.append($2) : $1; }
code_stmnt_no_scope	: def_statement						{ $$ = $1; }
					| var_statement						{ $$ = $1; }
					| expression_statement				{ $$ = $1; }
					| control_statement					{ $$ = $1; }
					| import_statement					{ $$ = $1; }
					| empty_statement					{ $$ = $1; }
code_stmnt			: code_stmnt_no_scope				{ $$ = $1; }
					| scope_statement					{ $$ = $1; }

var_decl			: IDENTIFIER								{ $$ = new VarDecl($1, nullptr, nullptr, nullptr, SourceLocation(yylineno)); }
					| IDENTIFIER ':' type						{ $$ = new VarDecl($1, $3, nullptr, nullptr, SourceLocation(yylineno)); }
var_decl_assign		: var_decl									{ $$ = $1; }
					| IDENTIFIER ':' type '=' value				{ $$ = new VarDecl($1, $3, $5, nullptr, SourceLocation(yylineno)); }
					| IDENTIFIER '=' value						{ $$ = new VarDecl($1, nullptr, $3, nullptr, SourceLocation(yylineno)); }
var_decl_assign_void: var_decl_assign							{ $$ = $1; }
					| IDENTIFIER ':' type '=' VOID				{ $$ = new VarDecl($1, $3, new PrimitiveLiteralExpr(PrimType::v, 0ull, SourceLocation(yylineno)), nullptr, SourceLocation(yylineno)); }

var_decl_list		: var_decl									{ $$ = DeclList::empty().append($1); }
					| var_decl_list ',' var_decl				{ $$ = $1.append($3); }
var_decl_assign_list: var_decl_assign							{ $$ = DeclList::empty().append($1); }
					| var_decl_assign_list ',' var_decl_assign	{ $$ = $1.append($3); }

//decl_attr			: EXTERN
//					| PUBLIC
//					| PRIVATE
//					| EXTERN '(' IDENTIFIER ')'
//decl_attrs		: decl_attr
//					| decl_attrs decl_attr


attribs				: any										{ $$ = NodeList::empty().append($1); }
					| attribs ',' any							{ $$ = $1.append($3); }
attributes			: '@' IDENTIFIER							{ $$ = NodeList::empty().append(new Identifier($2, SourceLocation(yylineno))); }
					| '@' '[' attribs ']'						{ $$ = $3; }
					| attributes '@' IDENTIFIER					{ $$ = $1.append(new Identifier($3, SourceLocation(yylineno))); }
					| attributes '@' '[' attribs ']'			{ $$ = $1.append($4); }

module_name			: IDENTIFIER								{ $$ = $1; }
					| module_name '.' IDENTIFIER				{ $$ = join_strings($1, $3); }

unknown_list		: unknown									{ $$ = NodeList::empty().append($1); }
					| unknown_list ',' unknown					{ $$ = $1.append($3); }
					| unknown_list ',' known_value				{ $$ = $1.append($3); }
					| unknown_list ',' known_type				{ $$ = $1.append($3); }
					| known_value_list ',' unknown				{ $$ = NodeList::create(Array<Node*>(Concat, $1.get().slice(), $3)); }
					| known_value_list ',' known_type			{ $$ = NodeList::create(Array<Node*>(Concat, $1.get().slice(), $3)); }
					| known_type_list ',' unknown				{ $$ = NodeList::create(Array<Node*>(Concat, $1.get().slice(), $3)); }
					| known_type_list ',' known_value			{ $$ = NodeList::create(Array<Node*>(Concat, $1.get().slice(), $3)); }
//type_list			: type										{ $$ = TypeExprList::empty().append($1); }
//					| type_list ',' type						{ $$ = $1.append($3); }
value_list			: value										{ $$ = ExprList::empty().append($1); }
					| value_list ',' value						{ $$ = $1.append($3); }
slice				: value SLICE value							{ $$ = new SliceExpr($1, $3, SourceLocation(yylineno)); }
value_or_slice		: value										{ $$ = $1; }
					| slice										{ $$ = $1; }
value_or_slice_list	: value_or_slice							{ $$ = ExprList::empty().append($1); }
					| value_or_slice_list ',' value_or_slice	{ $$ = $1.append($3); }
known_type_list		: known_type								{ $$ = TypeExprList::empty().append($1); }
					| known_type_list ',' known_type			{ $$ = $1.append($3); }
known_value_list	: known_value								{ $$ = ExprList::empty().append($1); }
					| known_value_list ',' known_value			{ $$ = $1.append($3); }
assign_value_list	: value_assign								{ $$ = ExprList::empty().append($1); }
					| assign_value_list ',' value_assign		{ $$ = $1.append($3); }
parameters			: '(' ')'									{ $$ = ExprList::empty(); }
					| '(' value_list ')'						{ $$ = $2; }
function_arguments	: '(' ')'									{ $$ = DeclList::empty(); }
					| '(' var_decl_assign_list ')'				{ $$ = $2; }
					| '(' var_decl_assign_list ',' ELIPSIS ')'	{ $$ = $2.append(new VarDecl("va_args", new CVarArgType(SourceLocation(yylineno)), nullptr, nullptr, SourceLocation(yylineno))); }
array				: '[' ']'									{ $$ = ExprList::empty(); }
					| '[' value_or_slice_list ']'				{ $$ = $2; }
body				: code_stmnt_no_scope						{ $$ = new ScopeStatement($1 ? Array<Statement*>{ $1 } : Array<Statement*>(), nullptr, SourceLocation(yylineno)); }
					| scope_statement							{ $$ = $1; }

/**** STATEMENTS ****/

empty_statement			: ';'																	{ $$ = nullptr; }
module_statement		: MODULE module_name ';'												{ $$ = new ModuleDecl($2, nullptr, SourceLocation(yylineno)); }
						| attributes MODULE module_name ';'										{ $$ = new ModuleDecl($3, $1.get(), SourceLocation(yylineno)); }
import_statement		: IMPORT module_name ';'												{ $$ = new ImportDecl($2, nullptr, SourceLocation(yylineno)); }
						| attributes IMPORT module_name ';'										{ $$ = new ImportDecl($3, $1.get(), SourceLocation(yylineno)); }
def_statement			: DEF IDENTIFIER ':' type ';'											{ $$ = new TypeDecl($2, $4, nullptr, SourceLocation(yylineno)); }
						| DEF IDENTIFIER ':' type '=' value ';'									{ $$ = new ValDecl($2, $4, $6, nullptr, SourceLocation(yylineno)); }
						| DEF IDENTIFIER ':' type '=' VOID ';'									{ $$ = new ValDecl($2, $4, new PrimitiveLiteralExpr(PrimType::v, 0ull, SourceLocation(yylineno)), nullptr, SourceLocation(yylineno)); }
						| DEF IDENTIFIER '=' value ';'											{ $$ = new ValDecl($2, nullptr, $4, nullptr, SourceLocation(yylineno)); }
						| FN IDENTIFIER function_literal_inner									{ $$ = new ValDecl($2, nullptr, $3, nullptr, SourceLocation(yylineno)); }
						| FN IDENTIFIER function_arguments ';'									{ $$ = nullptr; error(filename.c_str(), yylineno, "function prototype must have explicit return type"); }
						| FN IDENTIFIER function_arguments ':' type ';'							{ $$ = new ValDecl($2, new ::FunctionType($5, $3.get(), SourceLocation(yylineno)), new PrimitiveLiteralExpr(PrimType::v, 0ull, SourceLocation(yylineno)), nullptr, SourceLocation(yylineno)); }
						| STRUCT IDENTIFIER struct_def											{ $$ = new TypeDecl($2, $3, nullptr, SourceLocation(yylineno)); }
						| attributes DEF IDENTIFIER ':' type ';'								{ $$ = new TypeDecl($3, $5, $1.get(), SourceLocation(yylineno)); }
						| attributes DEF IDENTIFIER ':' type '=' value ';'						{ $$ = new ValDecl($3, $5, $7, $1.get(), SourceLocation(yylineno)); }
						| attributes DEF IDENTIFIER ':' type '=' VOID ';'						{ $$ = new ValDecl($3, $5, new PrimitiveLiteralExpr(PrimType::v, 0ull, SourceLocation(yylineno)), $1.get(), SourceLocation(yylineno)); }
						| attributes DEF IDENTIFIER '=' value ';'								{ $$ = new ValDecl($3, nullptr, $5, $1.get(), SourceLocation(yylineno)); }
						| attributes FN IDENTIFIER function_literal_inner						{ $$ = new ValDecl($3, nullptr, $4, $1.get(), SourceLocation(yylineno)); }
						| attributes FN IDENTIFIER function_arguments ';'						{ $$ = nullptr; error(filename.c_str(), yylineno, "function prototype must have explicit return type"); }
						| attributes FN IDENTIFIER function_arguments ':' type ';'				{ $$ = new ValDecl($3, new ::FunctionType($6, $4.get(), SourceLocation(yylineno)), new PrimitiveLiteralExpr(PrimType::v, 0ull, SourceLocation(yylineno)), $1.get(), SourceLocation(yylineno)); }
						| attributes STRUCT IDENTIFIER struct_def								{ $$ = new TypeDecl($3, $4, $1.get(), SourceLocation(yylineno)); }
var_statement			: VAR var_decl_assign_void ';'											{ $$ = $2; }
						| attributes VAR var_decl_assign_void ';'								{ $3->attributes().append($1.get()); $$ = $3; }
expression_statement	: value_assign ';'														{ $$ = new ExpressionStatement($1, SourceLocation(yylineno)); }
control_statement		: RETURN ';'															{ $$ = new ReturnStatement(nullptr, SourceLocation(yylineno)); }
						| RETURN value ';'														{ $$ = new ReturnStatement($2, SourceLocation(yylineno)); }
//						| BREAK ';'
						| IF '(' value ')' body ELSE body										{ $$ = new IfStatement($3, $5, $7, nullptr, SourceLocation(yylineno)); }
						| IF '(' value ')' body										%prec THEN	{ $$ = new IfStatement($3, $5, nullptr, nullptr, SourceLocation(yylineno)); }
						| IF '(' var_decl_assign_list ';' value ')' body ELSE body				{ $$ = new IfStatement($5, $7, $9, $3.get(), SourceLocation(yylineno)); }
						| IF '(' var_decl_assign_list ';' value ')' body			%prec THEN	{ $$ = new IfStatement($5, $7, nullptr, $3.get(), SourceLocation(yylineno)); }
						| DO body																{ $$ = new LoopStatement(nullptr, nullptr, nullptr, $2, SourceLocation(yylineno)); }
						| WHILE '(' value ')' body												{ $$ = new LoopStatement(nullptr, $3, nullptr, $5, SourceLocation(yylineno)); }
						| FOR '('                      ';'       ';'                   ')' body	{ $$ = new LoopStatement(nullptr, nullptr, nullptr, $6, SourceLocation(yylineno)); }
						| FOR '('                      ';'       ';' assign_value_list ')' body	{ $$ = new LoopStatement(nullptr, nullptr, $5.get(), $7, SourceLocation(yylineno)); }
						| FOR '('                      ';' value ';'                   ')' body	{ $$ = new LoopStatement(nullptr, $4, nullptr, $7, SourceLocation(yylineno)); }
						| FOR '('                      ';' value ';' assign_value_list ')' body	{ $$ = new LoopStatement(nullptr, $4, $6.get(), $8, SourceLocation(yylineno)); }
						| FOR '(' var_decl_assign_list ';'       ';'                   ')' body	{ $$ = new LoopStatement($3.get(), nullptr, nullptr, $7, SourceLocation(yylineno)); }
						| FOR '(' var_decl_assign_list ';'       ';' assign_value_list ')' body	{ $$ = new LoopStatement($3.get(), nullptr, $6.get(), $8, SourceLocation(yylineno)); }
						| FOR '(' var_decl_assign_list ';' value ';'                   ')' body	{ $$ = new LoopStatement($3.get(), $5, nullptr, $8, SourceLocation(yylineno)); }
						| FOR '(' var_decl_assign_list ';' value ';' assign_value_list ')' body	{ $$ = new LoopStatement($3.get(), $5, $7.get(), $9, SourceLocation(yylineno)); }
						| FOREACH '(' value_or_slice ')' body									{ $$ = makeForEach(nullptr, $3, $5, SourceLocation(yylineno)); }
						| FOREACH '(' var_decl_list ';' value_or_slice ')' body					{ $$ = makeForEach($3.get(), $5, $7, SourceLocation(yylineno)); }
//						| MATCH body
scope_statement			: '{' '}'																{ $$ = new ScopeStatement(nullptr, nullptr, SourceLocation(yylineno)); }
						| '{' code_statements '}'												{ $$ = new ScopeStatement($2.get(), nullptr, SourceLocation(yylineno)); }


/**** EXPRESSION PRODUCTIONS ****/

literal		: NUL		{ $$ = new PrimitiveLiteralExpr(SizeT_Type, 0ull, SourceLocation(yylineno)); }
			| INTEGER	{ $$ = new PrimitiveLiteralExpr($1, SourceLocation(yylineno)); }
			| FLOATING	{ $$ = new PrimitiveLiteralExpr($1, SourceLocation(yylineno)); }
			| CHARACTER	{ PrimType pt; char32_t c = parse_char($1, pt); $$ = new PrimitiveLiteralExpr(pt, c, SourceLocation(yylineno)); }
			| BOOL_T	{ $$ = new PrimitiveLiteralExpr((bool)$1, SourceLocation(yylineno)); }
			| STRING	{ $$ = Tuple::makeStringLiteralQuoted($1, SourceLocation(yylineno)); }

primitive	: VOID		{ $$ = PrimitiveType::get(PrimType::v, SourceLocation(yylineno)); }
			| U1		{ $$ = PrimitiveType::get(PrimType::u1, SourceLocation(yylineno)); }
			| I8		{ $$ = PrimitiveType::get(PrimType::i8, SourceLocation(yylineno)); }
			| U8		{ $$ = PrimitiveType::get(PrimType::u8, SourceLocation(yylineno)); }
			| I16		{ $$ = PrimitiveType::get(PrimType::i16, SourceLocation(yylineno)); }
			| U16		{ $$ = PrimitiveType::get(PrimType::u16, SourceLocation(yylineno)); }
			| I32		{ $$ = PrimitiveType::get(PrimType::i32, SourceLocation(yylineno)); }
			| U32		{ $$ = PrimitiveType::get(PrimType::u32, SourceLocation(yylineno)); }
			| I64		{ $$ = PrimitiveType::get(PrimType::i64, SourceLocation(yylineno)); }
			| U64		{ $$ = PrimitiveType::get(PrimType::u64, SourceLocation(yylineno)); }
			| I128		{ $$ = PrimitiveType::get(PrimType::i128, SourceLocation(yylineno)); }
			| U128		{ $$ = PrimitiveType::get(PrimType::u128, SourceLocation(yylineno)); }
			| IZ		{ $$ = PrimitiveType::get(SSizeT_Type, SourceLocation(yylineno)); }
			| UZ		{ $$ = PrimitiveType::get(SizeT_Type, SourceLocation(yylineno)); }
			| CZ		{ $$ = PrimitiveType::get(WCharT_Type, SourceLocation(yylineno)); }
			| C8		{ $$ = PrimitiveType::get(PrimType::c8, SourceLocation(yylineno)); }
			| C16		{ $$ = PrimitiveType::get(PrimType::c16, SourceLocation(yylineno)); }
			| C32		{ $$ = PrimitiveType::get(PrimType::c32, SourceLocation(yylineno)); }
			| F16		{ $$ = PrimitiveType::get(PrimType::f16, SourceLocation(yylineno)); }
			| F32		{ $$ = PrimitiveType::get(PrimType::f32, SourceLocation(yylineno)); }
			| F64		{ $$ = PrimitiveType::get(PrimType::f64, SourceLocation(yylineno)); }
			| F128		{ $$ = PrimitiveType::get(PrimType::f128, SourceLocation(yylineno)); }

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

struct_def				: '{' '}'												{ $$ = new Struct(nullptr, SourceLocation(yylineno)); }
						| '{' struct_statements '}'								{ $$ = new Struct($2.get(), SourceLocation(yylineno)); }
struct					: STRUCT struct_def										{ $$ = $2; }

function				: FN function_arguments									{ $$ = new ::FunctionType(PrimitiveType::get(PrimType::v, SourceLocation(yylineno)), $2.get(), SourceLocation(yylineno)); }
						| FN function_arguments ':' type						{ $$ = new ::FunctionType($4, $2.get(), SourceLocation(yylineno)); }

function_literal_inner	: function_arguments '{' '}'							{ $$ = new FunctionLiteralExpr(nullptr, $1.get(), nullptr, SourceLocation(yylineno)); }
						| function_arguments '{' code_statements '}'			{ $$ = new FunctionLiteralExpr($3.get(), $1.get(), nullptr, SourceLocation(yylineno)); }
						| function_arguments ':' type '{' code_statements '}'	{ $$ = new FunctionLiteralExpr($5.get(), $1.get(), $3, SourceLocation(yylineno)); }
function_literal		: FN function_literal_inner								{ $$ = $2; }

lambda	: FN IMPLY value						{ $$ = new FunctionLiteralExpr(Array<Statement*>{ new ReturnStatement($3, SourceLocation(yylineno)) }, nullptr, nullptr, SourceLocation(yylineno)); }
		| FN var_decl_list IMPLY value			{ $$ = new FunctionLiteralExpr(Array<Statement*>{ new ReturnStatement($4, SourceLocation(yylineno)) }, $2.get(), nullptr, SourceLocation(yylineno)); }

member	: any_postfix '.' IDENTIFIER			{ $$ = new MemberLookup($1, $3, SourceLocation(yylineno)); }

call	: value_postfix parameters				{ $$ = new CallExpr($1, $2.get(), SourceLocation(yylineno)); }

ref		: type_postfix ptr_type					{ $$ = new PointerType($2, $1, SourceLocation(yylineno)); }

unary	: unary_op value_prefix					{ $$ = new UnaryExpr($1, $2, SourceLocation(yylineno)); }
		| CAST '(' type ')' value_prefix		{ $$ = new TypeConvertExpr($5, $3, false, SourceLocation(yylineno)); }


/*** primaries ***/

unknown_primary		: IDENTIFIER									{ $$ = new Identifier($1, SourceLocation(yylineno)); }
					| PRAGMA '(' IDENTIFIER ')'						{ $$ = new UnknownExpr(makePragma($3, nullptr), SourceLocation(yylineno)); }
					| PRAGMA '(' IDENTIFIER ',' unknown_list ')'	{ $$ = new UnknownExpr(makePragma($3, $5.get()), SourceLocation(yylineno)); }
					| '[' unknown_list ']'							{ $$ = new Tuple($2.get(), SourceLocation(yylineno)); }
					| '[' unknown ';' value_list ']'				{ $$ = new Tuple($2, $4.get(), SourceLocation(yylineno)); }
					| '(' unknown ')'								{ $$ = $2; }
known_value_primary	: literal										{ $$ = $1; }
					| function_literal								{ $$ = $1; }
					| '[' known_value_list ']'						{ $$ = new Tuple(Array<Node*>{ $2.get().slice() }, SourceLocation(yylineno)); }
					| '[' known_value ';' value_list ']'			{ $$ = new Tuple($2, $4.get(), SourceLocation(yylineno)); }
					| '(' known_value ')'							{ $$ = $2; }
known_type_primary	: primitive										{ $$ = $1; }
					| CONST '(' type ')'							{ $$ = ModifiedType::makeModified(TypeMod::Const, $3, SourceLocation(yylineno)); }
					| struct										{ $$ = $1; }
					| function										{ $$ = $1; }
					| '[' known_type_list ']'						{ $$ = new Tuple(Array<Node*>{ $2.get().slice() }, SourceLocation(yylineno)); }
					| '[' known_type ';' value_list ']'				{ $$ = new Tuple($2, $4.get(), SourceLocation(yylineno)); }
					| '(' known_type ')'							{ $$ = $2; }


/*** postfix ***/

unknown_postfix		: unknown_primary			{ $$ = $1; }
					| member					{ $$ = $1; }
					| unknown_postfix array		{ $$ = new Index($1, $2.get(), SourceLocation(yylineno)); }
known_value_postfix	: known_value_primary		{ $$ = $1; }
					| call						{ $$ = $1; }
					| known_value_postfix array	{ $$ = new Index($1, $2.get(), SourceLocation(yylineno)); }
known_type_postfix	: known_type_primary		{ $$ = $1; }
					| ref						{ $$ = $1; }
					| known_type_postfix array	{ $$ = new Index($1, $2.get(), SourceLocation(yylineno)); }

value_postfix		: known_value_postfix		{ $$ = $1; }
					| unknown_postfix			{ $$ = $1; /* Check/fix category */ }
type_postfix		: known_type_postfix		{ $$ = $1; }
					| unknown_postfix			{ $$ = $1; /* Check/fix category */ }
any_postfix			: known_value_postfix		{ $$ = $1; }
					| known_type_postfix		{ $$ = $1; }
					| unknown_postfix			{ $$ = $1; }


/*** prefix ***/

unknown_prefix		: unknown_postfix			{ $$ = $1; }
known_value_prefix	: known_value_postfix		{ $$ = $1; }
					| unary						{ $$ = $1; }
known_type_prefix	: known_type_postfix		{ $$ = $1; }

value_prefix		: known_value_prefix		{ $$ = $1; }
					| unknown_prefix			{ $$ = $1; /* Check/fix category */ }


/*** infix1 ***/

unknown_infix1		: unknown_prefix					{ $$ = $1; }
known_value_infix1	: known_value_prefix				{ $$ = $1; }
					| value_infix1 pow_op value_prefix	{ $$ = new BinaryExpr($2, $1, $3, SourceLocation(yylineno)); }
known_type_infix1	: known_type_prefix					{ $$ = $1; }

value_infix1		: known_value_infix1				{ $$ = $1; }
					| unknown_infix1					{ $$ = $1; /* Check/fix category */ }

/*** infix2 ***/

unknown_infix2		: unknown_infix1					{ $$ = $1; }
known_value_infix2	: known_value_infix1				{ $$ = $1; }
					| value_infix2 mul_op value_infix1	{ $$ = new BinaryExpr($2, $1, $3, SourceLocation(yylineno)); }
known_type_infix2	: known_type_infix1					{ $$ = $1; }

value_infix2		: known_value_infix2				{ $$ = $1; }
					| unknown_infix2					{ $$ = $1; /* Check/fix category */ }

/*** infix3 ***/

unknown_infix3		: unknown_infix2					{ $$ = $1; }
known_value_infix3	: known_value_infix2				{ $$ = $1; }
					| value_infix3 add_op value_infix2	{ $$ = new BinaryExpr($2, $1, $3, SourceLocation(yylineno)); }
known_type_infix3	: known_type_infix2					{ $$ = $1; }

value_infix3		: known_value_infix3				{ $$ = $1; }
					| unknown_infix3					{ $$ = $1; /* Check/fix category */ }

/*** infix4 ***/

unknown_infix4		: unknown_infix3					{ $$ = $1; }
known_value_infix4	: known_value_infix3				{ $$ = $1; }
					| value_infix4 shift_op value_infix3{ $$ = new BinaryExpr($2, $1, $3, SourceLocation(yylineno)); }
known_type_infix4	: known_type_infix3					{ $$ = $1; }

value_infix4		: known_value_infix4				{ $$ = $1; }
					| unknown_infix4					{ $$ = $1; /* Check/fix category */ }

/*** infix5 ***/

unknown_infix5		: unknown_infix4					{ $$ = $1; }
known_value_infix5	: known_value_infix4				{ $$ = $1; }
					| value_infix5 is_op value_infix4	{ $$ = new BinaryExpr($2, $1, $3, SourceLocation(yylineno)); }
known_type_infix5	: known_type_infix4					{ $$ = $1; }

value_infix5		: known_value_infix5				{ $$ = $1; }
					| unknown_infix5					{ $$ = $1; /* Check/fix category */ }

/*** infix6 ***/

unknown_infix6		: unknown_infix5					{ $$ = $1; }
known_value_infix6	: known_value_infix5				{ $$ = $1; }
					| value_infix6 cmp_op value_infix5	{ $$ = new BinaryExpr($2, $1, $3, SourceLocation(yylineno)); }
known_type_infix6	: known_type_infix5					{ $$ = $1; }

value_infix6		: known_value_infix6				{ $$ = $1; }
					| unknown_infix6					{ $$ = $1; /* Check/fix category */ }

/*** infix7 ***/

unknown_infix7		: unknown_infix6					{ $$ = $1; }
known_value_infix7	: known_value_infix6				{ $$ = $1; }
					| value_infix7 eq_op value_infix6	{ $$ = new BinaryExpr($2, $1, $3, SourceLocation(yylineno)); }
known_type_infix7	: known_type_infix6					{ $$ = $1; }

value_infix7		: known_value_infix7				{ $$ = $1; }
					| unknown_infix7					{ $$ = $1; /* Check/fix category */ }

/*** infix8 ***/

unknown_infix8		: unknown_infix7					{ $$ = $1; }
known_value_infix8	: known_value_infix7				{ $$ = $1; }
					| value_infix8 '&' value_infix7		{ $$ = new BinaryExpr(BinOp::BitAnd, $1, $3, SourceLocation(yylineno)); }
known_type_infix8	: known_type_infix7					{ $$ = $1; }

value_infix8		: known_value_infix8				{ $$ = $1; }
					| unknown_infix8					{ $$ = $1; /* Check/fix category */ }

/*** infix9 ***/

unknown_infix9		: unknown_infix8					{ $$ = $1; }
known_value_infix9	: known_value_infix8				{ $$ = $1; }
					| value_infix9 '^' value_infix8		{ $$ = new BinaryExpr(BinOp::BitXor, $1, $3, SourceLocation(yylineno)); }
known_type_infix9	: known_type_infix8					{ $$ = $1; }

value_infix9		: known_value_infix9				{ $$ = $1; }
					| unknown_infix9					{ $$ = $1; /* Check/fix category */ }

/*** infix10 ***/

unknown_infix10		: unknown_infix9					{ $$ = $1; }
known_value_infix10	: known_value_infix9				{ $$ = $1; }
					| value_infix10 '|' value_infix9	{ $$ = new BinaryExpr(BinOp::BitOr, $1, $3, SourceLocation(yylineno)); }
known_type_infix10	: known_type_infix9					{ $$ = $1; }

value_infix10		: known_value_infix10				{ $$ = $1; }
					| unknown_infix10					{ $$ = $1; /* Check/fix category */ }

/*** infix11 ***/

unknown_infix11		: unknown_infix10					{ $$ = $1; }
known_value_infix11	: known_value_infix10				{ $$ = $1; }
					| value_infix11 AND value_infix10	{ $$ = new BinaryExpr(BinOp::LogicAnd, $1, $3, SourceLocation(yylineno)); }
known_type_infix11	: known_type_infix10				{ $$ = $1; }

value_infix11		: known_value_infix11				{ $$ = $1; }
					| unknown_infix11					{ $$ = $1; /* Check/fix category */ }

/*** infix12 ***/

unknown_infix12		: unknown_infix11					{ $$ = $1; }
known_value_infix12	: known_value_infix11				{ $$ = $1; }
					| value_infix12 OR value_infix11	{ $$ = new BinaryExpr(BinOp::LogicOr, $1, $3, SourceLocation(yylineno)); }
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
any				: known_value					{ $$ = $1; }
				| known_type					{ $$ = $1; }
				| unknown						{ $$ = $1; }

/*** assign ***/

value_assign	: value							{ $$ = $1; }
				| value_postfix assign_op value	{ $$ = new AssignExpr($1, $3, $2, SourceLocation(yylineno)); }
				| value_postfix BIND value		{ $$ = new BindExpr($1, $3, SourceLocation(yylineno)); }

%%

void yyerror(const char *s)
{
	error(filename.c_str(), yylineno, "parse error: %s", s);
}


Array<Statement*> parse(FILE *file, String name)
{
	filename = name;

	// set lex to read from it instead of defaulting to STDIN:
	yyin = file;
	yylineno = 1;

	// parse through the input until there is no more:
	do {
		yyparse();
	} while (!feof(yyin));

	return std::move(parseTree);
}
