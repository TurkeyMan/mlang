#pragma once

#include "gc_cpp.h"
#include "gc_allocator.h"

#include "sourceloc.h"

#include <string>
#include <vector>
#include <map>

#include "common_llvm.h"
#include <llvm/Support/raw_ostream.h>

class ASTVisitor;

using llvm::raw_ostream;

template <typename T>
using owner = std::unique_ptr<T>;
using std::move;

class Expr;
owner<Expr> Error(const char *Str);

//using string = std::basic_string<char, std::char_traits<char>, gc_allocator<char>>;
//template <class K, class T, class P = std::less<K>>
//using map = std::map<K, T, P, gc_allocator<std::pair<const K, T>>>;


enum TypeFlags { TF_Unsigned = 1, TF_Signed = 2, TF_Char = 4, TF_Float = 8, TF_Bool = 16 };

extern uint8_t typeFlags[];
extern uint8_t typeWidth[];
extern uint8_t typeBytes[];
extern const char *primTypeNames[];

#define isFloat(pt) (typeFlags[(int)pt] & TF_Float)
#define isBinary(pt) (typeFlags[(int)pt] & ~TF_Float)
#define isSigned(pt) (typeFlags[(int)pt] & TF_Signed)
#define isUnsigned(pt) (typeFlags[(int)pt] & TF_Unsigned)
#define isInt(pt) (typeFlags[(int)pt] > 0 && typeFlags[(int)pt] < TF_Char)
#define isChar(pt) (typeFlags[(int)pt] & TF_Char)
#define isBool(pt) (typeFlags[(int)pt] & TF_Bool)

template <typename T>
struct List
{
	T *ptr;
	size_t length;

	static List empty() { return List{ nullptr, 0 }; }

//	void destroy()
//	{
//		for (size_t i = 0; i < length; ++i)
//			ptr[i].~T();
//		free(ptr);
//	}

	template <typename U>
	List& append(U &&v)
	{
		expand();
		new(&ptr[length - 1]) T(std::forward<U>(v));
		return *this;
	}

	template <typename U>
	List& prepend(U &&v)
	{
		expand();
		for (size_t i = length - 1; i > 0; --i)
		{
			new(&ptr[i]) T(std::move(ptr[i - 1]));
			ptr[i - 1].~T();
		}
		new(&ptr[0]) T(std::forward<U>(v));
		return *this;
	}

	T* begin() const { return ptr; }
	T* end() const { return ptr + length; }

	T& operator[](size_t i) { return ptr[i]; }
	const T& operator[](size_t i) const { return ptr[i]; }

private:
	void expand()
	{
		++length;
		if (((length - 1) & length) == 0)
		{
			T *mem = (T*)GC_MALLOC(sizeof(T)*(length << 1));
//			T *mem = (T*)malloc(sizeof(T)*(length<<1));
			for (size_t i = 0; i < length - 1; ++i)
			{
				new(&mem[i]) T(std::move(ptr[i]));
				ptr[i].~T();
			}
//			free(ptr);
			ptr = mem;
		}
	}
};



inline raw_ostream &indent(raw_ostream &O, int size) {
	return O << std::string(size*2, ' ');
}


// prototype nodes used for arguments?
class Expr;
class TypeExpr;
class Statement;
class ValDecl;
using ExprList = List<Expr*>;
using TypeExprList = List<TypeExpr*>;
using StatementList = List<Statement*>;
using DeclList = List<ValDecl*>;


//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

enum class PrimType
{
	v, u1, i8, u8, c8, i16, u16, c16, i32, u32, c32, iz, uz, i64, u64, i128, u128, f16, f32, f64, f128
};


class Node : public gc_cleanup
{
public:
	Node(SourceLocation loc = CurLoc) : loc(loc) {}
	virtual ~Node()
	{
		if (destroyCodegen)
			destroyCodegen(_codegenData);
	}

	int getLine() const { return loc.Line; }
	int getCol() const { return loc.Col; }

	template <typename T>
	T* cgData()
	{
		static_assert(sizeof(T) <= sizeof(_codegenData), "Not big enough!");
		if (!destroyCodegen)
			destroyCodegen = [](void *mem) { ((T*)mem)->~T(); };
		return (T*)_codegenData;
	}

	virtual void accept(ASTVisitor &v);

	virtual raw_ostream &dump(raw_ostream &out, int ind) { return out; }

protected:
	friend class LLVMGenerator;

private:
	uint64_t _codegenData[3];
	void(*destroyCodegen)(void*) = nullptr;

	SourceLocation loc;
};

class Statement : public Node
{
public:
	Statement()
		: Node()
	{}

	void accept(ASTVisitor &v) override;
};

class Declaration : public Statement
{
public:
	Declaration()
		: Statement()
	{}

	void accept(ASTVisitor &v) override;
};

class Scope
{
	friend class Semantic;

	std::map<std::string, Declaration*> _declarations;
	std::map<std::string, Scope*> _imports;

	Node *_owner; // the node that owns this scope
	Scope *_parent; // parent scope

public:
	Scope(Scope *parent, Node *owner)
		: _parent(parent), _owner(owner)
	{
	}

	Node *owner() const { return _owner; }
	Scope *parent() const { return _parent; }
	Declaration *getDecl(const std::string& name, bool onlyLocal = false) const;
	void addDecl(const std::string& name, Declaration *decl);
	auto& symbols() const { return _declarations; }
};

class Module : public Node
{
	Scope _scope;

	std::string _filename;
	std::string _name;

	StatementList moduleStatements;

public:
	Module(std::string filename, std::string name, StatementList moduleStatements)
		: Node(), _scope(nullptr, this), _filename(move(filename)), _name(move(name)), moduleStatements(moduleStatements)
	{
	}

	const std::string& filename() const { return _filename; }
	const std::string& name() const { return _name; }
	std::string& name() { return _name; }

	Scope* scope() { return &_scope; }

	StatementList statements() { return moduleStatements; }

	void accept(ASTVisitor &v) override;
};

class ModuleStatement : public Statement
{
	std::string _name;

public:
	ModuleStatement(std::string name)
		: Statement(), _name(move(name))
	{
	}

	const std::string& name() const { return _name; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override {
		return Statement::dump(out << "module: #" << _name << '\n', ind);
	}
};

class ExpressionStatement : public Statement
{
	friend class Semantic;

	Expr *_expression;

public:
	ExpressionStatement(Expr *expression)
		: Statement(), _expression(expression)
	{
	}

	Expr* expression() const { return _expression; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class ReturnStatement : public Statement
{
	friend class Semantic;

	Expr *_expression;

public:
	ReturnStatement(Expr *expression)
		: Statement(), _expression(expression)
	{
	}

	Expr* expression() const { return _expression; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class IfStatement : public Statement
{
	friend class Semantic;

	Expr *_cond;
	Scope *_init = nullptr;
	Scope *_then = nullptr;
	Scope *_else = nullptr;

	DeclList _initStatements;

	StatementList _thenStatements;
	StatementList _elseStatements;

public:
	IfStatement(Expr *cond, StatementList thenStatements, StatementList elseStatements, DeclList initStatements = DeclList::empty())
		: _cond(cond), _thenStatements(thenStatements), _elseStatements(elseStatements), _initStatements(initStatements)
	{}

	Expr *cond() { return _cond; }

	Scope* initScope() { return _init; }
	DeclList initStatements() { return _initStatements; }

	Scope* thenScope() { return _then; }
	StatementList thenStatements() { return _thenStatements; }

	Scope* elseScope() { return _else; }
	StatementList elseStatements() { return _elseStatements; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class LoopStatement : public Statement
{
	friend class Semantic;

	DeclList _iterators;
	Expr *_cond;
	ExprList _increments;
	Scope *_body = nullptr;

	StatementList _loopEntry;
	StatementList _bodyStatements;

public:
	LoopStatement(DeclList iterators, Expr *cond, ExprList increments, StatementList bodyStatements, StatementList loopEntry = StatementList::empty())
		: _iterators(iterators), _cond(cond), _increments(increments), _bodyStatements(bodyStatements), _loopEntry(loopEntry)
	{}

	DeclList iterators() { return _iterators; }

	Expr *cond() { return _cond; }

	StatementList entryStatements() { return _loopEntry; }
	StatementList bodyStatements() { return _bodyStatements; }
	ExprList incrementExpressions() { return _increments; }

	Scope* bodyScope() { return _body; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};


//****************
//** Type nodes **
//****************

class PrimitiveType;
class FunctionType;

class TypeExpr : public Node
{
public:
	TypeExpr(SourceLocation Loc = CurLoc) : Node(Loc) {}
	virtual ~TypeExpr() {}

	virtual std::string stringof() const = 0;

	virtual Expr* init() = 0;

	virtual bool isSame(TypeExpr *other) const = 0;

	PrimitiveType* asPrimitive();
	FunctionType* asFunction();

	bool isVoid();
	bool isBoolean();
	bool isIntegral();
	bool isFloatingPoint();

	void accept(ASTVisitor &v) override;

//	virtual raw_ostream &dump(raw_ostream &out, int ind) { return out << ':' << getLine() << ':' << getCol() << '\n'; }
};

class PrimitiveType : public TypeExpr
{
	PrimType _type;
	Expr *_init = nullptr;

public:
	PrimitiveType(PrimType type) : TypeExpr(), _type(type) {}
	~PrimitiveType() {}

	PrimType type() const { return _type; }
	Expr* init() override;

	bool isSame(TypeExpr *other) const override;

	void accept(ASTVisitor &v) override;

	std::string stringof() const override;
	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class TypeIdentifier : public TypeExpr
{
	std::string name;

public:
	TypeIdentifier(std::string name)
		: TypeExpr(), name(name)
	{}

	TypeExpr* type() { assert(false); return nullptr; }
	Expr* init() override { return type()->init(); }

	bool isSame(TypeExpr *other) const override { assert(false); return false; }

	void accept(ASTVisitor &v) override;

	std::string stringof() const override { return std::string("@") + name; }
	raw_ostream &dump(raw_ostream &out, int ind) override { return out << stringof() << '\n'; }
};

class TupleType : public TypeExpr
{
	TypeExprList _types = TypeExprList::empty();

public:
	TupleType(TypeExprList types = TypeExprList::empty())
		: TypeExpr(), _types(types)
	{}

	TypeExprList types() { return _types; }
	Expr* init() override { assert(false); return nullptr; }

	bool isSame(TypeExpr *other) const override { assert(false); return false; }

	void accept(ASTVisitor &v) override;

	std::string stringof() const override
	{
		std::string r = "[ ";
		for (size_t i = 0; i < _types.length; ++i)
			r += (i > 0 ? std::string(",") : std::string()) + _types[i]->stringof();
		r += " ]";
		return r;
	}
	raw_ostream &dump(raw_ostream &out, int ind) override
	{
		out << "tuple: [\n";
		for (auto t : _types)
			t->dump(out, ind + 1);
		return indent(out, ind) << "]\n";
	}
};

class Struct : public TypeExpr
{
	Scope body;

	StatementList _members;

public:
	Struct(StatementList members = StatementList::empty())
		: TypeExpr(), body(nullptr, this), _members(members)
	{}

	Scope* scope() { return &body; }
	StatementList members() { return _members; }

	Expr* init() override { assert(false); return nullptr; }

	bool isSame(TypeExpr *other) const override { assert(false); return false; }

	void accept(ASTVisitor &v) override;

	std::string stringof() const override { return "struct{ ... }"; }
	raw_ostream &dump(raw_ostream &out, int ind) override
	{
		out << "struct: {\n";
		for (auto m : _members)
			m->dump(out, ind + 1);
		return indent(out, ind) << "}\n";
	}
};

class FunctionType : public TypeExpr
{
	TypeExpr *_returnType = nullptr;
	TypeExprList _args = TypeExprList::empty();

public:
	FunctionType(TypeExpr *returnType, TypeExprList args)
		: TypeExpr(), _returnType(returnType), _args(args) {}
	~FunctionType() {}

	Expr* init() override { assert(false); return nullptr; }

	bool isSame(TypeExpr *other) const override;

	TypeExpr* returnType() const { return _returnType; }
	TypeExprList argTypes() const { return _args; }

	void accept(ASTVisitor &v) override;

	std::string stringof() const override
	{
		std::string r = _returnType ? _returnType->stringof() : std::string("???");
		r += "(";
		for (size_t i = 0; i < _args.length; ++i)
			r += (i > 0 ? std::string(",") : std::string()) + _args[i]->stringof();
		r += ")";
		return r;
	}
	raw_ostream &dump(raw_ostream &out, int ind) override
	{
		out << "function\n";

		ind++;
		indent(out, ind) << "return: ";
		if (_returnType)
			_returnType->dump(out, ind + 1);
		else
			out << "???\n";
		indent(out, ind) << "args: (\n";
		for (auto a : _args)
			a->dump(indent(out, ind + 1), ind + 1);
		return indent(out, ind) << ")\n";
	}
};

inline PrimitiveType* TypeExpr::asPrimitive() { return dynamic_cast<PrimitiveType*>(this); }
inline FunctionType* TypeExpr::asFunction() { return dynamic_cast<FunctionType*>(this); }
inline bool TypeExpr::isVoid() { PrimitiveType *pt = dynamic_cast<PrimitiveType*>(this); return pt && pt->type() == PrimType::v; }
inline bool TypeExpr::isBoolean() { PrimitiveType *pt = dynamic_cast<PrimitiveType*>(this); return pt && isBool(pt->type()); }
inline bool TypeExpr::isIntegral() { PrimitiveType *pt = dynamic_cast<PrimitiveType*>(this); return pt && isInt(pt->type()); }
inline bool TypeExpr::isFloatingPoint() { PrimitiveType *pt = dynamic_cast<PrimitiveType*>(this); return pt && isFloat(pt->type()); }


//***********************
//** Declaration nodes **
//***********************

class TypeDecl : public Declaration
{
	friend class Semantic;
protected:
	std::string _name;
	TypeExpr *_type;

public:
	TypeDecl(std::string name, TypeExpr *type) // TODO: template args
		: Declaration(), _name(move(name)), _type(type)
	{}

	const std::string& name() const { return _name; }
	TypeExpr* type() const { return _type; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class ValDecl : public Declaration
{
	friend class Semantic;
	friend Statement* makeForEach(DeclList, Expr*, StatementList);
protected:
	std::string _name;
	TypeExpr *_type;
	Expr *_init;

public:
	ValDecl(std::string name, TypeExpr *type, Expr *init)
		: Declaration(), _name(move(name)), _type(type), _init(init)
	{}

	const std::string& name() { return _name; }
	TypeExpr* type() { return _type; }
	Expr* init() { return _init; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};
class VarDecl : public ValDecl
{
	friend class Semantic;
public:
	VarDecl(std::string name, TypeExpr *type, Expr *init)
		: ValDecl(name, type, init)
	{}

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};


//**********************
//** Expression nodes **
//**********************

class Expr : public Node
{
public:
	Expr(SourceLocation Loc = CurLoc) : Node(Loc) {}
	virtual ~Expr() {}

	virtual TypeExpr* type() = 0;

	void accept(ASTVisitor &v) override;

//	raw_ostream &dump(raw_ostream &out, int ind) override { return out << ':' << getLine() << ':' << getCol() << '\n'; }
};

class PrimitiveLiteralExpr : public Expr
{
	PrimType _type;
	TypeExpr *_typeExpr = nullptr;

	union
	{
		double f;
		int64_t i;
		uint64_t u;
		char32_t c;
	};

public:
	PrimitiveLiteralExpr(PrimType type, uint64_t v) : Expr(), _type(type), u(v) {}
	PrimitiveLiteralExpr(PrimType type, int64_t v) : Expr(), _type(type), i(v) {}
	PrimitiveLiteralExpr(PrimType type, double v) : Expr(), _type(type), f(v) {}
	PrimitiveLiteralExpr(PrimType type, char32_t v) : Expr(), _type(type), c(v) {}
	PrimitiveLiteralExpr(uint64_t v) : Expr(), _type(PrimType::u64), u(v) {}
	PrimitiveLiteralExpr(int64_t v) : Expr(), _type(PrimType::i64), i(v) {}
	PrimitiveLiteralExpr(double v) : Expr(), _type(PrimType::f64), f(v) {}
	PrimitiveLiteralExpr(char32_t v) : Expr(), _type(PrimType::c32), c(v) {}
	PrimitiveLiteralExpr(bool b) : Expr(), _type(PrimType::u1), u(b ? 1 : 0) {}

	PrimType primType() const { return _type; }
	double getFloat() const { return f; }
	int64_t getInt() const { return i; }
	uint64_t getUint() const { return u; }
	char32_t getChar() const { return c; }

	TypeExpr* type() override;

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override
	{
		switch (_type)
		{
			case PrimType::f16:
			case PrimType::f32:
			case PrimType::f64:
			case PrimType::f128:
				return Expr::dump(out << f, ind) << '\n';
			case PrimType::u1:
			case PrimType::u8:
			case PrimType::u16:
			case PrimType::u32:
			case PrimType::u64:
			case PrimType::u128:
				return Expr::dump(out << u, ind) << '\n';
			case PrimType::i8:
			case PrimType::i16:
			case PrimType::i32:
			case PrimType::i64:
			case PrimType::i128:
				return Expr::dump(out << i, ind) << '\n';
			case PrimType::c8:
			case PrimType::c16:
			case PrimType::c32:
				return Expr::dump(out << c, ind) << '\n';
			default:
				assert(0); return out;
		}
	}
};

class ArrayLiteralExpr: public Expr {
	ExprList items;

public:
	ArrayLiteralExpr(ExprList items)
		: Expr(), items(move(items)) {}

	TypeExpr* type() override { return nullptr; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override {
		assert("!");
		return out;
	}
};

class FunctionLiteralExpr : public Expr
{
	friend class Semantic;

	Scope body;

	StatementList bodyStatements;

	DeclList _args;
	TypeExpr *returnType;
	bool inferReturnType;

	TypeExprList argTypes = TypeExprList::empty();
	FunctionType *_type = nullptr;

public:
	FunctionLiteralExpr(StatementList bodyStatements, DeclList args, TypeExpr *returnType)
		: Expr(), body(nullptr, this), bodyStatements(bodyStatements), _args(args), returnType(returnType), inferReturnType(returnType == nullptr)
	{}

	FunctionType* type() override { return _type; }

	DeclList args() const { return _args; }
	StatementList statements() { return bodyStatements; }

	Scope* scope() { return &body; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind)
	{
		out << "fn\n";

		ind++;
		indent(out, ind) << "return: ";
		if (returnType)
			returnType->dump(out, ind + 1);
		else
			out << "???\n";
		indent(out, ind) << "args: (\n";
		for (auto a : _args)
			a->dump(indent(out, ind + 1), ind + 1);
		indent(out, ind) << ")\n";
		indent(out, ind) << "body: {\n";
		for (auto s : bodyStatements)
			s->dump(indent(out, ind + 1), ind + 1);
		return indent(out, ind) << "}\n";
	}
};


class IdentifierExpr : public Expr
{
	friend class Semantic;

	std::string name;

	Declaration *_target = nullptr;

public:
	IdentifierExpr(const std::string &name)
		: Expr(), name(std::move(name)) {}
	const std::string &getName() const { return name; }

	Declaration* target() { return _target; }
	TypeExpr* type() override { return static_cast<ValDecl*>(_target)->type(); }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override {
		return Expr::dump(out << '#' << name << "\n", ind);
	}
};

class TypeConvertExpr : public Expr
{
	friend class Semantic;

	Expr *_expr;
	TypeExpr *_newType;
	bool _implicit;

public:
	TypeConvertExpr(Expr *expr, TypeExpr *newType, bool implicit = true)
		: Expr(), _expr(expr), _newType(newType), _implicit(implicit) {}

	Expr* expr() { return _expr; }
	TypeExpr* type() override { return _newType; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override {
		Expr::dump(out << "cast\n", ind);
		_newType->dump(indent(out, ind) << "target: ", ind + 1);
		_expr->dump(indent(out, ind) << "expr: ", ind + 1);
		return out;
	}
};

enum class UnaryOp
{
	Pos, Neg, BitNot, LogicNot, PreInc, PreDec
};
class UnaryExpr : public Expr
{
	friend class Semantic;

	UnaryOp _op;
	Expr *_operand;
	TypeExpr *_type = nullptr;

public:
	UnaryExpr(UnaryOp op, Expr *operand)
		: Expr(), _op(op), _operand(operand) {}

	UnaryOp op() { return _op; }
	Expr* operand() { return _operand; }

	TypeExpr* type() override { return _type; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

enum class BinOp
{
	Add, Sub, Mul, Div, Mod, Pow, Cat,
	ASL, ASR, LSR,
	BitAnd, BitOr, BitXor, LogicAnd, LogicOr, LogicXor,
	Eq, Ne, Gt, Ge, Lt, Le
};
class BinaryExpr : public Expr
{
	friend class Semantic;

	BinOp _op;
	Expr *_lhs, *_rhs;
	TypeExpr *_type = nullptr;

public:
	BinaryExpr(BinOp op, Expr *lhs, Expr *rhs)
		: Expr(), _op(op), _lhs(lhs), _rhs(rhs) {}

	BinOp op() { return _op; }
	Expr* lhs() { return _lhs; }
	Expr* rhs() { return _rhs; }

	TypeExpr* type() override { return _type; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

/// CallExpr - Expression class for function calls.
class IndexExpr : public Expr {
	owner<Expr> Source;
	std::vector<owner<Expr>> Args;

public:
	IndexExpr(SourceLocation Loc, owner<Expr> Source,
		std::vector<owner<Expr>> Args)
		: Expr(Loc), Source(move(Source)), Args(std::move(Args)) {}

	TypeExpr* type() override;

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override {
		assert("!");
		return out;
	}
};

class CallExpr : public Expr
{
	friend class Semantic;

	Expr *_func;
	ExprList _callArgs;

public:
	CallExpr(Expr *func, ExprList callArgs)
		: _func(func), _callArgs(callArgs)
	{}

	TypeExpr* type() override
	{
		FunctionType *f = dynamic_cast<FunctionType*>(_func->type());
		return f->returnType();
	}

	Expr *function() { return _func; }
	ExprList callArgs() { return _callArgs; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

/// Prototype - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes), as well as if it is an operator.
class PrototypeDecl : public Declaration
{
	std::string Name;
	std::vector<std::string> Args;
	bool IsOperator;
	unsigned Precedence; // Precedence if a binary op.
	int Line;

public:
	PrototypeDecl(SourceLocation Loc, const std::string &Name, std::vector<std::string> Args, bool IsOperator = false, unsigned Prec = 0)
		: Declaration(), Name(Name), Args(std::move(Args)), IsOperator(IsOperator), Precedence(Prec), Line(Loc.Line)
	{}

	const std::string &name() const { return Name; }

	bool isUnaryOp() const { return IsOperator && Args.size() == 1; }
	bool isBinaryOp() const { return IsOperator && Args.size() == 2; }

	char getOperatorName() const
	{
		assert(isUnaryOp() || isBinaryOp());
		return Name[Name.size() - 1];
	}

	unsigned getBinaryPrecedence() const { return Precedence; }
	int getLine() const { return Line; }

	void accept(ASTVisitor &v) override;
};

/// Function - This class represents a function definition itself.
class FunctionDecl : public Declaration
{
	Scope body;

	owner<PrototypeDecl> Proto;
	owner<Expr> Body;

public:
	FunctionDecl(owner<PrototypeDecl> Proto, owner<Expr> Body)
		: Declaration(), body(nullptr, this), Proto(std::move(Proto)), Body(std::move(Body))
	{}

	Scope* scope() { return &body; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind)
	{
		indent(out, ind) << "Function\n";
		++ind;
		indent(out, ind) << "Body:";
		return Body ? Body->dump(out, ind) : out << "null\n";
	}
};

Statement* makeForEach(DeclList iterators, Expr *range, StatementList body);





class Generic : public Node
{
public:
	enum class Type
	{
		String,

		List,

		TempalteId,
		Type,
		Instantiate,

		Const,
		Pointer,
		Ref,

		TypedId,

		Struct,
		Tuple,

		Array,

		ArrayLiteral,

		Module,
		DefType,
		DefConst,
		Var,

		Elipsis,

		MemberLookup,
		Call,
		OpIndex,
		OpPostInc,
		OpPostDec,
		OpAssign,
		OpBind,
		OpMulEq,
		OpDivEq,
		OpModEq,
		OpAddEq,
		OpSubEq,
		OpConcatEq,
		OpBitAndEq,
		OpBitXorEq,
		OpBitOrEq,
		OpAndEq,
		OpXorEq,
		OpOrEq,
		OpASLEq,
		OpASREq,
		OpLSREq,

		Return,
		Break,
	};

	Generic(Type type, Node *l = nullptr, Node *r = nullptr)
		: Node(), type(type), l(l), r(r) {}

	template <typename T>
	List<T> asList() const
	{
		assert(type == Type::List);

		List<T> l = List<T>::empty();
		asListImpl(l, this);
		return l;
	}

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
	raw_ostream &dumpList(raw_ostream &out, int ind);

	//private:
	Type type;
	union
	{
		double f;
		__int64 i;
		unsigned __int64 u;
		const char *s;
	};
	Node *l;
	Node *r;

private:
	template <typename T>
	static void asListImpl(List<T> &l, const Generic *pList)
	{
		if (pList->l)
		{
			Generic *gl = dynamic_cast<Generic*>(pList->l);
			if (gl && gl->type == Generic::Type::List)
				asListImpl(l, gl);
			else
				l.append(dynamic_cast<T>(pList->l));
		}
		if (pList->r)
		{
			Generic *gr = dynamic_cast<Generic*>(pList->r);
			if (gr && gr->type == Generic::Type::List)
				asListImpl(l, gr);
			else
				l.append(dynamic_cast<T>(pList->r));
		}
	}
};

Node* Push(Node *n);
Node* Pop();
Node* Top();

Generic* String(const char* s);
Generic* TypeId(const char* i);

Generic* Add(Generic::Type type, Node *l = nullptr, Node *r = nullptr);
Generic* SetChildren(Generic *node, Node *l = nullptr, Node *r = nullptr);
