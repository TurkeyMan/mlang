#pragma once

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


template <typename T>
struct List
{
	T *ptr;
	size_t length;

	static List empty() { return List{ nullptr, 0 }; }

	void destroy()
	{
		for (size_t i = 0; i < length; ++i)
			ptr[i].~T();
		free(ptr);
	}

	template <typename U>
	List& append(U &&v)
	{
		expand();
		new(&ptr[length - 1]) T(std::forward<U>(v));
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
			T *mem = (T*)malloc(sizeof(T)*(length<<1));
			for (size_t i = 0; i < length - 1; ++i)
			{
				new(&mem[i]) T(std::move(ptr[i]));
				ptr[i].~T();
			}
			free(ptr);
			ptr = mem;
		}
	}
};



inline raw_ostream &indent(raw_ostream &O, int size) {
	return O << std::string(size*2, ' ');
}


//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

enum class PrimType
{
	v, u1, i8, u8, i16, u16, i32, u32, i64, u64, iz, uz, f32, f64, c8, c16, c32
};


class Node
{
public:
	Node(SourceLocation loc = CurLoc) : loc(loc) {}
	virtual ~Node() {}

	int getLine() const { return loc.Line; }
	int getCol() const { return loc.Col; }

	virtual void accept(ASTVisitor &v);

	virtual raw_ostream &dump(raw_ostream &out, int ind) { return out; }

private:
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
using StatementList = List<Statement*>;

class Declaration : public Statement
{
public:
	Declaration()
		: Statement()
	{}

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

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override {
		return Statement::dump(out << "module: #" << _name << '\n', ind);
	}
};

class Scope : public Node
{
	std::map<std::string, Declaration*> _symbols;

	Scope *_parent;

public:
	Scope(Scope *parent)
		: Node(), _parent(parent)
	{
	}

	Scope *parent() const { return _parent; }
	Declaration *getDecl(const std::string& name, bool onlyLocal = false) const;
	void addDecl(const std::string& name, Declaration *decl);
	auto& symbols() const { return _symbols; }

	void accept(ASTVisitor &v) override;
};

class Module : public Scope
{
	std::string _filename;
	std::string _name;

public:
	Module(std::string filename, std::string name)
		: Scope(nullptr), _filename(move(filename)), _name(move(name))
	{
	}

	const std::string& filename() const { return _filename; }
	const std::string& name() const { return _name; }
	std::string& name() { return _name; }

	void accept(ASTVisitor &v) override;
};


class Generic : public Node
{
public:
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
		Instantiate,

		Const,
		Pointer,
		Ref,

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

		Elipsis,

		MemberLookup,
		Call,
		OpIndex,
		OpPostInc,
		OpPostDec,
		OpPreInc,
		OpPreDec,
		OpUnaryPlus,
		OpUnaryMinus,
		OpUnaryNot,
		OpUnaryComp,
		OpMul,
		OpDiv,
		OpMod,
		OpAdd,
		OpSub,
		OpConcat,
		OpASL,
		OpASR,
		OpLSR,
		OpLt,
		OpGt,
		OpLe,
		OpGe,
		OpEq,
		OpNe,
		OpBitAnd,
		OpBitXor,
		OpBitOr,
		OpAnd,
		OpXor,
		OpOr,
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


//****************
//** Type nodes **
//****************

class TypeExpr : public Node
{
public:
	TypeExpr(SourceLocation Loc = CurLoc) : Node(Loc) {}
	virtual ~TypeExpr() {}

	virtual std::string stringof() const = 0;

	virtual Expr* init() = 0;

	void accept(ASTVisitor &v) override;

//	virtual raw_ostream &dump(raw_ostream &out, int ind) { return out << ':' << getLine() << ':' << getCol() << '\n'; }
};
using TypeExprList = List<TypeExpr*>;

class PrimitiveType : public TypeExpr
{
	PrimType _type;
	Expr *_init = nullptr;

public:
	PrimitiveType(PrimType type) : TypeExpr(), _type(type) {}
	~PrimitiveType() {}

	PrimType type() const { return _type; }
	Expr* init() override;

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
	StatementList members;
	Scope *body;
	Scope *parent;

public:
	Struct(StatementList members = StatementList::empty())
		: TypeExpr(), members(members)
	{}

	Expr* init() override { assert(false); return nullptr; }

	void accept(ASTVisitor &v) override;

	std::string stringof() const override { return "struct{ ... }"; }
	raw_ostream &dump(raw_ostream &out, int ind) override
	{
		out << "struct: {\n";
		for (auto m : members)
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
			a->dump(out, ind + 1);
		return indent(out, ind) << ")\n";
	}
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
using ExprList = List<Expr*>;

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

	PrimType primType() const { return _type; }
	double getFloat() const { return f; }
	int64_t getint() const { return i; }
	uint64_t getUint() const { return u; }
	char32_t getChar() const { return c; }

	TypeExpr* type() override;

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override
	{
		switch (_type)
		{
			case PrimType::f32:
			case PrimType::f64:
				return Expr::dump(out << f, ind) << '\n';
			case PrimType::u8:
			case PrimType::u16:
			case PrimType::u32:
			case PrimType::u64:
				return Expr::dump(out << u, ind) << '\n';
			case PrimType::i8:
			case PrimType::i16:
			case PrimType::i32:
			case PrimType::i64:
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
	StatementList bodyStatements;
	StatementList _args;
	TypeExpr *returnType;

	Scope *body;
	Scope *parent;

public:
	FunctionLiteralExpr(StatementList bodyStatements, StatementList args, TypeExpr *returnType)
		: Expr(), bodyStatements(bodyStatements), _args(args), returnType(returnType)
	{}

	TypeExpr* type() override { assert(false); return nullptr; }

	StatementList args() const { return _args; }

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


/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public Expr {
	std::string Name;

public:
	VariableExprAST(SourceLocation Loc, const std::string &Name)
		: Expr(Loc), Name(Name) {}
	const std::string &getName() const { return Name; }

	TypeExpr* type() override;

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override {
		return Expr::dump(out << Name, ind);
	}
};

/// UnaryExprAST - Expression class for a unary operator.
class UnaryExprAST : public Expr {
	char Opcode;
	owner<Expr> Operand;

public:
	UnaryExprAST(char Opcode, owner<Expr> Operand)
		: Expr(), Opcode(Opcode), Operand(std::move(Operand)) {}

	TypeExpr* type() override;

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override {
		Expr::dump(out << "unary" << Opcode, ind);
		Operand->dump(out, ind + 1);
		return out;
	}
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public Expr {
	char Op;
	owner<Expr> LHS, RHS;

public:
	BinaryExprAST(SourceLocation Loc, char Op, owner<Expr> LHS, owner<Expr> RHS)
		: Expr(Loc), Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}

	TypeExpr* type() override;

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override {
		Expr::dump(out << "binary" << Op, ind);
		LHS->dump(indent(out, ind) << "LHS:", ind + 1);
		RHS->dump(indent(out, ind) << "RHS:", ind + 1);
		return out;
	}
};

/// CallExprAST - Expression class for function calls.
class IndexExprAST : public Expr {
	owner<Expr> Source;
	std::vector<owner<Expr>> Args;

public:
	IndexExprAST(SourceLocation Loc, owner<Expr> Source,
		std::vector<owner<Expr>> Args)
		: Expr(Loc), Source(move(Source)), Args(std::move(Args)) {}

	TypeExpr* type() override;

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override {
		assert("!");
		return out;
	}
};

/// CallExprAST - Expression class for function calls.
class CallExprAST : public Expr {
	std::string Callee;
	std::vector<owner<Expr>> Args;

public:
	CallExprAST(SourceLocation Loc, const std::string &Callee,
		std::vector<owner<Expr>> Args)
		: Expr(Loc), Callee(Callee), Args(std::move(Args)) {}

	TypeExpr* type() override;

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override {
		Expr::dump(out << "call " << Callee, ind);
		for (const auto &Arg : Args)
			Arg->dump(indent(out, ind + 1), ind + 1);
		return out;
	}
};

/// IfExprAST - Expression class for if/then/else.
class IfExprAST : public Expr {
	owner<Expr> Cond, Then, Else;

public:
	IfExprAST(SourceLocation Loc, owner<Expr> Cond,
		owner<Expr> Then, owner<Expr> Else)
		: Expr(Loc), Cond(std::move(Cond)), Then(std::move(Then)),
		Else(std::move(Else)) {}

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override {
		Expr::dump(out << "if", ind);
		Cond->dump(indent(out, ind) << "Cond:", ind + 1);
		Then->dump(indent(out, ind) << "Then:", ind + 1);
		Else->dump(indent(out, ind) << "Else:", ind + 1);
		return out;
	}
};

/// ForExprAST - Expression class for for/in.
class ForExprAST : public Expr
{
	std::string VarName;
	owner<Expr> Start, End, Step, Body;

public:
	ForExprAST(const std::string &VarName, owner<Expr> Start, owner<Expr> End, owner<Expr> Step, owner<Expr> Body)
		: Expr(), VarName(VarName), Start(std::move(Start)), End(std::move(End)), Step(std::move(Step)), Body(std::move(Body))
	{}

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override
	{
		Expr::dump(out << "for", ind);
		Start->dump(indent(out, ind) << "Cond:", ind + 1);
		End->dump(indent(out, ind) << "End:", ind + 1);
		Step->dump(indent(out, ind) << "Step:", ind + 1);
		Body->dump(indent(out, ind) << "Body:", ind + 1);
		return out;
	}
};


class TypeDecl : public Declaration
{
	std::string _name;
	TypeExpr *_type;

public:
	TypeDecl(std::string name, TypeExpr *type) // TODO: template args
		: Declaration(), _name(move(name)), _type(type)
	{}

	const std::string& name() const { return _name; }
	TypeExpr* type() const { return _type; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override
	{
		Declaration::dump(out << "type: " << _name, ind) << "\n";
		++ind;
		if (_type)
			_type->dump(indent(out, ind) << "as: ", ind);
		return out;
	}
};

class ValDecl : public Declaration
{
	friend class Semantic;
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

	raw_ostream &dump(raw_ostream &out, int ind) override
	{
		Declaration::dump(out << "def: #" << _name, ind) << "\n";
		++ind;
		if (_type)
			_type->dump(indent(out, ind) << "type: ", ind);
		if (_init)
			_init->dump(indent(out, ind) << "val: ", ind);
		return out;
	}
};
class VarDecl : public ValDecl
{
public:
	VarDecl(std::string name, TypeExpr *type, Expr *init)
		: ValDecl(name, type, init)
	{}

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override
	{
		Declaration::dump(out << "var: #" << _name, ind) << "\n";
		++ind;
		if (_type)
			_type->dump(indent(out, ind) << "type: ", ind);
		if (_init)
			_init->dump(indent(out, ind) << "init: ", ind);
		return out;
	}
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
	owner<PrototypeDecl> Proto;
	owner<Expr> Body;

	Scope *body;
	Scope *parent;

public:
	FunctionDecl(owner<PrototypeDecl> Proto, owner<Expr> Body)
		: Declaration(), Proto(std::move(Proto)), Body(std::move(Body))
	{}

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind)
	{
		indent(out, ind) << "Function\n";
		++ind;
		indent(out, ind) << "Body:";
		return Body ? Body->dump(out, ind) : out << "null\n";
	}
};


Node* Push(Node *n);
Node* Pop();
Node* Top();

Generic* String(const char* s);
Generic* Identifier(const char* i);
Generic* TypeId(const char* i);
Generic* Int(__int64 i);
Generic* UInt(unsigned __int64 i);
Generic* Float(double f);

Generic* Add(Generic::Type type, Node *l = nullptr, Node *r = nullptr);
Generic* SetChildren(Generic *node, Node *l = nullptr, Node *r = nullptr);

void Print(Node *n);
