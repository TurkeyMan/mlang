#pragma once

#include "gc_cpp.h"
#include "gc_allocator.h"

#include "sourceloc.h"
#include "util.h"

#include <string>
#include <vector>
#include <map>

#include "common_llvm.h"
#include <llvm/Support/raw_ostream.h>

class ASTVisitor;

using llvm::raw_ostream;

using std::move;


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

#define tyWidth(pt) typeWidth[(int)pt]
#define tyBytes(pt) typeBytes[(int)pt]

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

	List append(const T &v) const
	{
		List<T> l = { ptr, length };
		l.expand();
		new(&l.ptr[l.length - 1]) T(v);
		return l;
	}
	List append(T &&v) const
	{
		List<T> l = { ptr, length };
		l.expand();
		new(&l.ptr[l.length - 1]) T(std::move(v));
		return l;
	}
	template <typename U>
	List append(const List<U> &list) const
	{
		List<T> l = { ptr, length };
		for (auto &i : list)
			l = l.append(i);
		return l;
	}

	template <typename U>
	List prepend(U &&v) const
	{
		List<T> l = { ptr, length };
		l.expand();
		for (size_t i = l.length - 1; i > 0; --i)
		{
			new(&l.ptr[i]) T(std::move(l.ptr[i - 1]));
			l.ptr[i - 1].~T();
		}
		new(&l.ptr[0]) T(std::forward<U>(v));
		return l;
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
class Statement;
class Declaration;
class ValDecl;
class VarDecl;

class Node;
class Expr;
class TypeExpr;
class AmbiguousExpr;

class PrimitiveType;
class FunctionType;
class PointerType;
class Struct;
class Tuple;

using NodeList = List<Node*>;
using ExprList = List<Expr*>;
using TypeExprList = List<TypeExpr*>;
using StatementList = List<Statement*>;
using DeclList = List<ValDecl*>;


enum class ConvType
{
	Convertible,
	LosesPrecision,
	OnlyExplicit,
	NoConversion
};


//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

class Node : public gc_cleanup
{
	friend class Semantic;
	friend class LLVMGenerator;
public:
	Node(SourceLocation loc) : loc(loc) {}
	virtual ~Node()
	{
		if (destroyCodegen)
			destroyCodegen(_codegenData);
	}

	virtual std::string stringof() const = 0;
	virtual std::string mangleof() const = 0;

	// all nodes may have properties
	virtual Node* getMember(const std::string &name);

	SourceLocation getLoc() const { return loc; }
	int getLine() const { return loc.line; }
	int getCol() const { return loc.col; }

	Expr* asExpr();
	TypeExpr* asType();

	template <typename T>
	T* cgData()
	{
		static_assert(sizeof(T) <= sizeof(_codegenData), "Not big enough!");
		if (!destroyCodegen)
			destroyCodegen = [](void *mem) { ((T*)mem)->~T(); };
		return (T*)_codegenData;
	}

	virtual void accept(ASTVisitor &v) = 0;
	virtual raw_ostream &dump(raw_ostream &out, int ind) { return out; }

private:
	uint64_t _codegenData[3];
	void(*destroyCodegen)(void*) = nullptr;

	SourceLocation loc;
	bool _doneSemantic = false;
	bool _doneCodegen = false;

protected:
	bool doneSemantic()
	{
		if (_doneSemantic)
			return true;
		_doneSemantic = true;
		return false;
	}
	bool doneCodegen()
	{
		if (_doneCodegen)
			return true;
		_doneCodegen = true;
		return false;
	}
};


//************
//** Scopes **
//************

class Scope : public virtual Node
{
	friend class Semantic;

	std::map<std::string, Declaration*> _symbols;
	std::map<std::string, Scope*> _imports;

	Scope *_parentScope; // parent scope

	std::string stringof() const override { assert(false); return std::string(); }
	std::string mangleof() const override { assert(false); return std::string(); }

public:
	Scope(Scope *parent, SourceLocation loc)
		: Node(loc), _parentScope(parent)
	{}

	Scope *parentScope() const { return _parentScope; }
	Declaration *getDecl(const std::string& name, bool onlyLocal = false);
	void addDecl(const std::string& name, Declaration *decl);
	auto& symbols() const { return _symbols; }
};

extern std::map<std::string, TypeExpr*> typesUsed;

template <typename TypeNode, typename... Args> struct MakeTypeHelper;
template <typename TypeNode, typename... Args>
TypeNode* makeType(Scope *scope, Args&&... args)
{
	return MakeTypeHelper<TypeNode, Args...>::makeType(scope, std::forward<Args>(args)...);
}

//****************
//** Node types **
//****************

class Expr : public virtual Node
{
public:
	Expr(SourceLocation loc) : Node(loc) {}

	virtual TypeExpr* type() = 0;

	virtual std::string mangleof() const override { return std::string(); }

	Expr* makeConversion(TypeExpr *targetType, bool implicit = true);

	virtual Expr* resolveExpr() { return this; }

	// TODO: constant evaluation
	virtual Expr* constEval() { return nullptr; };

	Node* getMember(const std::string &name) override;

	bool isConstantInt();
	int64_t getIntValue();

//	raw_ostream &dump(raw_ostream &out, int ind) override {}
};

class TypeExpr : public virtual Node
{
public:
	TypeExpr(SourceLocation loc) : Node(loc) {}

	virtual Expr* init() const = 0;

	virtual size_t size() const = 0;
	virtual size_t alignment() const = 0;

	virtual bool isSame(const TypeExpr *other) const = 0;
	virtual ConvType convertible(const TypeExpr *target) const = 0;
	virtual Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const = 0;

	virtual TypeExpr* resolveType() { return this; }

	//helpers
	TypeExpr* resultType();
	TypeExpr* evalType();

	int ptrDepth() const;

	PrimitiveType* asPrimitive();
	const PrimitiveType* asPrimitive() const;
	::FunctionType* asFunction();
	const ::FunctionType* asFunction() const;
	::PointerType* asPointer();
	const ::PointerType* asPointer() const;
	Struct* asStruct();
	const Struct* asStruct() const;
	Tuple* asTuple();
	const Tuple* asTuple() const;

	bool isVoid() const;
	bool isBoolean() const;
	bool isIntegral() const;
	bool isFloatingPoint() const;

	Node *getMember(const std::string &name);

//	raw_ostream &dump(raw_ostream &out, int ind) override {}
};

class AmbiguousExpr : public Expr, public TypeExpr
{
	friend class Semantic;

public:
	AmbiguousExpr(SourceLocation loc)
		: Expr(loc), TypeExpr(loc) {}

	std::string mangleof() const override
	{
		if (isExpr())
			return Expr::mangleof();
		return std::string();
	}

	virtual bool isType() const = 0;
	virtual bool isExpr() const { return !isType(); }

	Node* resolve()
	{
		if (isType())
			return resolveType();
		return resolveExpr();
	}

	Node *getMember(const std::string &name)
	{
		if (isType())
			return TypeExpr::getMember(name);
		return Expr::getMember(name);
	}

//	raw_ostream &dump(raw_ostream &out, int ind) override {}
};

class Statement : public virtual Node
{
	friend class Semantic;
public:

	Statement(SourceLocation loc)
		: Node(loc)
	{}

	virtual bool didReturn() const = 0;
};

class Module : public virtual Node, public Scope
{
	friend class Semantic;

	std::string _filename;
	std::string _name;

	StatementList moduleStatements;

public:
	Module(std::string filename, std::string name, StatementList moduleStatements, SourceLocation loc)
		: Node(loc), Scope(nullptr, loc), _filename(move(filename)), _name(move(name)), moduleStatements(moduleStatements)
	{
	}

	std::string stringof() const override { return _name; assert(false); } // TODO: test me!
	std::string mangleof() const override { return "_M" + _name; assert(false); } // TODO: test me!

	const std::string& filename() const { return _filename; }
	const std::string& name() const { return _name; }
	std::string& name() { return _name; }

	StatementList statements() { return moduleStatements; }

	void accept(ASTVisitor &v) override;
};


//*********************
//** Statement nodes **
//*********************

class Declaration : public Statement
{
	friend class Semantic;
protected:

	std::string _name;
	std::string _mangledName;

public:
	const std::string& name() const { return _name; }
	const std::string& mangledName() const { return _mangledName; }

	Declaration(std::string name, SourceLocation loc)
		: Statement(loc), _name(std::move(name))
	{}

	bool didReturn() const override { return false; }

	void accept(ASTVisitor &v) override;
};

class ModuleStatement : public Statement
{
	friend class Semantic;

	std::string _name;

public:
	ModuleStatement(std::string name, SourceLocation loc)
		: Node(loc), Statement(loc), _name(move(name))
	{
	}

	const std::string& name() const { return _name; }

	bool didReturn() const override { return false; }

	std::string stringof() const override { return _name; assert(false); } // TODO: test me!
	std::string mangleof() const override { return "_M" + _name; assert(false); } // TODO: test me!

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
	ExpressionStatement(Expr *expression, SourceLocation loc)
		: Node(loc), Statement(loc), _expression(expression)
	{
	}

	Expr* expression() const { return _expression; }

	bool didReturn() const override { return false; }

	std::string stringof() const override { return _expression->stringof(); }
	std::string mangleof() const override { assert(false); return std::string(); }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class ReturnStatement : public Statement
{
	friend class Semantic;

	Expr *_expression;

public:
	ReturnStatement(Expr *expression, SourceLocation loc)
		: Node(loc), Statement(loc), _expression(expression)
	{
	}

	Expr* expression() const { return _expression; }

	bool didReturn() const override { return true; }

	std::string stringof() const override { return "return " + _expression->stringof(); }
	std::string mangleof() const override { assert(false); return std::string(); }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class ScopeStatement : public Statement, public Scope
{
	friend class Semantic;
	friend Statement* makeForEach(DeclList, Expr*, ScopeStatement*, SourceLocation);

	StatementList _statements;

	bool _didReturn = false;

	// TODO: scope attributes

public:
	ScopeStatement(StatementList statements, Scope *parentScope, SourceLocation loc)
		: Node(loc), Statement(loc), Scope(parentScope, loc), _statements(statements)
	{}

	StatementList statements() const { return _statements; }
	bool didReturn() const override { return _didReturn; }

	std::string stringof() const override { assert(false); return std::string(); }
	std::string mangleof() const override { assert(false); return std::string(); }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class IfStatement : public Statement, public Scope
{
	friend class Semantic;

	Expr *_cond;
	ScopeStatement *_then = nullptr;
	ScopeStatement *_else = nullptr;

	DeclList _initStatements;

public:
	IfStatement(Expr *cond, ScopeStatement *thenStatements, ScopeStatement *elseStatements, DeclList initStatements, SourceLocation loc)
		: Node(loc), Statement(loc), Scope(nullptr, loc), _cond(cond), _then(thenStatements), _else(elseStatements), _initStatements(initStatements)
	{}

	Expr *cond() { return _cond; }

	DeclList initStatements() { return _initStatements; }

	ScopeStatement* thenStatements() { return _then; }
	ScopeStatement* elseStatements() { return _else; }

	bool thenReturned() const { return _then && _then->didReturn(); }
	bool elseReturned() const { return _else && _else->didReturn(); }
	bool didReturn() const override { return thenReturned() && elseReturned(); }

	std::string stringof() const override { assert(false); return std::string(); }
	std::string mangleof() const override { assert(false); return std::string(); }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class LoopStatement : public Statement, public Scope
{
	friend class Semantic;

	DeclList _iterators;
	Expr *_cond;
	ExprList _increments;

	ScopeStatement* _body;

public:
	LoopStatement(DeclList iterators, Expr *cond, ExprList increments, ScopeStatement* body, SourceLocation loc)
		: Node(loc), Statement(loc), Scope(nullptr, loc), _iterators(iterators), _cond(cond), _increments(increments), _body(body)
	{}

	DeclList iterators() { return _iterators; }

	Expr *cond() { return _cond; }

	ScopeStatement* body() { return _body; }
	ExprList incrementExpressions() { return _increments; }

	bool didReturn() const override { return _body->didReturn(); }

	std::string stringof() const override { assert(false); return std::string(); }
	std::string mangleof() const override { assert(false); return std::string(); }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};


//****************
//** Type nodes **
//****************

enum class PrimType
{
	v, u1, i8, u8, c8, i16, u16, c16, i32, u32, c32, i64, u64, i128, u128, f16, f32, f64, f128, __NumTypes
};
extern PrimType SizeT_Type, SSizeT_Type;

extern uint8_t typeWidth[(size_t)PrimType::__NumTypes];
extern uint8_t typeBytes[(size_t)PrimType::__NumTypes];

class PrimitiveType : public TypeExpr
{
	friend class Semantic;

	PrimType _type;
	Expr *_init = nullptr;

public:
	static PrimitiveType* get(PrimType type, SourceLocation loc) { return makeType<PrimitiveType>(nullptr, type, loc); }

	PrimitiveType(PrimType type, SourceLocation loc) : Node(loc), TypeExpr(loc), _type(type) {}
	~PrimitiveType() {}

	PrimType type() const { return _type; }
	Expr* init() const override;

	size_t size() const override { return typeBytes[(int)_type]; }
	size_t alignment() const override { return typeBytes[(int)_type]; }

	Node *getMember(const std::string &name) override;

	bool isSame(const TypeExpr *other) const override;
	ConvType convertible(const TypeExpr *target) const override;
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override;

	void accept(ASTVisitor &v) override;

	std::string stringof() const override;
	std::string mangleof() const override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

enum class PtrType
{
	RawPtr,
	UniquePtr,
	BorrowedPtr,
	LValue
};
class PointerType : public TypeExpr
{
	friend class Semantic;

	PtrType _type;
	TypeExpr *_pointerTarget;
	Expr *_init = nullptr;

public:
	PointerType(PtrType type, TypeExpr *targetType, SourceLocation loc)
		: Node(loc), TypeExpr(loc), _type(type), _pointerTarget(targetType) {}
	~PointerType() {}

	PtrType ptrType() const { return _type; }
	TypeExpr *targetType() const { return _pointerTarget; }
	Expr* init() const override { return _init; }

	size_t size() const override { return typeBytes[(int)SizeT_Type]; }
	size_t alignment() const override { return typeBytes[(int)SizeT_Type]; }

	Node *getMember(const std::string &name) override;

	bool isSame(const TypeExpr *other) const override;
	ConvType convertible(const TypeExpr *target) const override;
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override;
	bool canPromote(PtrType to) const;

	void accept(ASTVisitor &v) override;

	std::string stringof() const override;
	std::string mangleof() const override { assert(false); return std::string(); }

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class Struct : public TypeExpr, public Scope
{
	friend class Semantic;

	struct DataMember
	{
		size_t offset;
		VarDecl *decl;
	};

	StatementList _members;
	std::vector<DataMember> _dataMembers;

	Expr *_init = nullptr;

	size_t _sizeof = 0, _alignment = 0;

public:
	Struct(StatementList members, SourceLocation loc)
		: Node(loc), TypeExpr(loc), Scope(nullptr, loc), _members(members)
	{}

	std::vector<DataMember>& dataMembers() { return _dataMembers; }
	size_t memberIndex(const std::string &name);

	Expr* init() const override { return _init; }

	size_t size() const override { return _sizeof; }
	size_t alignment() const override { return _alignment; }

	Node *getMember(const std::string &name) override;

	bool isSame(const TypeExpr *other) const override;
	ConvType convertible(const TypeExpr *target) const override;
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override;

	void accept(ASTVisitor &v) override;

	std::string stringof() const override;
	std::string mangleof() const override { assert(false); return std::string(); }

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class FunctionType : public TypeExpr
{
	friend class Semantic;

	TypeExpr *_returnType = nullptr;
	TypeExprList _args = TypeExprList::empty();

public:
	FunctionType(TypeExpr *returnType, TypeExprList args, SourceLocation loc)
		: Node(loc), TypeExpr(loc), _returnType(returnType), _args(args) {}
	~FunctionType() {}

	Expr* init() const override { assert(false); return nullptr; }

	size_t size() const override { return 0; }
	size_t alignment() const override { return 0; }

	Node *getMember(const std::string &name) override { assert(false); return nullptr; }

	bool isSame(const TypeExpr *other) const override;
	ConvType convertible(const TypeExpr *target) const override;
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override { assert(false); return nullptr; }

	TypeExpr* returnType() const { return _returnType; }
	TypeExprList argTypes() const { return _args; }

	void accept(ASTVisitor &v) override;

	std::string stringof() const override;
	std::string mangleof() const override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};


//***********************
//** Declaration nodes **
//***********************

class TypeDecl : public Declaration
{
	friend class Semantic;
protected:
	TypeExpr *_type;

public:
	TypeDecl(std::string name, TypeExpr *type, SourceLocation loc) // TODO: template args
		: Node(loc), Declaration(move(name), loc), _type(type)
	{}

	TypeExpr* type() { return _type; }

	std::string stringof() const override { assert(false); return std::string(); }
	std::string mangleof() const override { assert(false); return std::string(); }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class ValDecl : public Declaration
{
	friend class Semantic;
protected:
	TypeExpr *_type;
	Expr *_value;

public:
	ValDecl(std::string name, TypeExpr *type, Expr *value, SourceLocation loc)
		: Node(loc), Declaration(move(name), loc), _type(type), _value(value)
	{}

	TypeExpr* type() { return _type; }
	Expr* value() { return _value; }

	std::string stringof() const override { assert(false); return std::string(); }
	std::string mangleof() const override { assert(false); return std::string(); }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};
class VarDecl : public ValDecl
{
	friend class Semantic;
	friend Statement* makeForEach(DeclList, Expr*, ScopeStatement*, SourceLocation);
protected:
	TypeExpr *_valType;
	Expr *_init;

public:
	VarDecl(std::string name, TypeExpr *type, Expr *init, SourceLocation loc)
		: Node(loc), ValDecl(move(name), nullptr, nullptr, loc), _valType(type), _init(init)
	{}

	TypeExpr* targetType() { return _valType; }
	Expr* init() { return _init; }

	std::string stringof() const override { assert(false); return std::string(); }
	std::string mangleof() const override { assert(false); return std::string(); }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};


//**********************
//** Expression nodes **
//**********************

class PrimitiveLiteralExpr : public Expr
{
	friend class Semantic;

	PrimType _type;
	PrimitiveType *_typeExpr = nullptr;

	union
	{
		double f;
		int64_t i;
		uint64_t u;
		char32_t c;
	};

public:
	PrimitiveLiteralExpr(PrimType type, uint64_t v, SourceLocation loc) : Node(loc), Expr(loc), _type(type), _typeExpr(PrimitiveType::get(type, loc)), u(v) {}
	PrimitiveLiteralExpr(PrimType type, int64_t v, SourceLocation loc) : Node(loc), Expr(loc), _type(type), _typeExpr(PrimitiveType::get(type, loc)), i(v) {}
	PrimitiveLiteralExpr(PrimType type, double v, SourceLocation loc) : Node(loc), Expr(loc), _type(type), _typeExpr(PrimitiveType::get(type, loc)), f(v) {}
	PrimitiveLiteralExpr(PrimType type, char32_t v, SourceLocation loc) : Node(loc), Expr(loc), _type(type), _typeExpr(PrimitiveType::get(type, loc)), c(v) {}
	PrimitiveLiteralExpr(uint64_t v, SourceLocation loc) : PrimitiveLiteralExpr(PrimType::u64, v, loc) {}
	PrimitiveLiteralExpr(int64_t v, SourceLocation loc) : PrimitiveLiteralExpr(PrimType::i64, v, loc) {}
	PrimitiveLiteralExpr(double v, SourceLocation loc) : PrimitiveLiteralExpr(PrimType::f64, v, loc) {}
	PrimitiveLiteralExpr(char32_t v, SourceLocation loc) : PrimitiveLiteralExpr(PrimType::c32, v, loc) {}
	PrimitiveLiteralExpr(bool b, SourceLocation loc) : PrimitiveLiteralExpr(PrimType::u1, b ? 1ull : 0ull, loc) {}

	PrimType primType() const { return _type; }
	double getFloat() const { return f; }
	int64_t getInt() const { return i; }
	uint64_t getUint() const { return u; }
	char32_t getChar() const { return c; }

	PrimitiveType* type() override { return _typeExpr; }
	PrimitiveLiteralExpr* constEval() override { return this; };

	std::string stringof() const override;
	std::string mangleof() const override { assert(false); return std::string(); }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class AggregateLiteralExpr : public Expr
{
	friend class Semantic;

	TypeExpr *_type;
	ExprList _items;

public:
	AggregateLiteralExpr(ExprList items, TypeExpr *type, SourceLocation loc)
		: Node(loc), Expr(loc), _type(type), _items(move(items)) {}

	TypeExpr* type() override { return _type; }

	ExprList items() { return _items; }

	std::string stringof() const override { assert(false); return std::string(); }
	std::string mangleof() const override { assert(false); return std::string(); }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class FunctionLiteralExpr : public Expr, public Scope
{
	friend class Semantic;

	StatementList bodyStatements;

	DeclList _args;
	TypeExpr *returnType;
	bool inferReturnType;

	TypeExprList argTypes = TypeExprList::empty();
	::FunctionType *_type = nullptr;

public:
	FunctionLiteralExpr(StatementList bodyStatements, DeclList args, TypeExpr *returnType, SourceLocation loc)
		: Node(loc), Expr(loc), Scope(nullptr, loc), bodyStatements(bodyStatements), _args(args), returnType(returnType), inferReturnType(returnType == nullptr)
	{}

	::FunctionType* type() override { return _type; }

	DeclList args() const { return _args; }
	StatementList statements() { return bodyStatements; }

	Node *getMember(const std::string &name) override { return Expr::getMember(name); }

	std::string stringof() const override { assert(false); return std::string(); }
	std::string mangleof() const override { assert(false); return std::string(); }

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

class RefExpr : public Expr
{
	friend class Semantic;
public:
	enum Type
	{
		Direct,
		Member,
		Index,
		Absolute
	};

private:
	::PointerType *_type = nullptr;

	RefExpr *_owner = nullptr;
	VarDecl *_target = nullptr;
	size_t _absolute = 0;
	size_t _element = 0;
	Expr *_index = nullptr;

	Type _refType;

public:

	RefExpr(VarDecl *target, RefExpr *owner, SourceLocation loc)
		: Node(loc), Expr(loc), _owner(owner), _target(target), _refType(owner ? Type::Member : Type::Direct) {}
	RefExpr(RefExpr *owner, size_t element, SourceLocation loc)
		: Node(loc), Expr(loc), _owner(owner), _element(element), _refType(Type::Index) {}
	RefExpr(RefExpr *owner, Expr *index, SourceLocation loc)
		: Node(loc), Expr(loc), _owner(owner), _index(index), _refType(Type::Index) {}
	RefExpr(PtrType ptrType, size_t target, TypeExpr *targetType, SourceLocation loc)
		: Node(loc), Expr(loc), _type(new ::PointerType(ptrType, targetType, loc)), _absolute(target), _refType(Type::Absolute) {}

	::PointerType* type() override { return _type; }
	TypeExpr* targetType() const { return _type->targetType(); }
	Type refType() const { return _refType; }

	RefExpr* owner() { return _owner; }
	VarDecl* target() { return _target; }
	size_t address() { return _absolute; }
	size_t element() { return _element; }
	Expr* index() { return _index; }

	Node *getMember(const std::string &name) override;

	std::string stringof() const override { assert(false); return std::string(); }
	std::string mangleof() const override { assert(false); return std::string(); }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class DerefExpr : public Expr
{
	friend class Semantic;

	Expr *_expr;

public:
	DerefExpr(Expr *expr, SourceLocation loc)
		: Node(loc), Expr(loc), _expr(expr) {}

	Expr* expr() { return _expr; }
	TypeExpr* type() override { return _expr->type()->asPointer()->targetType(); }

	std::string stringof() const override { assert(false); return std::string(); }
	std::string mangleof() const override { assert(false); return std::string(); }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class TypeConvertExpr : public Expr
{
	friend class Semantic;

	Expr *_expr;
	TypeExpr *_newType;
	bool _implicit;

public:
	TypeConvertExpr(Expr *expr, TypeExpr *newType, bool implicit, SourceLocation loc)
		: Node(loc), Expr(loc), _expr(expr), _newType(newType), _implicit(implicit) {}

	Expr* expr() { return _expr; }
	TypeExpr* type() override { return _newType; }

	std::string stringof() const override { assert(false); return std::string(); }
	std::string mangleof() const override { assert(false); return std::string(); }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
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
	UnaryExpr(UnaryOp op, Expr *operand, SourceLocation loc)
		: Node(loc), Expr(loc), _op(op), _operand(operand) {}

	UnaryOp op() { return _op; }
	Expr* operand() { return _operand; }

	TypeExpr* type() override { return _type; }

	std::string stringof() const override { assert(false); return std::string(); }
	std::string mangleof() const override { assert(false); return std::string(); }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

enum class BinOp
{
	None,
	Add, Sub, Mul, Div, Mod, Pow, Cat,
	SHL, ASR, LSR,
	BitAnd, BitOr, BitXor, LogicAnd, LogicOr, LogicXor,
	Eq, Ne, Gt, Ge, Lt, Le,
	Is, IsNot, __NumOps
};
class BinaryExpr : public Expr
{
	friend class Semantic;

	BinOp _op;
	Expr *_lhs, *_rhs;
	TypeExpr *_type = nullptr;

public:
	BinaryExpr(BinOp op, Expr *lhs, Expr *rhs, SourceLocation loc)
		: Node(loc), Expr(loc), _op(op), _lhs(lhs), _rhs(rhs) {}

	BinOp op() { return _op; }
	Expr* lhs() { return _lhs; }
	Expr* rhs() { return _rhs; }

	TypeExpr* type() override { return _type; }

	std::string stringof() const override { assert(false); return std::string(); }
	std::string mangleof() const override { assert(false); return std::string(); }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class CallExpr : public Expr
{
	friend class Semantic;

	Expr *_func;
	ExprList _callArgs;

public:
	CallExpr(Expr *func, ExprList callArgs, SourceLocation loc)
		: Node(loc), Expr(loc), _func(func), _callArgs(callArgs)
	{}

	TypeExpr* type() override
	{
		::FunctionType *f = dynamic_cast<::FunctionType*>(_func->type());
		return f->returnType();
	}

	Expr* function() { return _func; }
	ExprList callArgs() { return _callArgs; }

	Node* getMember(const std::string &name) override;

	std::string stringof() const override { assert(false); return std::string(); }
	std::string mangleof() const override { assert(false); return std::string(); }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class AssignExpr : public Expr
{
	friend class Semantic;

	Expr *_target;
	Expr *_expr;
	BinOp _op;

public:
	AssignExpr(Expr *target, Expr *expr, BinOp op, SourceLocation loc)
		: Node(loc), Expr(loc), _target(target), _expr(expr), _op(op)
	{}

	TypeExpr* type() override { return _target->type(); }

	Expr *target() { return _target; }
	Expr *expr() { return _expr; }

	std::string stringof() const override { assert(false); return std::string(); }
	std::string mangleof() const override { assert(false); return std::string(); }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class BindExpr : public Expr
{
	friend class Semantic;

	Expr *_target;
	Expr *_expr;

public:
	BindExpr(Expr *target, Expr *expr, SourceLocation loc)
		: Node(loc), Expr(loc), _target(target), _expr(expr)
	{}

	TypeExpr* type() override { return _target->type(); }

	Expr *target() { return _target; }
	Expr *expr() { return _expr; }

	std::string stringof() const override { assert(false); return std::string(); }
	std::string mangleof() const override { assert(false); return std::string(); }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};


class Identifier : public AmbiguousExpr
{
	friend class Semantic;

	std::string _name;

	ValDecl *_var = nullptr;
	TypeDecl *_type = nullptr;

public:
	Identifier(const std::string &name, SourceLocation loc)
		: Node(loc), AmbiguousExpr(loc), _name(std::move(name)) {}

	const std::string &getName() const { return _name; }

	Declaration* target() const { return _var ? (Declaration*)_var : (Declaration*)_type; }

	bool isType() const override { return _type != nullptr; }

	// expr overrides
	TypeExpr* type() override { return resolveExpr()->type(); }
	Expr* constEval() override { return resolveExpr()->constEval(); };

	Expr* resolveExpr() override { return _var ? _var->value() : nullptr; }
	TypeExpr* resolveType() override { return _type ? _type->type() : nullptr; }

	// type overrides
	std::string stringof() const override
	{
		if (isType())
			return _type->stringof();
		return _name;
	}
	std::string mangleof() const override
	{
		if (isType())
			return _type->mangleof();
		assert(false); // TODO: what is the scope of the symbol?
		return _var->mangleof();
	}

	Expr* init() const override { return _type->type()->init(); }

	size_t size() const override { return _type->type()->size(); }
	size_t alignment() const override { return _type->type()->alignment(); }

	bool isSame(const TypeExpr *other) const override { return _type->type()->isSame(other); }
	ConvType convertible(const TypeExpr *target) const override { return _type->type()->convertible(target); }
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override { return _type->type()->makeConversion(expr, targetType, implicit); }

	Node *getMember(const std::string &name) override;

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class MemberLookup : public AmbiguousExpr
{
	friend class Semantic;

	std::string _member;

	Node *_node;
	Node *_result;

public:
	MemberLookup(Node *node, std::string member, SourceLocation loc)
		: Node(loc), AmbiguousExpr(loc), _member(move(member)), _node(node) {}

	Node* expr() const { return _result; }

	bool isType() const override { return dynamic_cast<TypeExpr*>(_result) != nullptr; }

	// expr overrides
	TypeExpr* type() override { return resolveExpr()->type(); }
	Expr* constEval() override { return resolveExpr()->constEval(); };

	Expr* resolveExpr() override { return dynamic_cast<Expr*>(_result); }
	TypeExpr* resolveType() override { return dynamic_cast<TypeExpr*>(_result); }

	// type overrides
	std::string stringof() const override
	{
		if (isType())
			return _result->stringof();
		return _node->stringof() + "." + _member;
	}
	std::string mangleof() const override
	{
		if (isType())
			return _result->mangleof();
		assert(false);
		return std::string();
	}

	Expr* init() const override { return dynamic_cast<TypeExpr*>(_result)->init(); }

	size_t size() const override { return dynamic_cast<TypeExpr*>(_result)->size(); }
	size_t alignment() const override { return dynamic_cast<TypeExpr*>(_result)->alignment(); }

	bool isSame(const TypeExpr *other) const override { return dynamic_cast<TypeExpr*>(_result)->isSame(other); }
	ConvType convertible(const TypeExpr *target) const override { return dynamic_cast<TypeExpr*>(_result)->convertible(target); }
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override { return dynamic_cast<TypeExpr*>(_result)->makeConversion(expr, targetType, implicit); }

	Node *getMember(const std::string &name) override;

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class Tuple : public AmbiguousExpr
{
	friend class Semantic;

	NodeList _elements = NodeList::empty();
	std::vector<size_t> _offsets;

	Node *_element = nullptr;
	ExprList _shape;
	Expr *_numElements = nullptr;

	bool allExpr = false, allTypes = false;
	size_t _size = 0, _alignment = 0;

	Tuple *_type = nullptr;
	Tuple *_init = nullptr;

public:
	Tuple(NodeList elements, SourceLocation loc)
		: Node(loc), AmbiguousExpr(loc), _elements(elements)
	{
		// check if all elements are the same, if so, make it into an array...
		//...
	}
	Tuple(Node *element, ExprList shape, SourceLocation loc)
		: Node(loc), AmbiguousExpr(loc), _element(element), _shape(shape)
	{}

	NodeList elements() const { return _elements; }

	bool isSequence() const { return _element != nullptr; }
	Node* seqElement() { return _element; }
	ExprList shape() { return _shape; }
	int dimensions() const { return _element == nullptr ? 1 : _shape.length; }
	ptrdiff_t numElements(int dimension = -1) const;
	bool isDynamicSize() const { return numElements() == -1; }
	Expr* dynamicSize() { return _numElements; }

	bool isType() const override { return allTypes; }
	bool isExpr() const override { return allExpr; }

	// expr overrides
	TypeExpr* type() override;
	Expr* constEval() override { assert(false); return nullptr; };

	Expr* resolveExpr() override { return this; }
	TypeExpr* resolveType() override { return this; }

	// type overrides
	std::string stringof() const override;
	std::string mangleof() const override
	{
//		if (isType())
//			return _result->mangleof();
		assert(false);
		return std::string();
	}

	Expr* init() const override;

	size_t size() const override { return _size; }
	size_t alignment() const override { return _alignment; }

	bool isSame(const TypeExpr *other) const override;
	ConvType convertible(const TypeExpr *target) const override;
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override;

	Node *getMember(const std::string &name) override; // length, etc

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;

private:
	void analyse();
};

class Index : public AmbiguousExpr
{
	friend class Semantic;

	Node *_node;
	Node *_result = nullptr;

	ExprList _indices;

public:
	Index(Node *node, ExprList indices, SourceLocation loc)
		: Node(loc), AmbiguousExpr(loc), _node(node), _indices(indices) {}

	Node* source() { return _node; }
	ExprList indices() { return _indices; }

	Node* expr() const { return _result; }

	bool isType() const override { return _result->asType() != nullptr; }

	// expr overrides
	TypeExpr* type() override { return resolveExpr()->type(); }
	Expr* constEval() override { return resolveExpr()->constEval(); };

	Expr* resolveExpr() override { return _result->asExpr(); }
	TypeExpr* resolveType() override { return _result->asType(); }

	// type overrides
	std::string stringof() const override
	{
		if (isType())
			return _result->stringof();
		std::string t = _node->stringof() + "[";
		bool first = true;
		for (auto &i : _indices)
		{
			if (first)
				first = false;
			else
				t += ", ";
			t += i->stringof();
		}
		return t + "]";
	}
	std::string mangleof() const override
	{
		if (isType())
			return _result->mangleof();
		assert(false);
		return std::string();
	}

	Expr* init() const override { return dynamic_cast<TypeExpr*>(_result)->init(); }

	size_t size() const override { return dynamic_cast<TypeExpr*>(_result)->size(); }
	size_t alignment() const override { return dynamic_cast<TypeExpr*>(_result)->alignment(); }

	bool isSame(const TypeExpr *other) const override { return dynamic_cast<TypeExpr*>(_result)->isSame(other); }
	ConvType convertible(const TypeExpr *target) const override { return dynamic_cast<TypeExpr*>(_result)->convertible(target); }
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override { return dynamic_cast<TypeExpr*>(_result)->makeConversion(expr, targetType, implicit); }

	Node *getMember(const std::string &name) override;

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

/*
/// Prototype - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes), as well as if it is an operator.
class PrototypeDecl : public Declaration
{
	friend class Semantic;

	std::vector<std::string> Args;
	bool IsOperator;
	unsigned Precedence; // Precedence if a binary op.
	int Line;

public:
	PrototypeDecl(SourceLocation Loc, const std::string &Name, std::vector<std::string> Args, bool IsOperator = false, unsigned Prec = 0)
		: Declaration(move(Name), Loc), Args(std::move(Args)), IsOperator(IsOperator), Precedence(Prec), Line(Loc.line)
	{}

	const std::string &name() const { return _name; }

	bool isUnaryOp() const { return IsOperator && Args.size() == 1; }
	bool isBinaryOp() const { return IsOperator && Args.size() == 2; }

	char getOperatorName() const
	{
		assert(isUnaryOp() || isBinaryOp());
		return _name[_name.size() - 1];
	}

	unsigned getBinaryPrecedence() const { return Precedence; }
	int getLine() const { return Line; }

	void accept(ASTVisitor &v) override;
};

/// Function - This class represents a function definition itself.
class FunctionDecl : public Declaration, public Scope
{
	friend class Semantic;

	owner<PrototypeDecl> Proto;
	owner<Expr> Body;

public:
	FunctionDecl(owner<PrototypeDecl> Proto, owner<Expr> Body)
		: Declaration(""), Scope(nullptr), Proto(std::move(Proto)), Body(std::move(Body))
	{}

	std::string stringof() const override { assert(false); return std::string(); }
	std::string mangleof() const override { assert(false); return std::string(); }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind)
	{
		indent(out, ind) << "Function\n";
		++ind;
		indent(out, ind) << "Body:";
		return Body ? Body->dump(out, ind) : out << "null\n";
	}
};
*/

Statement* makeForEach(DeclList iterators, Expr *range, StatementList body, SourceLocation loc);

template <typename TypeNode, typename... Args>
struct MakeTypeHelper
{
	static TypeNode* makeType(Scope *scope, Args&&... args)
	{
		pNew = nullptr; // find

		if (!pNew)
		{
			TypeNode *pNew = new TypeNode(std::forward<Args>(args)...);
			// insert...
		}
		return pNew;
	}
};
template <typename... Args>
struct MakeTypeHelper<PrimitiveType, Args...>
{
	static PrimitiveType* makeType(Scope *scope, PrimType type, SourceLocation loc);
};






// inlines
inline Expr* Node::asExpr()
{
	AmbiguousExpr *e = dynamic_cast<AmbiguousExpr*>(this);
	if (e && e->isExpr())
		return e->resolveExpr();
	return dynamic_cast<Expr*>(this);
}
inline TypeExpr* Node::asType()
{
	AmbiguousExpr *e = dynamic_cast<AmbiguousExpr*>(this);
	if (e && e->isType())
		return e->resolveType();
	return dynamic_cast<TypeExpr*>(this);
}

inline bool Expr::isConstantInt() { return type()->isIntegral() && constEval() != nullptr; }
inline int64_t Expr::getIntValue() { return dynamic_cast<PrimitiveLiteralExpr*>(constEval())->getInt(); }
inline Expr* Expr::makeConversion(TypeExpr *targetType, bool implicit)
{
	return type()->makeConversion(this, targetType, implicit);
}

inline TypeExpr* TypeExpr::resultType()
{
	::FunctionType *f = asFunction();
	if (f)
		return f->returnType()->evalType();
	return this;
}
inline TypeExpr* TypeExpr::evalType()
{
	::FunctionType *f = asFunction();
	if (f)
		return f->returnType()->evalType();
	::PointerType *p = asPointer();
	if (p)
		return p->targetType()->evalType();
	return this;
}

inline PrimitiveType* TypeExpr::asPrimitive() { return dynamic_cast<PrimitiveType*>(this); }
inline const PrimitiveType* TypeExpr::asPrimitive() const { return dynamic_cast<const PrimitiveType*>(this); }
inline ::FunctionType* TypeExpr::asFunction() { return dynamic_cast<::FunctionType*>(this); }
inline const ::FunctionType* TypeExpr::asFunction() const { return dynamic_cast<const ::FunctionType*>(this); }
inline ::PointerType* TypeExpr::asPointer() { return dynamic_cast<::PointerType*>(this); }
inline const ::PointerType* TypeExpr::asPointer() const { return dynamic_cast<const ::PointerType*>(this); }
inline Struct* TypeExpr::asStruct() { return dynamic_cast<Struct*>(this); }
inline const Struct* TypeExpr::asStruct() const { return dynamic_cast<const Struct*>(this); }
inline Tuple* TypeExpr::asTuple() { return dynamic_cast<Tuple*>(this); }
inline const Tuple* TypeExpr::asTuple() const { return dynamic_cast<const Tuple*>(this); }
inline bool TypeExpr::isVoid() const { const PrimitiveType *pt = dynamic_cast<const PrimitiveType*>(this); return pt && pt->type() == PrimType::v; }
inline bool TypeExpr::isBoolean() const { const PrimitiveType *pt = dynamic_cast<const PrimitiveType*>(this); return pt && isBool(pt->type()); }
inline bool TypeExpr::isIntegral() const { const PrimitiveType *pt = dynamic_cast<const PrimitiveType*>(this); return pt && isInt(pt->type()); }
inline bool TypeExpr::isFloatingPoint() const { const PrimitiveType *pt = dynamic_cast<const PrimitiveType*>(this); return pt && isFloat(pt->type()); }
