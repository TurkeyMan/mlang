﻿#pragma once

#include "gc_cpp.h"
#include "gc_allocator.h"

#include "sourceloc.h"
#include "util.h"
#include "error.h"

#include <map>
#include <set>

#include "common_llvm.h"
#include <llvm/Support/raw_ostream.h>
#include <llvm/ADT/ArrayRef.h>

using llvm::raw_ostream;

namespace m {

template <typename T, bool S>
inline llvm::ArrayRef<T> arr_ref(Slice<T, S> arr) { return{ arr.ptr, arr.length }; }
inline llvm::StringRef str_ref(String str) { return{ str.ptr, str.length }; }

struct string_less
{
	typedef String first_argument_type;
	typedef String second_argument_type;
	typedef bool result_type;
	/*constexpr*/ bool operator()(String _Left, String _Right) const
	{
		return _Left.cmp(_Right) < 0;
	}
};

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


class ASTVisitor;


//using string = std::basic_string<char, std::char_traits<char>, gc_allocator<char>>;
//template <class K, class T, class P = std::less<K>>
//using map = std::map<K, T, P, gc_allocator<std::pair<const K, T>>>;


enum TypeFlags { TF_Unsigned = 1, TF_Signed = 2, TF_Char = 4, TF_Float = 8, TF_Bool = 16 };

extern uint8_t typeFlags[];
extern uint8_t typeWidth[];
extern uint8_t typeBytes[];

#define isFloat(pt) (typeFlags[(int)pt] & TF_Float)
#define isNotFloat(pt) (typeFlags[(int)pt] & ~TF_Float)
#define isSignedInt(pt) (typeFlags[(int)pt] & TF_Signed)
#define isUnsignedInt(pt) (typeFlags[(int)pt] & TF_Unsigned)
#define isInt(pt) (typeFlags[(int)pt] > 0 && typeFlags[(int)pt] < TF_Char)
#define isChar(pt) (typeFlags[(int)pt] & TF_Char)
#define isBool(pt) (typeFlags[(int)pt] & TF_Bool)

#define tyWidth(pt) typeWidth[(int)pt]
#define tyBytes(pt) typeBytes[(int)pt]


inline raw_ostream &indent(raw_ostream &O, int size) {
	return O << std::string(size*2, ' ');
}

// prototype nodes used for arguments?
class Module;

class Statement;
class Declaration;
class ValDecl;
class VarDecl;

class Node;
class Expr;
class TypeExpr;
class AmbiguousExpr;

class PrimitiveType;
class ModifiedType;
class FunctionType;
class PointerType;
class Struct;
class Tuple;

class ModuleDecl;

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

	virtual MutableString64 stringof() const = 0;
	virtual MutableString64 mangleof() const = 0;

	// all nodes may have properties
	virtual Node* getMember(String name);

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

	Array<Declaration*> _symbolTable;
	std::map<SharedString, Declaration*, string_less> _symbols;
	std::set<Module*> _imports;

	Scope *_parentScope = nullptr; // parent scope
	Module *_module = nullptr;

	MutableString64 stringof() const override { assert(false); return String(); }
	MutableString64 mangleof() const override { assert(false); return String(); }

public:
	Scope(Scope *parent, SourceLocation loc)
		: Node(loc), _parentScope(parent)
	{}

	Module *module() const { return _module; }
	Scope *parentScope() const { return _parentScope; }
	Declaration *getDecl(String name, bool onlyLocal = false);
	void addDecl(SharedString name, Declaration *decl);
	const auto& symbols() const { return _symbolTable; }

	void setParent(Scope *parent) { _parentScope = parent; }
};

extern std::map<SharedString, TypeExpr*, string_less> typesUsed;

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
protected:
	bool _isConstant = false;

public:
	Expr(bool isConst, SourceLocation loc) : Node(loc), _isConstant(isConst) {}

	virtual TypeExpr* type() = 0;

	virtual MutableString64 mangleof() const override { return String(); }

	Expr* makeConversion(TypeExpr *targetType, bool implicit = true);

	virtual Expr* resolveExpr() { return this; }

	// TODO: constant evaluation
	virtual Expr* constEval() { return nullptr; };

	Node* getMember(String name) override;

	bool isConstant() const { return _isConstant; }

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
	ModifiedType* asModified();
	const ModifiedType* asModified() const;
	FunctionType* asFunction();
	const FunctionType* asFunction() const;
	PointerType* asPointer();
	const PointerType* asPointer() const;
	Struct* asStruct();
	const Struct* asStruct() const;
	Tuple* asTuple();
	const Tuple* asTuple() const;

	bool isVoid() const;
	bool isBoolean() const;
	bool isIntegral() const;
	bool isFloatingPoint() const;
	bool isSomeChar() const;
	bool isConst() const;

	Node *getMember(String name) override;

//	raw_ostream &dump(raw_ostream &out, int ind) override {}
};

class AmbiguousExpr : public Expr, public TypeExpr
{
	friend class Semantic;

public:
	AmbiguousExpr(SourceLocation loc)
		: Expr(false, loc), TypeExpr(loc) {}

	MutableString64 mangleof() const override
	{
		if (isExpr())
			return Expr::mangleof();
		return String();
	}

	virtual bool isType() const = 0;
	virtual bool isExpr() const { return !isType(); }

	Node* resolve()
	{
		if (isType())
			return resolveType();
		return resolveExpr();
	}

	Node *getMember(String name)
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

	Array<SharedString> _name;
	SharedString _path;
	SharedString _filename;
	SharedString _directory;

	std::map<SharedString, Module*, string_less> _submodules;

	StatementList moduleStatements;

	ModuleDecl *_declaration;

public:
	Module(SharedString path, SharedString filename, Array<SharedString> moduleName, StatementList moduleStatements, ModuleDecl *moduleDecl)
		: Node(SourceLocation(0)), Scope(nullptr, SourceLocation(0)), _name(std::move(moduleName)), _path(std::move(path)), _filename(std::move(filename)), moduleStatements(moduleStatements), _declaration(moduleDecl)
	{}

	MutableString64 stringof() const override;
	MutableString64 mangleof() const override;

	const SharedString& path() const { return _path; }
	const SharedString& filename() const { return _filename; }
	const SharedString& directory() const { return _directory; }
//	MutableString64 fqn() const { return  }
	const Array<SharedString>& fullName() const { return _name; }
	const SharedString& name() const { return _name.back(); }
	SharedString& name() { return _name.back(); }

	std::map<SharedString, Module*, string_less>& submodules() { return _submodules; }

	StatementList statements() { return moduleStatements; }

	ModuleDecl *getModuleDecl() const { return _declaration; }

	void accept(ASTVisitor &v) override;
};


//*********************
//** Statement nodes **
//*********************

class Declaration : public Statement
{
	friend class Semantic;
protected:

	NodeList _attributes;

	SharedString _name;
	mutable SharedString _mangledName;

	Scope *_owner = nullptr;

public:
	const SharedString& name() const { return _name; }
	const SharedString& mangledName() const;

	Declaration(SharedString name, NodeList attrs, SourceLocation loc)
		: Statement(loc), _attributes(std::move(attrs)), _name(std::move(name))
	{}

	NodeList attributes() const { return _attributes; }
	void appendAttributes(NodeList attrs) { _attributes.append(attrs); }

	bool didReturn() const override { return false; }

	Node *getMember(String name) override;

	void accept(ASTVisitor &v) override;
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

	MutableString64 stringof() const override { return _expression->stringof(); }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

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

	MutableString64 stringof() const override { return MutableString64(Concat, "return ", _expression->stringof()); }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

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

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

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

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

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

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

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

	Node *getMember(String name) override;

	bool isSame(const TypeExpr *other) const override;
	ConvType convertible(const TypeExpr *target) const override;
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override;

	void accept(ASTVisitor &v) override;

	MutableString64 stringof() const override;
	MutableString64 mangleof() const override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

enum TypeMod : uint32_t
{
	Const = 1
};
class ModifiedType : public TypeExpr
{
	friend class Semantic;

	TypeMod _mod;
	TypeExpr *_type;
	Expr *_init = nullptr;

public:
	ModifiedType(TypeMod mods, TypeExpr *type, SourceLocation loc)
		: Node(loc), TypeExpr(loc), _mod(mods), _type(type) {}
	~ModifiedType() {}

	static ModifiedType* makeModified(TypeMod mods, TypeExpr *type, SourceLocation loc);

	TypeMod mod() const { return _mod; }
	TypeExpr *innerType() const { return _type; }
	Expr* init() const override { return _type->init(); }

	size_t size() const override { return _type->size(); }
	size_t alignment() const override { return _type->alignment(); }

	Node *getMember(String name) override;

	bool isSame(const TypeExpr *other) const override;
	ConvType convertible(const TypeExpr *target) const override;
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override;

	void accept(ASTVisitor &v) override;

	MutableString64 stringof() const override;
	MutableString64 mangleof() const override;

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

	Node *getMember(String name) override;

	bool isSame(const TypeExpr *other) const override;
	ConvType convertible(const TypeExpr *target) const override;
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override;
	bool canPromote(PtrType to) const;

	void accept(ASTVisitor &v) override;

	MutableString64 stringof() const override;
	MutableString64 mangleof() const override;

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
	Array<DataMember> _dataMembers;

	Expr *_init = nullptr;

	size_t _sizeof = 0, _alignment = 0;

	SharedString _givenName;
	SourceLocation _defLoc = SourceLocation(0);

public:
	Struct(StatementList members, SourceLocation loc)
		: Node(loc), TypeExpr(loc), Scope(nullptr, loc), _members(members)
	{}

	Array<DataMember>& dataMembers() { return _dataMembers; }
	size_t memberIndex(String name);

	Expr* init() const override { return _init; }

	size_t size() const override { return _sizeof; }
	size_t alignment() const override { return _alignment; }

	SharedString& givenName() { return _givenName; }
	SourceLocation& defLoc() { return _defLoc; }

	Node *getMember(String name) override;

	bool isSame(const TypeExpr *other) const override;
	ConvType convertible(const TypeExpr *target) const override;
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override;

	void accept(ASTVisitor &v) override;

	MutableString64 stringof() const override;
	MutableString64 mangleof() const override { assert(false); return nullptr; }

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class FunctionType : public TypeExpr
{
	friend class Semantic;

	TypeExpr *_returnType = nullptr;
	TypeExprList _argTypes = TypeExprList::empty();
	DeclList _args = DeclList::empty();

public:
	FunctionType(TypeExpr *returnType, TypeExprList args, SourceLocation loc)
		: Node(loc), TypeExpr(loc), _returnType(returnType), _argTypes(args) {}
	FunctionType(TypeExpr *returnType, DeclList args, SourceLocation loc)
		: Node(loc), TypeExpr(loc), _returnType(returnType), _args(args) {}
	~FunctionType() {}

	Expr* init() const override { assert(false); return nullptr; }

	size_t size() const override { return 0; }
	size_t alignment() const override { return 0; }

	Node *getMember(String name) override { assert(false); return nullptr; }

	bool isSame(const TypeExpr *other) const override;
	ConvType convertible(const TypeExpr *target) const override;
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override { assert(false); return nullptr; }

	TypeExpr* returnType() const { return _returnType; }
	TypeExprList argTypes() const { return _argTypes; }

	void accept(ASTVisitor &v) override;

	MutableString64 stringof() const override;
	MutableString64 mangleof() const override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};


//***********************
//** Declaration nodes **
//***********************

class ModuleDecl : public Declaration
{
	friend class Semantic;

	Module *_module = nullptr;

public:
	ModuleDecl(SharedString name, NodeList attrs, SourceLocation loc)
		: Node(loc), Declaration(name, std::move(attrs), loc)
	{
	}

	bool didReturn() const override { return false; }

	MutableString64 stringof() const override { return _name; assert(false); } // TODO: test me!
	MutableString64 mangleof() const override { return MutableString64(Concat, "_M", _name); assert(false); } // TODO: test me!

	Node *getMember(String name) override;

	Module* module() { return _module; }
	void setModule(Module *module) { _module = module; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override {
		return Statement::dump(out << "module: #" << str_ref(_name) << '\n', ind);
	}
};

class ImportDecl : public Declaration
{
	friend class Semantic;

	Module *_module = nullptr;

public:
	ImportDecl(SharedString name, NodeList attrs, SourceLocation loc)
		: Node(loc), Declaration(name, std::move(attrs), loc)
	{
	}

	bool didReturn() const override { return false; }

	MutableString64 stringof() const override { return _name; assert(false); } // TODO: test me!
	MutableString64 mangleof() const override { return MutableString64(Concat, "_M", _name); assert(false); } // TODO: test me!

	Node *getMember(String name) override;

	Module* module() { return _module; }
	void setModule(Module *module) { _module = module; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override {
		return Statement::dump(out << "import: #" << str_ref(_name) << '\n', ind);
	}
};

class TypeDecl : public Declaration
{
	friend class Semantic;
protected:
	TypeExpr *_type;

public:
	TypeDecl(SharedString name, TypeExpr *type, NodeList attrs, SourceLocation loc) // TODO: template args
		: Node(loc), Declaration(std::move(name), std::move(attrs), loc), _type(type)
	{}

	TypeExpr* type() { return _type; }

	MutableString64 stringof() const override { return _name; }
	MutableString64 mangleof() const override { return MutableString64(Concat, std::to_string(_name.size()), _name); }

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
	ValDecl(SharedString name, TypeExpr *type, Expr *value, NodeList attrs, SourceLocation loc)
		: Node(loc), Declaration(std::move(name), std::move(attrs), loc), _type(type), _value(value)
	{}

	TypeExpr* type() { return _type; }
	Expr* value() { return _value; }

	MutableString64 stringof() const override;
	MutableString64 mangleof() const override;

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
	VarDecl(SharedString name, TypeExpr *type, Expr *init, NodeList attrs, SourceLocation loc)
		: Node(loc), ValDecl(std::move(name), nullptr, nullptr, std::move(attrs), loc), _valType(type), _init(init)
	{}

	TypeExpr* targetType() { return _valType; }
	Expr* init() { return _init; }

	MutableString64 stringof() const override;
	MutableString64 mangleof() const override;

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
	PrimitiveLiteralExpr(PrimType type, uint64_t v, SourceLocation loc) : Node(loc), Expr(true, loc), _type(type), _typeExpr(PrimitiveType::get(type, loc)), u(v) {}
	PrimitiveLiteralExpr(PrimType type, int64_t v, SourceLocation loc) : Node(loc), Expr(true, loc), _type(type), _typeExpr(PrimitiveType::get(type, loc)), i(v) {}
	PrimitiveLiteralExpr(PrimType type, double v, SourceLocation loc) : Node(loc), Expr(true, loc), _type(type), _typeExpr(PrimitiveType::get(type, loc)), f(v) {}
	PrimitiveLiteralExpr(PrimType type, char32_t v, SourceLocation loc) : Node(loc), Expr(true, loc), _type(type), _typeExpr(PrimitiveType::get(type, loc)), c(v) {}
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

	MutableString64 stringof() const override;
	MutableString64 mangleof() const override { assert(false); return nullptr; }

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
		: Node(loc), Expr(false, loc), _type(type), _items(std::move(items)) {}

	TypeExpr* type() override { return _type; }

	ExprList items() { return _items; }

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

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
	FunctionType *_type = nullptr;

	SharedString _givenName;
	SourceLocation _defLoc = SourceLocation(0);

public:
	FunctionLiteralExpr(StatementList bodyStatements, DeclList args, TypeExpr *returnType, SourceLocation loc)
		: Node(loc), Expr(true, loc), Scope(nullptr, loc), bodyStatements(bodyStatements), _args(args), returnType(returnType), inferReturnType(returnType == nullptr)
	{
		static int literalCount = 0;
		_givenName = SharedString(Concat, "fn_literal_", std::to_string(literalCount++));
	}

	FunctionType* type() override { return _type; }

	DeclList args() const { return _args; }
	StatementList statements() { return bodyStatements; }

	Node *getMember(String name) override { return Expr::getMember(name); }

	SharedString& givenName() { return _givenName; }
	SourceLocation& defLoc() { return _defLoc; }

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

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
	PointerType *_type = nullptr;

	Expr *_owner = nullptr;
	VarDecl *_target = nullptr;
	size_t _absolute = 0;
	size_t _element = 0;
	Expr *_index = nullptr;

	Type _refType;

public:

	RefExpr(VarDecl *target, RefExpr *owner, SourceLocation loc)
		: Node(loc), Expr(false, loc), _owner(owner), _target(target), _refType(owner ? Type::Member : Type::Direct) {}
	RefExpr(Expr *owner, size_t element, SourceLocation loc)
		: Node(loc), Expr(false, loc), _owner(owner), _element(element), _refType(Type::Index) {}
	RefExpr(Expr *owner, Expr *index, SourceLocation loc)
		: Node(loc), Expr(false, loc), _owner(owner), _index(index), _refType(Type::Index) {}
	RefExpr(PtrType ptrType, size_t target, TypeExpr *targetType, SourceLocation loc)
		: Node(loc), Expr(false, loc), _type(new PointerType(ptrType, targetType, loc)), _absolute(target), _refType(Type::Absolute) {}

	PointerType* type() override { return _type; }
	TypeExpr* targetType() const { return _type->targetType(); }
	Type refType() const { return _refType; }

	Expr* owner() { return _owner; }
	VarDecl* target() { return _target; }
	size_t address() { return _absolute; }
	size_t element() { return _element; }
	Expr* index() { return _index; }

	Node *getMember(String name) override;

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class DerefExpr : public Expr
{
	friend class Semantic;

	Expr *_expr;

public:
	DerefExpr(Expr *expr, SourceLocation loc)
		: Node(loc), Expr(false, loc), _expr(expr) {}

	Expr* expr() { return _expr; }
	TypeExpr* type() override { return _expr->type()->asPointer()->targetType(); }

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

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
		: Node(loc), Expr(expr->isConstant(), loc), _expr(expr), _newType(newType), _implicit(implicit) {}

	Expr* expr() { return _expr; }
	TypeExpr* type() override { return _newType; }

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

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
		: Node(loc), Expr(operand->isConstant(), loc), _op(op), _operand(operand) {}

	UnaryOp op() { return _op; }
	Expr* operand() { return _operand; }

	TypeExpr* type() override { return _type; }

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

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
		: Node(loc), Expr(lhs->isConstant() && rhs->isConstant(), loc), _op(op), _lhs(lhs), _rhs(rhs) {}

	BinOp op() { return _op; }
	Expr* lhs() { return _lhs; }
	Expr* rhs() { return _rhs; }

	TypeExpr* type() override { return _type; }

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

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
		: Node(loc), Expr(false, loc), _func(func), _callArgs(callArgs)
	{}

	TypeExpr* type() override
	{
		FunctionType *f = dynamic_cast<FunctionType*>(_func->type());
		return f->returnType();
	}

	Expr* function() { return _func; }
	ExprList callArgs() { return _callArgs; }

	Node* getMember(String name) override;

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

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
		: Node(loc), Expr(false, loc), _target(target), _expr(expr), _op(op)
	{}

	TypeExpr* type() override { return _target->type(); }

	Expr *target() { return _target; }
	Expr *expr() { return _expr; }

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

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
		: Node(loc), Expr(false, loc), _target(target), _expr(expr)
	{}

	TypeExpr* type() override { return _target->type(); }

	Expr *target() { return _target; }
	Expr *expr() { return _expr; }

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class UnknownExpr : public AmbiguousExpr
{
	friend class Semantic;

	Node *_node;

public:
	UnknownExpr(Node *node, SourceLocation loc)
		: Node(loc), AmbiguousExpr(loc), _node(node) {}

	Node *node() const { return _node; }

	bool isType() const override { return dynamic_cast<TypeExpr*>(_node) != nullptr; }
	bool isExpr() const override { return dynamic_cast<Expr*>(_node) != nullptr; }

	MutableString64 stringof() const override { return _node->stringof(); }
	MutableString64 mangleof() const override { return _node->mangleof(); }

	Node *getMember(String name) override { return _node->getMember(name); }

	// expr overrides
	TypeExpr* type() override { return dynamic_cast<Expr*>(_node)->type(); }
	Expr* constEval() override { return dynamic_cast<Expr*>(_node)->constEval(); };

	Expr* resolveExpr() override { return dynamic_cast<Expr*>(_node); }
	TypeExpr* resolveType() override { return dynamic_cast<TypeExpr*>(_node); }

	// type overrides
	Expr* init() const override { return dynamic_cast<TypeExpr*>(_node)->init(); }

	size_t size() const override { return dynamic_cast<TypeExpr*>(_node)->size(); }
	size_t alignment() const override { return dynamic_cast<TypeExpr*>(_node)->alignment(); }

	bool isSame(const TypeExpr *other) const override { return dynamic_cast<TypeExpr*>(_node)->isSame(other); }
	ConvType convertible(const TypeExpr *target) const override { return dynamic_cast<TypeExpr*>(_node)->convertible(target); }
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override { return dynamic_cast<TypeExpr*>(_node)->makeConversion(expr, targetType, implicit); }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class Identifier : public AmbiguousExpr
{
	friend class Semantic;

	SharedString _name;

	ValDecl *_var = nullptr;
	TypeDecl *_type = nullptr;
	ModuleDecl *_module = nullptr;

public:
	Identifier(SharedString name, SourceLocation loc)
		: Node(loc), AmbiguousExpr(loc), _name(std::move(name)) {}

	const SharedString &getName() const { return _name; }

	Declaration* target() const { return _var ? (Declaration*)_var : (Declaration*)_type; }

	bool isExpr() const override { return _var != nullptr; }
	bool isType() const override { return _type != nullptr; }

	// expr overrides
	TypeExpr* type() override { return _var->type(); }
	Expr* constEval() override { return resolveExpr()->constEval(); };

	Expr* resolveExpr() override { return _var ? _var->value() : nullptr; }
	TypeExpr* resolveType() override { return _type ? _type->type() : nullptr; }

	// type overrides
	MutableString64 stringof() const override
	{
		if (isType())
			return _type->stringof();
		else if (_module)
			return _module->stringof();
		return _name;
	}
	MutableString64 mangleof() const override
	{
		if (isType())
			return _type->mangleof();
		assert(false); // TODO: what is the scope of the symbol?
		if (_module)
			return _module->mangleof();
		return _var->mangleof();
	}

	Expr* init() const override { return _type->type()->init(); }

	size_t size() const override { return _type->type()->size(); }
	size_t alignment() const override { return _type->type()->alignment(); }

	bool isSame(const TypeExpr *other) const override { return _type->type()->isSame(other); }
	ConvType convertible(const TypeExpr *target) const override { return _type->type()->convertible(target); }
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override { return _type->type()->makeConversion(expr, targetType, implicit); }

	Node *getMember(String name) override;

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class MemberLookup : public AmbiguousExpr
{
	friend class Semantic;

	SharedString _member;

	Node *_node;
	Node *_result;

public:
	MemberLookup(Node *node, SharedString member, SourceLocation loc)
		: Node(loc), AmbiguousExpr(loc), _member(std::move(member)), _node(node) {}

	Node* expr() const { return _result; }

	bool isType() const override;
	bool isExpr() const override;

	// expr overrides
	TypeExpr* type() override;
	Expr* constEval() override { return resolveExpr()->constEval(); };

	Expr* resolveExpr() override;
	TypeExpr* resolveType() override { return ((const MemberLookup*)this)->resolveType(); }
	TypeExpr* resolveType() const;

	// type overrides
	MutableString64 stringof() const override
	{
		if (isType())
			return _result->stringof();
		return MutableString64(Concat, _node->stringof(), '.', _member);
	}
	MutableString64 mangleof() const override
	{
		if (isType())
			return _result->mangleof();
		assert(false);
		return nullptr;
	}

	Expr* init() const override { return resolveType()->init(); }

	size_t size() const override { return resolveType()->size(); }
	size_t alignment() const override { return resolveType()->alignment(); }

	bool isSame(const TypeExpr *other) const override { return resolveType()->isSame(other); }
	ConvType convertible(const TypeExpr *target) const override { return resolveType()->convertible(target); }
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override { return resolveType()->makeConversion(expr, targetType, implicit); }

	Node *getMember(String name) override;

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class Tuple : public AmbiguousExpr
{
	friend class Semantic;

	NodeList _elements = NodeList::empty();
	Array<size_t> _offsets;

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

	static Tuple* makeStringLiteralQuoted(String str, SourceLocation loc);
	static Tuple* makeStringLiteral(String str, PrimType type, bool unescape, SourceLocation loc);

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

	bool isString() const { return isExpr() && _type->isSequence() && _type->_element->asType()->isSomeChar(); }

	// expr overrides
	TypeExpr* type() override;
	Expr* constEval() override { assert(false); return nullptr; };

	Expr* resolveExpr() override { return this; }
	TypeExpr* resolveType() override { return this; }

	// type overrides
	MutableString64 stringof() const override;
	MutableString64 mangleof() const override;

	Expr* init() const override;

	size_t size() const override { return _size; }
	size_t alignment() const override { return _alignment; }

	bool isSame(const TypeExpr *other) const override;
	ConvType convertible(const TypeExpr *target) const override;
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override;

	Node *getMember(String name) override; // length, etc

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
	MutableString64 stringof() const override
	{
		if (isType())
			return _result->stringof();
		MutableString64 t = MutableString64(Concat, _node->stringof(), '[');
		bool first = true;
		for (auto &i : _indices)
		{
			if (first)
				first = false;
			else
				t.append(", ");
			t.append(i->stringof());
		}
		return t.append(']');
	}
	MutableString64 mangleof() const override
	{
		if (isType())
			return _result->mangleof();
		assert(false);
		return nullptr;
	}

	Expr* init() const override { return dynamic_cast<TypeExpr*>(_result)->init(); }

	size_t size() const override { return dynamic_cast<TypeExpr*>(_result)->size(); }
	size_t alignment() const override { return dynamic_cast<TypeExpr*>(_result)->alignment(); }

	bool isSame(const TypeExpr *other) const override { return dynamic_cast<TypeExpr*>(_result)->isSame(other); }
	ConvType convertible(const TypeExpr *target) const override { return dynamic_cast<TypeExpr*>(_result)->convertible(target); }
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override { return dynamic_cast<TypeExpr*>(_result)->makeConversion(expr, targetType, implicit); }

	Node *getMember(String name) override;

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
		: Declaration(std::move(Name), Loc), Args(std::move(Args)), IsOperator(IsOperator), Precedence(Prec), Line(Loc.line)
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

	MutableString64 stringof() const override { assert(false); return std::string(); }
	MutableString64 mangleof() const override { assert(false); return std::string(); }

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

Node* makePragma(String identifier, NodeList args);

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
	if (e)
		return e->isExpr() ? e->resolveExpr() : nullptr;
	return dynamic_cast<Expr*>(this);
}
inline TypeExpr* Node::asType()
{
	AmbiguousExpr *e = dynamic_cast<AmbiguousExpr*>(this);
	if (e)
		return e->isType() ? e->resolveType() : nullptr;
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
	FunctionType *f = asFunction();
	if (f)
		return f->returnType()->evalType();
	return this;
}
inline TypeExpr* TypeExpr::evalType()
{
	FunctionType *f = asFunction();
	if (f)
		return f->returnType()->evalType();
	PointerType *p = asPointer();
	if (p)
		return p->targetType()->evalType();
	return this;
}

inline PrimitiveType* TypeExpr::asPrimitive() { return dynamic_cast<PrimitiveType*>(this); }
inline const PrimitiveType* TypeExpr::asPrimitive() const { return dynamic_cast<const PrimitiveType*>(this); }
inline ModifiedType* TypeExpr::asModified() { return dynamic_cast<ModifiedType*>(this); }
inline const ModifiedType* TypeExpr::asModified() const { return dynamic_cast<const ModifiedType*>(this); }
inline FunctionType* TypeExpr::asFunction() { return dynamic_cast<FunctionType*>(this); }
inline const FunctionType* TypeExpr::asFunction() const { return dynamic_cast<const FunctionType*>(this); }
inline PointerType* TypeExpr::asPointer() { return dynamic_cast<PointerType*>(this); }
inline const PointerType* TypeExpr::asPointer() const { return dynamic_cast<const PointerType*>(this); }
inline Struct* TypeExpr::asStruct() { return dynamic_cast<Struct*>(this); }
inline const Struct* TypeExpr::asStruct() const { return dynamic_cast<const Struct*>(this); }
inline Tuple* TypeExpr::asTuple() { return dynamic_cast<Tuple*>(this); }
inline const Tuple* TypeExpr::asTuple() const { return dynamic_cast<const Tuple*>(this); }
inline bool TypeExpr::isVoid() const { const PrimitiveType *pt = dynamic_cast<const PrimitiveType*>(this); return pt && pt->type() == PrimType::v; }
inline bool TypeExpr::isBoolean() const { const PrimitiveType *pt = dynamic_cast<const PrimitiveType*>(this); return pt && isBool(pt->type()); }
inline bool TypeExpr::isIntegral() const { const PrimitiveType *pt = dynamic_cast<const PrimitiveType*>(this); return pt && isInt(pt->type()); }
inline bool TypeExpr::isFloatingPoint() const { const PrimitiveType *pt = dynamic_cast<const PrimitiveType*>(this); return pt && isFloat(pt->type()); }
inline bool TypeExpr::isSomeChar() const { const PrimitiveType *pt = dynamic_cast<const PrimitiveType*>(this); return pt && isChar(pt->type()); }
inline bool TypeExpr::isConst() const { const ModifiedType *mod = dynamic_cast<const ModifiedType*>(this); return mod && mod->mod() == TypeMod::Const; }

}
