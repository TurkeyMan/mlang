#pragma once

#include "gc_cpp.h"
#include "gc_allocator.h"

#include "sourceloc.h"
#include "util.h"
#include "error.h"

#include <map>
#include <set>

#include "codegen/llvm/common_llvm.h"
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

#include "ast/statement.h"
#include "ast/type.h"
#include "ast/declaration.h"
#include "ast/expression.h"
#include "ast/indirect.h"

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
