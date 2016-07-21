#include "ast.h"
#include "astvisitor.h"

void Node::accept(ASTVisitor &v) { v.visit(*this); }
void Statement::accept(ASTVisitor &v) { v.visit(*this); }
void Declaration::accept(ASTVisitor &v) { v.visit(*this); }
void ModuleStatement::accept(ASTVisitor &v) { v.visit(*this); }
void Scope::accept(ASTVisitor &v) { v.visit(*this); }
void Module::accept(ASTVisitor &v) { v.visit(*this); }
void TypeExpr::accept(ASTVisitor &v) { v.visit(*this); }
void PrimitiveType::accept(ASTVisitor &v) { v.visit(*this); }
void TypeIdentifier::accept(ASTVisitor &v) { v.visit(*this); }
void TupleType::accept(ASTVisitor &v) { v.visit(*this); }
void Struct::accept(ASTVisitor &v) { v.visit(*this); }
void FunctionType::accept(ASTVisitor &v) { v.visit(*this); }
void Expr::accept(ASTVisitor &v) { v.visit(*this); }
void Generic::accept(ASTVisitor &v) { v.visit(*this); }
void PrimitiveLiteralExpr::accept(ASTVisitor &v) { v.visit(*this); }
void ArrayLiteralExpr::accept(ASTVisitor &v) { v.visit(*this); }
void FunctionLiteralExpr::accept(ASTVisitor &v) { v.visit(*this); }
void VariableExprAST::accept(ASTVisitor &v) { v.visit(*this); }
void UnaryExprAST::accept(ASTVisitor &v) { v.visit(*this); }
void BinaryExprAST::accept(ASTVisitor &v) { v.visit(*this); }
void IndexExprAST::accept(ASTVisitor &v) { v.visit(*this); }
void CallExprAST::accept(ASTVisitor &v) { v.visit(*this); }
void IfExprAST::accept(ASTVisitor &v) { v.visit(*this); }
void ForExprAST::accept(ASTVisitor &v) { v.visit(*this); }
void TypeDecl::accept(ASTVisitor &v) { v.visit(*this); }
void ValDecl::accept(ASTVisitor &v) { v.visit(*this); }
void VarDecl::accept(ASTVisitor &v) { v.visit(*this); }
void PrototypeDecl::accept(ASTVisitor &v) { v.visit(*this); }
void FunctionDecl::accept(ASTVisitor &v) { v.visit(*this); }


const char * types[] =
{
	"Int",
	"UInt",
	"Double",
	"String",
	"List",
	"Id",
	"TempalteId",
	"Type",
	"FunctionType",
	"Instantiate",
	"Const",
	"Pointer",
	"Ref",
	"TypedId",
	"Struct",
	"Tuple",
	"Array",
	"ArrayLiteral",
	"FunctionLiteral",
	"Module",
	"DefType",
	"DefConst",
	"Var",
	"Elipsis",
	"MemberLookup",
	"Call",
	"OpIndex",
	"OpPostInc",
	"OpPostDec",
	"OpPreInc",
	"OpPreDec",
	"OpUnaryPlus",
	"OpUnaryMinus",
	"OpUnaryNot",
	"OpUnaryComp",
	"OpMul",
	"OpDiv",
	"OpMod",
	"OpAdd",
	"OpSub",
	"OpConcat",
	"OpASL",
	"OpASR",
	"OpLSR",
	"OpLt",
	"OpGt",
	"OpLe",
	"OpGe",
	"OpEq",
	"OpNe",
	"OpBitAnd",
	"OpBitXor",
	"OpBitOr",
	"OpAnd",
	"OpXor",
	"OpOr",
	"OpAssign",
	"OpBind",
	"OpMulEq",
	"OpDivEq",
	"OpModEq",
	"OpAddEq",
	"OpSubEq",
	"OpConcatEq",
	"OpBitAndEq",
	"OpBitXorEq",
	"OpBitOrEq",
	"OpAndEq",
	"OpXorEq",
	"OpOrEq",
	"OpASLEq",
	"OpASREq",
	"OpLSREq",
	"Return",
	"Break"
};

raw_ostream &Generic::dumpList(raw_ostream &out, int ind)
{
	if (l)
	{
		Generic *gl = dynamic_cast<Generic*>(l);
		if (gl && gl->type == Type::List)
			gl->dumpList(out, ind);
		else
			l->dump(indent(out, ind), ind);
	}
	if (r)
	{
		Generic *gr = dynamic_cast<Generic*>(r);
		if (gr && gr->type == Type::List)
			gr->dumpList(out, ind);
		else
			r->dump(indent(out, ind), ind);
	}
	return out;
}

raw_ostream &Generic::dump(raw_ostream &out, int ind)
{
	switch (type)
	{
		case Type::List:
		{
			out << "generic(List): [\n";
			dumpList(out, ind + 1);
			return indent(out, ind) << "]\n";
		}
		case Type::Int:
			return out << i << "\n";
		case Type::UInt:
			return out << u << "\n";
		case Type::Double:
			return out << f << "\n";
		case Type::String:
			return out << "generic(String): \"" << s << "\"\n";
		case Type::Module:
			return l->dump(out << "generic(Module): ", ind);
		case Type::Id:
			return out << '#' << s << "\n";
		default:
			Node::dump(out << "generic(" << types[(size_t)type] << ")", ind) << ":\n";
			++ind;
			if (l)
				l->dump(indent(out, ind) << "l: ", ind);
			if (r)
				r->dump(indent(out, ind) << "r: ", ind);
	}
	return out;
}


static const char *primTypes[] =
{
	"v", "u1", "i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64", "iz", "uz", "f32", "f64", "c8", "c16", "c32"
};

std::string PrimitiveType::stringof() const
{
	return std::string("@") + primTypes[(size_t)type()];
}

raw_ostream &PrimitiveType::dump(raw_ostream &out, int ind)
{
	return out << stringof() << '\n';
}

Expr* PrimitiveType::init()
{
	if (!_init)
	{
		switch (_type)
		{
		case PrimType::u1:
		case PrimType::u8:
		case PrimType::u16:
		case PrimType::u32:
		case PrimType::u64:
		case PrimType::i8:
		case PrimType::i16:
		case PrimType::i32:
		case PrimType::i64:
			_init = new PrimitiveLiteralExpr(_type, (uint64_t)0); break;
		case PrimType::c8:
		case PrimType::c16:
		case PrimType::c32:
			_init = new PrimitiveLiteralExpr(_type, (char32_t)0); break;
		case PrimType::f32:
		case PrimType::f64:
			_init = new PrimitiveLiteralExpr(_type, 0.0); break;
		case PrimType::v:
		default:
			assert(0);
		}
	}
	return _init;
}

TypeExpr* PrimitiveLiteralExpr::type()
{
	if (!_typeExpr)
		_typeExpr = new PrimitiveType(_type);
	return _typeExpr;
}


Declaration *Scope::getDecl(const std::string& name, bool onlyLocal) const
{
	auto i = _symbols.find(name);
	if (i == _symbols.end())
	{
		if (!onlyLocal)
			return _parent->getDecl(name, false);
		return nullptr;
	}
	return i->second;
}
void Scope::addDecl(const std::string& name, Declaration *decl)
{
	_symbols.insert({ name, decl });
}



Node* stack[2048];
size_t depth = 0;


Node* Push(Node *n)
{
	stack[depth] = n;
	return stack[depth++];
}
Node* Pop()
{
	assert(depth > 0);
	return stack[--depth];
}
Node* Top()
{
	assert(depth > 0);
	return stack[depth - 1];
}

Generic* String(const char* s)
{
	Generic *pN = new Generic(Generic::Type::String);
	pN->s = s;
	return pN;
}
Generic* Identifier(const char* i)
{
	Generic *pN = new Generic(Generic::Type::Id);
	pN->s = i;
	return pN;
}
Generic* TypeId(const char* i)
{
	Generic *pN = new Generic(Generic::Type::TypedId);
	pN->s = i;
	return pN;
}
Generic* Int(__int64 i)
{
	Generic *pN = new Generic(Generic::Type::TypedId);
	pN->i = i;
	return pN;
}
Generic* UInt(unsigned __int64 i)
{
	Generic *pN = new Generic(Generic::Type::TypedId);
	pN->u = i;
	return pN;
}
Generic* Float(double f)
{
	Generic *pN = new Generic(Generic::Type::TypedId);
	pN->f = f;
	return pN;
}

Generic* Add(Generic::Type _type, Node *_l, Node *_r)
{
	return new Generic(_type, _l, _r);
}
Generic* SetChildren(Generic *node, Node *_l, Node *_r)
{
	if (_l)
		node->l = _l;
	if (_r)
		node->r = _r;
	return node;
}

void Print(Node *n)
{
//	n->dump()
}
