#include "ast.h"
#include "astvisitor.h"

void Node::accept(ASTVisitor &v) { v.visit(*this); }
void Statement::accept(ASTVisitor &v) { v.visit(*this); }
void Declaration::accept(ASTVisitor &v) { v.visit(*this); }
void ModuleStatement::accept(ASTVisitor &v) { v.visit(*this); }
void ReturnStatement::accept(ASTVisitor &v) { v.visit(*this); }
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
void IdentifierExpr::accept(ASTVisitor &v) { v.visit(*this); }
void UnaryExpr::accept(ASTVisitor &v) { v.visit(*this); }
void BinaryExpr::accept(ASTVisitor &v) { v.visit(*this); }
void IndexExpr::accept(ASTVisitor &v) { v.visit(*this); }
void CallExpr::accept(ASTVisitor &v) { v.visit(*this); }
void IfExprAST::accept(ASTVisitor &v) { v.visit(*this); }
void ForExprAST::accept(ASTVisitor &v) { v.visit(*this); }
void TypeDecl::accept(ASTVisitor &v) { v.visit(*this); }
void ValDecl::accept(ASTVisitor &v) { v.visit(*this); }
void VarDecl::accept(ASTVisitor &v) { v.visit(*this); }
void PrototypeDecl::accept(ASTVisitor &v) { v.visit(*this); }
void FunctionDecl::accept(ASTVisitor &v) { v.visit(*this); }


Declaration *Scope::getDecl(const std::string& name, bool onlyLocal) const
{
	auto i = _declarations.find(name);
	if (i == _declarations.end())
	{
		if (!onlyLocal)
			return _parent->getDecl(name, false);
		return nullptr;
	}
	return i->second;
}
void Scope::addDecl(const std::string& name, Declaration *decl)
{
	_declarations.insert({ name, decl });
}

raw_ostream &ReturnStatement::dump(raw_ostream &out, int ind)
{
	Statement::dump(out << "return: ", ind);
	return _expression->dump(out, ind + 1);
}


//****************
//** Type nodes **
//****************

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


//***********************
//** Declaration nodes **
//***********************

raw_ostream &TypeDecl::dump(raw_ostream &out, int ind)
{
	Declaration::dump(out << "type: " << _name, ind) << "\n";
	++ind;
	if (_type)
		_type->dump(indent(out, ind) << "as: ", ind);
	return out;
}

raw_ostream &ValDecl::dump(raw_ostream &out, int ind)
{
	Declaration::dump(out << "def: #" << _name, ind) << "\n";
	++ind;
	if (_type)
		_type->dump(indent(out, ind) << "type: ", ind);
	if (_init)
		_init->dump(indent(out, ind) << "val: ", ind);
	return out;
}

raw_ostream &VarDecl::dump(raw_ostream &out, int ind)
{
	Declaration::dump(out << "var: #" << _name, ind) << "\n";
	++ind;
	if (_type)
		_type->dump(indent(out, ind) << "type: ", ind);
	if (_init)
		_init->dump(indent(out, ind) << "init: ", ind);
	return out;
}


//**********************
//** Expression nodes **
//**********************

TypeExpr* PrimitiveLiteralExpr::type()
{
	if (!_typeExpr)
		_typeExpr = new PrimitiveType(_type);
	return _typeExpr;
}


static const char *unaryOps[] =
{
	"+", "-", "~", "!", "++", "--"
};
raw_ostream &UnaryExpr::dump(raw_ostream &out, int ind)
{
	Expr::dump(out << "unary" << unaryOps[(int)_op] << "\n", ind);
	_operand->dump(indent(out, ind + 1) << "operand: ", ind + 1);
	return out;
}

static const char *binOps[] =
{
	"+", "-", "*", "/", "%", "^^", "~",
	"<<", ">>", ">>>",
	"&", "|", "^", "&&", "||", "^^",
	"==", "!=", ">", ">=", "<", "<="
};
raw_ostream &BinaryExpr::dump(raw_ostream &out, int ind)
{
	Expr::dump(out << "binary" << binOps[(int)_op] << "\n", ind);
	_lhs->dump(indent(out, ind + 1) << "lhs: ", ind + 1);
	_rhs->dump(indent(out, ind + 1) << "rhs: ", ind + 1);
	return out;
}




/////////////////////////////////////////////

const char * types[] =
{
	"String",
	"List",
	"TempalteId",
	"Type",
	"Instantiate",
	"Const",
	"Pointer",
	"Ref",
	"TypedId",
	"Struct",
	"Tuple",
	"Array",
	"ArrayLiteral",
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
	case Type::String:
		return out << "generic(String): \"" << s << "\"\n";
	case Type::Module:
		return l->dump(out << "generic(Module): ", ind);
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
Generic* TypeId(const char* i)
{
	Generic *pN = new Generic(Generic::Type::TypedId);
	pN->s = i;
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
