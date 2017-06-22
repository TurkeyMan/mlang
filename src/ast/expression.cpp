#include "ast.h"
#include "astvisitor.h"
#include "error.h"

namespace m {

void PrimitiveLiteralExpr::accept(ASTVisitor &v) { v.visit(*this); }
void AggregateLiteralExpr::accept(ASTVisitor &v) { v.visit(*this); }
void FunctionLiteralExpr::accept(ASTVisitor &v) { v.visit(*this); }
void RefExpr::accept(ASTVisitor &v) { v.visit(*this); }
void DerefExpr::accept(ASTVisitor &v) { v.visit(*this); }
void TypeConvertExpr::accept(ASTVisitor &v) { v.visit(*this); }
void UnaryExpr::accept(ASTVisitor &v) { v.visit(*this); }
void BinaryExpr::accept(ASTVisitor &v) { v.visit(*this); }
void CallExpr::accept(ASTVisitor &v) { v.visit(*this); }
void AssignExpr::accept(ASTVisitor &v) { v.visit(*this); }
void BindExpr::accept(ASTVisitor &v) { v.visit(*this); }
void SliceExpr::accept(ASTVisitor &v) { v.visit(*this); }


//**********************
//** Expression nodes **
//**********************

MutableString64 PrimitiveLiteralExpr::stringof() const
{
	switch (_type)
	{
	case PrimType::v:
		return "void";
	case PrimType::u1:
		return u ? "true" : "false";
	case PrimType::f16:
	case PrimType::f32:
	case PrimType::f64:
	case PrimType::f128:
		return std::to_string(f);
	case PrimType::u8:
	case PrimType::u16:
	case PrimType::u32:
	case PrimType::u64:
	case PrimType::u128:
		return std::to_string(u);
	case PrimType::i8:
	case PrimType::i16:
	case PrimType::i32:
	case PrimType::i64:
	case PrimType::i128:
		return std::to_string(i);
	case PrimType::c8:
	case PrimType::c16:
	case PrimType::c32:
	{
		MutableString64 s;
		if (c < ' ' || c == '\'' || c == '\\')
		{
			switch (c)
			{
			case '\'': s = "'\\\''"; break;
			case '\\': s = "'\\\\'"; break;
			case '\0': s = "'\\0'"; break;
			case '\a': s = "'\\a'"; break;
			case '\b': s = "'\\b'"; break;
			case '\f': s = "'\\f'"; break;
			case '\n': s = "'\\n'"; break;
			case '\r': s = "'\\r'"; break;
			case '\t': s = "'\\t'"; break;
			case '\v': s = "'\\v'"; break;
			default:
			{
				static const char hex[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' };
				char h[2];
				h[0] = hex[c >> 4];
				h[1] = hex[c & 0xF];
				s.append("'\\x", Slice<char>(h, 2), '\'');
			}
			}
		}
		else
			s.append('\'', c, '\'');

		if (_type == PrimType::c16)
			s.append('w');
		else if (_type == PrimType::c32)
			s.append('d');

		return s;
	}
	default:
		assert(0);
		return nullptr;
	}
}

raw_ostream &PrimitiveLiteralExpr::dump(raw_ostream &out, int ind)
{
	return Expr::dump(out << str_ref(stringof()), ind) << '\n';
}

raw_ostream &AggregateLiteralExpr::dump(raw_ostream &out, int ind)
{
	Expr::dump(out << "aggregate\n", ind);
	indent(out, ind) << "type: " << str_ref(_type->stringof()) << '\n';
	indent(out, ind) << "values: {\n";
	for (auto i : _items)
		i->dump(indent(out, ind + 1), ind + 1);
	indent(out, ind) << "}\n";
	return out;
}

Node *RefExpr::getMember(String name)
{
	// base class member?
	Node *r = Expr::getMember(name);
	if (r)
		return r;

	// type member?
	r = type()->getMember(name);
	if (r)
		return r;

	TypeExpr *target = targetType()->resolveType();

	// KINDA HAX: if the target is a struct, look-up member
	Struct *s = dynamic_cast<Struct*>(target);
	if (s)
	{
		Declaration *decl = s->getDecl(name, true);
		if (dynamic_cast<VarDecl*>(decl))
			return new RefExpr((VarDecl*)decl, this, getLoc());
	}
	// KINDA HAX: if target is an array, we can get some things...
	Tuple *arr = dynamic_cast<Tuple*>(target);
	if (arr && arr->isSequence())
	{
		if (name.eq("ptr"))
			return makeConversion(new PointerType(PtrType::RawPtr, arr->seqElement()->asType(), SourceLocation(0)), false);
	}

	r = target->getMember(name);
	if (r)
		return r;

	DerefExpr *deref = new DerefExpr(this, getLoc());
	r = deref->getMember(name);
	if (r)
		return r;

	deref->~DerefExpr();
	return nullptr;
}

raw_ostream &RefExpr::dump(raw_ostream &out, int ind)
{
	Expr::dump(out << "ref\n", ind);
	_type->dump(indent(out, ind) << "type: ", ind);
	if (_target)
		indent(out, ind) << "target: #" << str_ref(_target->name()) << '\n';
	else
		indent(out, ind) << "target: 0x" << _absolute << '\n';
	return out;
}

raw_ostream &DerefExpr::dump(raw_ostream &out, int ind)
{
	Expr::dump(out << "deref\n", ind);
	_expr->dump(indent(out, ind) << "expr: ", ind + 1);
	return out;
}

raw_ostream &TypeConvertExpr::dump(raw_ostream &out, int ind)
{
	Expr::dump(out << "cast\n", ind);
	_newType->dump(indent(out, ind) << "target: ", ind + 1);
	_expr->dump(indent(out, ind) << "expr: ", ind + 1);
	return out;
}

static const char *unaryOps[] =
{
	"+", "-", "~", "!", "++", "--"
};
raw_ostream &UnaryExpr::dump(raw_ostream &out, int ind)
{
	Expr::dump(out << "unary " << unaryOps[(int)_op] << "\n", ind);
	_operand->dump(indent(out, ind) << "operand: ", ind + 1);
	return out;
}

static const char *binOps[(int)BinOp::__NumOps] =
{
	"",
	"+", "-", "*", "/", "%", "^^", "~",
	"<<", ">>", ">>>",
	"&", "|", "^", "&&", "||", "^^",
	"==", "!=", ">", ">=", "<", "<="
};
raw_ostream &BinaryExpr::dump(raw_ostream &out, int ind)
{
	Expr::dump(out << "binary " << binOps[(int)_op] << "\n", ind);
	_lhs->dump(indent(out, ind) << "lhs: ", ind + 1);
	_rhs->dump(indent(out, ind) << "rhs: ", ind + 1);
	return out;
}

Node *CallExpr::getMember(String name)
{
	// base class member?
	Node *r = Expr::getMember(name);
	if (r)
		return r;
	return nullptr;
}

raw_ostream &CallExpr::dump(raw_ostream &out, int ind)
{
	Expr::dump(out << "call\n", ind);
	ind++;
	_func->dump(indent(out, ind) << "function: ", ind + 1);
	indent(out, ind) << "args: (\n";
	for (auto a : _callArgs)
		a->dump(indent(out, ind + 1), ind + 1);
	indent(out, ind) << ")\n";
	return out;
}

raw_ostream &AssignExpr::dump(raw_ostream &out, int ind)
{
	Expr::dump(out << "assign " << binOps[(int)_op] << "=\n", ind);
	ind++;
	_target->dump(indent(out, ind) << "target: ", ind + 1);
	_expr->dump(indent(out, ind) << "expr: ", ind + 1);
	return out;
}

raw_ostream &BindExpr::dump(raw_ostream &out, int ind)
{
	Expr::dump(out << "bind\n", ind);
	ind++;
	_target->dump(indent(out, ind) << "target: ", ind + 1);
	_expr->dump(indent(out, ind) << "expr: ", ind + 1);
	return out;
}

TypeExpr* SliceExpr::type()
{
	if (!_tup)
		_tup = new Tuple({ _from, _to }, getLoc());
	return _tup->resolveExpr()->type();
}

raw_ostream &SliceExpr::dump(raw_ostream &out, int ind)
{
	Expr::dump(out << "..\n", ind);
	ind++;
	_from->dump(indent(out, ind) << "from: ", ind + 1);
	_to->dump(indent(out, ind) << "to: ", ind + 1);
	return out;
}

}
