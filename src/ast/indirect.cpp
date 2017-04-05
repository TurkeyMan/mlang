#include "ast.h"
#include "astvisitor.h"
#include "error.h"

namespace m {

void UnknownExpr::accept(ASTVisitor &v) { v.visit(*this); }
void Identifier::accept(ASTVisitor &v) { v.visit(*this); }
void MemberLookup::accept(ASTVisitor &v) { v.visit(*this); }
void Tuple::accept(ASTVisitor &v) { v.visit(*this); }
void Index::accept(ASTVisitor &v) { v.visit(*this); }


//********************
//** Indirect nodes **
//********************

raw_ostream &UnknownExpr::dump(raw_ostream &out, int ind)
{
	return _node->dump(out, ind);
}

Node *Identifier::getMember(String name)
{
	if (_module)
		return _module->getMember(name);

	// base class member?
	Node *r = AmbiguousExpr::getMember(name);
	if (r)
		return r;

	return resolve()->getMember(name);
}

raw_ostream &Identifier::dump(raw_ostream &out, int ind)
{
	const char *ty = _var ? "#" : (_type ? "@" : "");
	return Node::dump(out << ty << str_ref(_name) << "\n", ind);
}

bool MemberLookup::isType() const
{
	return dynamic_cast<TypeExpr*>(_result) != nullptr || dynamic_cast<TypeDecl*>(_result) != nullptr;
}
bool MemberLookup::isExpr() const
{
	return dynamic_cast<Expr*>(_result) != nullptr || dynamic_cast<ValDecl*>(_result);
}

TypeExpr* MemberLookup::type()
{
	ValDecl *val = dynamic_cast<ValDecl*>(_result);
	if (val)
		return val->type();
	Expr *expr = dynamic_cast<Expr*>(_result);
	if (expr)
		return expr->type();
	return nullptr;
}

Expr* MemberLookup::resolveExpr()
{
	Expr *expr = dynamic_cast<Expr*>(_result);
	if (expr)
		return expr;
	ValDecl *val = dynamic_cast<ValDecl*>(_result);
	if (val)
		return val->value();
	return nullptr;
}
TypeExpr* MemberLookup::resolveType() const
{
	TypeExpr *type = dynamic_cast<TypeExpr*>(_result);
	if (type)
		return type;
	TypeDecl *typedecl = dynamic_cast<TypeDecl*>(_result);
	if (typedecl)
		return typedecl->type();
	return nullptr;
}

Node *MemberLookup::getMember(String name)
{
	ModuleDecl *mod = dynamic_cast<ModuleDecl*>(_result);
	if (mod)
		return mod->getMember(name);

	// base class member?
	Node *r = AmbiguousExpr::getMember(name);
	if (r)
		return r;

	return resolve()->getMember(name);
}

raw_ostream &MemberLookup::dump(raw_ostream &out, int ind)
{
	Node::dump(out << "lookup\n", ind);
	indent(out, ind) << "member: " << str_ref(_member) << "\n";
	_node->dump(indent(out, ind) << "node: ", ind + 1);
	return out;
}

Tuple* Tuple::makeStringLiteralQuoted(String str, SourceLocation loc)
{
	assert(str[0] == '"' || str[0] == '`');

	bool escaped = str[0] == '"';

	PrimType type = PrimType::c8;
	if (str[str.length - 1] == 'w')
	{
		type = PrimType::c16;
		str.pop_back();
	}
	else if (str[str.length - 1] == 'd')
	{
		type = PrimType::c32;
		str.pop_back();
	}

	assert(str[str.length - 1] == (escaped ? '"' : '`'));

	return makeStringLiteral(str.slice(1, str.length - 1), type, escaped, loc);
}

Tuple* Tuple::makeStringLiteral(String str, PrimType type, bool unescape, SourceLocation loc)
{
	NodeList s = NodeList::empty();
	Array<size_t> offsets;

	offsets.reserve(str.length);
	size_t offset = 0;

	while (!str.empty())
	{
		char32_t c = str.pop_front_char();

		if (unescape && c == '\\')
		{
			c = str.pop_front_char();

			switch (c)
			{
			case '0':	c = '\0';	break;
			case 'a':	c = '\a';	break;
			case 'b':	c = '\b';	break;
			case 'f':	c = '\f';	break;
			case 'n':	c = '\n';	break;
			case 'r':	c = '\r';	break;
			case 't':	c = '\t';	break;
			case 'v':	c = '\v';	break;
			case 'x':
			{
				if (!detail::is_hex(str[0]) || !detail::is_hex(str[1]))
					error("file", loc.line, "Invalid hexadecimal character.");
				c = (char32_t)str.pop_front(2).parse_int<16>();
				break;
			}
			case 'u':
			{
				if (!detail::is_hex(str[0]) || !detail::is_hex(str[1]) || !detail::is_hex(str[2]) || !detail::is_hex(str[3]))
					error("file", loc.line, "Invalid hexadecimal character.");
				c = (char32_t)str.pop_front(4).parse_int<16>();
				break;
			}
			case 'U':
			{
				if (!detail::is_hex(str[0]) || !detail::is_hex(str[1]) || !detail::is_hex(str[2]) || !detail::is_hex(str[3]) ||
					!detail::is_hex(str[4]) || !detail::is_hex(str[5]) || !detail::is_hex(str[6]) || !detail::is_hex(str[7]))
					error("file", loc.line, "Invalid hexadecimal character.");
				c = (char32_t)str.pop_front(8).parse_int<16>();
				break;
			}
			default:
				break;
			}
		}

		if (type == PrimType::c8)
		{
			char buffer[6];
			size_t l = detail::utf_encode(c, buffer);
			for (size_t j = 0; j < l; ++j)
			{
				offsets.push_back(offset);
				offset += 1;
				s = s.append(new PrimitiveLiteralExpr(type, (char32_t)buffer[j], loc));
			}
		}
		else if (type == PrimType::c16)
		{
			char16_t buffer[3];
			size_t l = detail::utf_encode(c, buffer);
			for (size_t j = 0; j < l; ++j)
			{
				offsets.push_back(offset);
				offset += 2;
				s = s.append(new PrimitiveLiteralExpr(type, (char32_t)buffer[j], loc));
			}
		}
		else if (type == PrimType::c32)
		{
			offsets.push_back(offset);
			offset += 4;
			s = s.append(new PrimitiveLiteralExpr(type, c, loc));
		}
		else
			assert(false);
	}

	PrimitiveType *ty = PrimitiveType::get(type, loc);

	Tuple *r = new Tuple(s, loc);

	r->_size = ty->size() * s.length;
	r->_alignment = ty->alignment();
	r->allExpr = true;
	r->_offsets = std::move(offsets);

	r->_type = new Tuple(ty, ExprList::empty().append(new PrimitiveLiteralExpr(SizeT_Type, s.length, loc)), loc);

	return r;
}

void Tuple::analyse()
{
#define alignto(x, a) (((x) + ((a)-1)) & ~((a)-1))

	if (_alignment != 0)
		return; // already done?

				// compose the struct
	_size = 0;
	_alignment = 0;

	if (!isSequence())
	{
		TypeExpr *ty1 = _elements[0]->asType();
		if (ty1)
		{
			bool same = true;
			if (_elements.length > 1)
			{
				for (size_t i = 1; i < _elements.length; ++i)
				{
					TypeExpr *ty2 = _elements[i]->asType();
					if (!ty2 || !ty1->isSame(ty2))
					{
						same = false;
						break;
					}
				}
			}
			if (same)
			{
				_element = _elements[0];
				_shape = ExprList::empty().append(new PrimitiveLiteralExpr(SizeT_Type, _elements.length, getLoc()));
				_elements = NodeList::empty();
			}
		}
	}

	if (isSequence())
	{
		allExpr = false;
		allTypes = false;
		Expr *expr = _element->asExpr();
		if (expr)
		{
			allExpr = true;
			_alignment = expr->type()->alignment();
			_size = expr->type()->size();
		}
		else
		{
			TypeExpr *type = _element->asType();
			if (type)
			{
				allTypes = true;
				_alignment = type->alignment();
				_size = type->size();
			}
		}

		size_t numElements = 1;
		for (auto e : _shape)
			numElements *= e->constEval()->getIntValue();
		_size *= numElements;
	}
	else
	{
		allExpr = true;
		allTypes = _elements.length > 0;
		for (auto e : _elements)
		{
			size_t size = 0;
			size_t align = 0;

			Expr *expr = e->asExpr();
			if (expr)
			{
				size = expr->type()->size();
				align = expr->type()->alignment();

				allTypes = false;
			}
			else
			{
				TypeExpr *type = e->asType();
				if (type)
				{
					size = type->size();
					align = type->alignment();

					allExpr = false;
				}
				else
				{
					allExpr = false;
					allTypes = false;

					// accept other nodes in tuples?
					assert(false);
				}
			}

			_size = alignto(_size, align);
			_offsets.push_back(_size);
			_size += size;
			_alignment = align > _alignment ? align : _alignment;
		}

		_size = alignto(_size, _alignment);
	}
}

ptrdiff_t Tuple::numElements(int dimension) const
{
	assert(dimension < dimensions());

	if (_element == nullptr)
		return _elements.length;

	if (dimension < 0)
	{
		ptrdiff_t len = 1;
		for (auto e : _shape)
		{
			if (!e->type()->isIntegral())
				error("file", getLine(), "Array index must be integral!");
			e = e->constEval();
			if (!e)
				return -1;
			return (ptrdiff_t)e->getIntValue();
		}
		return len;
	}

	if (!_shape[dimension]->type()->isIntegral())
		error("file", getLine(), "Array index must be integral!");
	Expr *e = _shape[dimension]->constEval();
	if (!e)
		return -1;
	return e->getIntValue();
}
TypeExpr* Tuple::type()
{
	if (!_type)
	{
		assert(allExpr);
		if (!allExpr)
			return nullptr; // error?

		if (isSequence())
		{
			Expr *expr = dynamic_cast<Expr*>(_element);
			Tuple *r = new Tuple(expr->type(), _shape, SourceLocation(-1));
			r->analyse();
			_type = r;
		}
		else
		{
			NodeList types = NodeList::empty();
			for (auto e : _elements)
			{
				Expr *expr = dynamic_cast<Expr*>(e);
				types = types.append(expr->type());
			}

			Tuple *r = new Tuple(types, SourceLocation(-1));
			r->analyse();
			_type = r;
		}
	}
	return _type;
}

Expr* Tuple::init() const
{
	if (!_init)
	{
		assert(allTypes);
		if (!allTypes)
			return nullptr; // error?

		if (isSequence())
		{
			TypeExpr *t = dynamic_cast<TypeExpr*>(_element);
			Tuple *r = new Tuple(t->init(), _shape, SourceLocation(-1));
			r->_type = (Tuple*)this;
			r->analyse();
			((Tuple*)this)->_init = r;
		}
		else
		{
			NodeList init = NodeList::empty();
			for (auto e : _elements)
			{
				TypeExpr *t = dynamic_cast<TypeExpr*>(e);
				init = init.append(t->init());
			}

			Tuple *r = new Tuple(init, SourceLocation(-1));
			r->_type = (Tuple*)this;
			r->analyse();
			((Tuple*)this)->_init = r;
		}
	}
	return _init;
}

bool Tuple::isSame(const TypeExpr *other) const
{
	const Tuple *t = dynamic_cast<const Tuple*>(other);
	if (t == nullptr)
		return false;

	size_t d = dimensions();
	if (d == t->dimensions())
	{
		for (size_t i = 0; i < dimensions(); ++i)
		{
			size_t count = numElements(i);
			if (count == -1 || count != t->numElements(i))
				return false;
		}
	}

	const TypeExpr *e1 = nullptr, *e2 = nullptr;
	if (_element)
	{
		e1 = _element->asType();
		if (!e1)
			return false;
	}
	if (t->_element)
	{
		e2 = t->_element->asType();
		if (!e2)
			return false;
	}

	if (e1 && e2)
		return e1->isSame(e2);

	// arbitrary tuple?
	size_t count = numElements(0);
	for (size_t i = 0; i < count; ++i)
	{
		const TypeExpr *t1 = e1 ? e1 : _elements[i]->asType();
		const TypeExpr *t2 = e2 ? e2 : t->_elements[i]->asType();
		if (!t1 || !t2)
			return false;
		if (!t1->isSame(t2))
			return false;
	}
	return true;
}

ConvType Tuple::convertible(const TypeExpr *target) const
{
	const Tuple *t = dynamic_cast<const Tuple*>(target);
	if (t == nullptr)
		return ConvType::NoConversion;

	// TODO: tuples can convert to structs?

	size_t d = dimensions();
	if (d == t->dimensions())
	{
		for (size_t i = 0; i < dimensions(); ++i)
		{
			size_t count = numElements(i);
			if (count == -1 || count != t->numElements(i))
				return ConvType::NoConversion;
		}
	}

	const TypeExpr *e1 = nullptr, *e2 = nullptr;
	if (_element)
	{
		e1 = _element->asType();
		if (!e1)
			return ConvType::NoConversion;
	}
	if (t->_element)
	{
		e2 = t->_element->asType();
		if (!e2)
			return ConvType::NoConversion;
	}

	if (e1 && e2)
		return e1->convertible(e2);

	// arbitrary tuple?
	ConvType conv = ConvType::Convertible;
	size_t count = numElements(0);
	for (size_t i = 0; i < count; ++i)
	{
		const TypeExpr *t1 = e1 ? e1 : _elements[i]->asType();
		const TypeExpr *t2 = e2 ? e2 : t->_elements[i]->asType();
		if (!t1 || !t2)
			return ConvType::NoConversion;
		ConvType c = t1->convertible(t2);
		switch (c)
		{
		case ConvType::NoConversion:
			return ConvType::NoConversion;
		case ConvType::LosesPrecision:
			if (conv != ConvType::OnlyExplicit)
				conv = ConvType::LosesPrecision;
			break;
		case ConvType::OnlyExplicit:
			conv = ConvType::OnlyExplicit;
		default:
			break;
		}
	}
	return conv;
}

Expr* Tuple::makeConversion(Expr *expr, TypeExpr *targetType, bool implicit) const
{
	Tuple *tup = dynamic_cast<Tuple*>(expr);

	TypeExpr *type = expr->type();

	if (type->isSame(targetType))
		return expr;

	ConvType conv = type->convertible(targetType);
	if (conv == ConvType::NoConversion)
		error("file", getLine(), "Can't convert %s to %s.", type->stringof().c_str(), targetType->stringof().c_str());
	else if (implicit && conv == ConvType::OnlyExplicit)
		error("file", getLine(), "Can't implicitly convert %s to %s. Use explicit cast.", type->stringof().c_str(), targetType->stringof().c_str());
	else if (conv == ConvType::LosesPrecision)
		emitWarning("file", getLine(), "Conversion from %s to %s loses precision.", type->stringof().c_str(), targetType->stringof().c_str());

	const Tuple *t = dynamic_cast<const Tuple*>(targetType);

	if (tup->_element && t->_element)
	{
		// convert only the array element
		Expr *e = tup->_element->asExpr();
		Tuple *r = new Tuple(e->makeConversion(t->_element->asType()), tup->shape(), expr->getLoc());
		r->analyse();
		return r;
	}

	NodeList nodes = NodeList::empty();
	size_t count = tup->numElements(0);
	for (size_t i = 0; i < count; ++i)
	{
		// TODO: optimize by not recalculating _element each cycle...
		Expr *expr = (tup->_element ? tup->_element : tup->_elements[i])->asExpr();
		TypeExpr *ty = (t->_element ? t->_element : t->_elements[i])->asType();
		nodes = nodes.append(expr->makeConversion(ty, implicit));
	}
	Tuple *r = new Tuple(nodes, expr->getLoc());
	r->analyse();
	return r;
}

Node *Tuple::getMember(String name)
{
	if (name.eq("length") && dimensions() == 1)
	{
		if (_element)
			return _shape[0];
		return new PrimitiveLiteralExpr(SizeT_Type, _elements.length, getLoc());
	}
	if (isType())
	{
		if (name.eq("init"))
			return init();
	}
	if (isType() || isExpr())
	{
		// TODO: can we support these properties on mixed tuples?
		if (name.eq("sizeof"))
			return new PrimitiveLiteralExpr(SizeT_Type, size(), getLoc());
		if (name.eq("alignof"))
			return new PrimitiveLiteralExpr(SizeT_Type, alignment(), getLoc());
	}
	return Node::getMember(name);
}

MutableString64 Tuple::stringof() const
{
	MutableString64 s;
	if (isString())
	{
		s = "\"";
		for (auto e : _elements)
		{
			MutableString64 c = e->asExpr()->constEval()->stringof();
			if (c[c.length - 1] == 'w' || c[c.length - 1] == 'd')
				c.pop_back();
			s.append(c.slice(1, c.length - 1));
		}

		PrimType pt = dynamic_cast<const PrimitiveType*>(_type->_element)->type();
		if (pt != PrimType::c8)
			s.append(pt == PrimType::c16 ? "\"w" : "\"d");
		else
			s.append('"');
		return s;
	}
	else
	{
		s = "[ ";
		if (isSequence())
		{
			s.append(_element->stringof(), "; ");
			bool first = true;
			for (auto &i : _shape)
			{
				if (!first)
					s.append(", ");
				first = false;
				s.append(i->stringof());
			}
		}
		else
		{
			bool first = true;
			for (auto &e : _elements)
			{
				if (!first)
					s.append(", ");
				first = false;
				s.append(e->stringof());
			}
		}
		s.append(" ]");
	}
	return s;
}
MutableString64 Tuple::mangleof() const
{
	MutableString64 m(Concat, 'T', std::to_string(numElements()));
	if (isSequence())
	{
		m.append('S', _element->mangleof());
	}
	else
	{
		m.append('E');
		for (auto e : _elements)
			m.append(e->mangleof());
	}
	return m;
}
raw_ostream &Tuple::dump(raw_ostream &out, int ind)
{
	if (isString())
	{
		return out << str_ref(stringof()) << '\n';
	}
	else
	{
		out << "[\n";
		if (isSequence())
		{
			_element->dump(indent(out, ind + 1), ind + 1);
			indent(out, ind + 1) << "...\n";
			for (auto e : _shape)
				e->dump(indent(out, ind + 1), ind + 1);
		}
		else
		{
			for (auto e : _elements)
				e->dump(indent(out, ind + 1), ind + 1);
		}
		return indent(out, ind) << "]\n";
	}
}

Node *Index::getMember(String name)
{
	return dynamic_cast<AmbiguousExpr*>(_result)->resolve()->getMember(name);
}

raw_ostream &Index::dump(raw_ostream &out, int ind)
{
	out << "index: " << str_ref(_node->stringof()) << "\n";
	indent(out, ind) << "indices: [\n";
	for (auto &i : _indices)
		i->dump(indent(out, ind + 1), ind + 1);
	return indent(out, ind) << "]\n";
}

}
