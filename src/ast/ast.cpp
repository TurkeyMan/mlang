#include "ast.h"
#include "astvisitor.h"
#include "error.h"

namespace m {

std::map<SharedString, TypeExpr*, string_less> typesUsed;

PrimType SizeT_Type = PrimType::i64;
PrimType SSizeT_Type = PrimType::u64;

uint8_t typeFlags[(size_t)PrimType::__NumTypes] =
{
	0, 17, 2, 1, 5, 2, 1, 5, 2, 1, 5, 2, 1, 2, 1, 8, 8, 8, 8
};
uint8_t typeWidth[(size_t)PrimType::__NumTypes] =
{
	0, 1, 8, 8, 8, 16, 16, 16, 32, 32, 32, 64, 64, 128, 128, 16, 32, 64, 128
};
uint8_t typeBytes[(size_t)PrimType::__NumTypes] =
{
	0, 1, 1, 1, 1, 2, 2, 2, 4, 4, 4, 8, 8, 16, 16, 2, 4, 8, 16
};


void Module::accept(ASTVisitor &v) { v.visit(*this); }


Node* makePragma(String identifier, NodeList args)
{
	error(nullptr, 0, "Unknown: pragma(%s, ...)", (const char*)identifier.c_str());
	return nullptr;
}

Declaration *Scope::getDecl(String name, bool onlyLocal)
{
	auto i = _symbols.find(name);
	if (i != _symbols.end())
		return i->second;
	if (!onlyLocal)
	{
		Declaration *decl = nullptr;
		for (auto import : _imports)
		{
			Declaration *d = import->getDecl(name, true);
			if (d)
			{
				if (decl)
					error(nullptr, 0, "'%s': ambiguous lookup", (const char*)name.c_str());
				else
					decl = d;
			}
		}
		if (decl)
			return decl;
		if (!onlyLocal && _parentScope)
			return _parentScope->getDecl(name, false);
	}
	return nullptr;
}
void Scope::addDecl(SharedString name, Declaration *decl)
{
	_symbolTable.push_back(decl);
	_symbols.insert({ std::move(name), decl });
}

MutableString64 Module::stringof() const
{
	if (_declaration)
		return _declaration->name();

	MutableString64 r = _name.length ? String(_name[0]) : String();
	for (size_t i = 1; i < _name.length; ++i)
		r.append('.', _name[i]);
	return r;
}
MutableString64 Module::mangleof() const
{
	MutableString64 r(Concat, std::to_string(_name[0].length), _name[0]);
	for (size_t i = 1; i < _name.length; ++i)
		r.append(std::to_string(_name[i].length), _name[i]);
	return r;
}

Node *Node::getMember(String name)
{
	if (name.eq("stringof"))
		return Tuple::makeStringLiteral(stringof(), PrimType::c8, false, SourceLocation(0));
	if (name.eq("mangleof"))
		return Tuple::makeStringLiteral(mangleof(), PrimType::c8, false, SourceLocation(0));
	return nullptr;
}

Node *Expr::getMember(String name)
{
	if (name.eq("sizeof"))
		return new PrimitiveLiteralExpr(SizeT_Type, type()->size(), getLoc());
	if (name.eq("alignof"))
		return new PrimitiveLiteralExpr(SizeT_Type, type()->alignment(), getLoc());
	return Node::getMember(name);
}

int TypeExpr::ptrDepth() const
{
	const TypeExpr *type = this;

	// ignore const on pointers
	while (type->isConst())
		type = type->asModified()->innerType();

	const PointerType *ptr = type->asPointer();
	int depth = 0;
	while (ptr)
	{
		++depth;
		type = ptr->targetType();
		while (type->isConst())
			type = type->asModified()->innerType();
		ptr = type->asPointer();
	}
	return depth;
}

Node *TypeExpr::getMember(String name)
{
	if (name.eq("init"))
		return init();
	if (name.eq("sizeof"))
		return new PrimitiveLiteralExpr(SizeT_Type, size(), getLoc());
	if (name.eq("alignof"))
		return new PrimitiveLiteralExpr(SizeT_Type, alignment(), getLoc());
	return Node::getMember(name);
}

}
