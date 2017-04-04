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
const char *primTypeNames[(size_t)PrimType::__NumTypes] =
{
	"void", "bool", "byte", "ubyte", "char", "short", "ushort", "wchar", "int", "uint", "dchar", "long", "ulong", "cent", "ucent", "half", "float", "double", "extended"
};
const char *primTypeMangle[(size_t)PrimType::__NumTypes] =
{ // unused lower case chars: aegknrz
	"v", "b", "o", "p", "c", "s", "t", "w", "i", "j", "u", "l", "m", "x", "y", "h", "f", "d", "q"
};


void Declaration::accept(ASTVisitor &v) { v.visit(*this); }
void ExpressionStatement::accept(ASTVisitor &v) { v.visit(*this); }
void ReturnStatement::accept(ASTVisitor &v) { v.visit(*this); }
void ScopeStatement::accept(ASTVisitor &v) { v.visit(*this); }
void IfStatement::accept(ASTVisitor &v) { v.visit(*this); }
void LoopStatement::accept(ASTVisitor &v) { v.visit(*this); }
void Module::accept(ASTVisitor &v) { v.visit(*this); }
void PrimitiveType::accept(ASTVisitor &v) { v.visit(*this); }
void ModifiedType::accept(ASTVisitor &v) { v.visit(*this); }
void PointerType::accept(ASTVisitor &v) { v.visit(*this); }
void Struct::accept(ASTVisitor &v) { v.visit(*this); }
void FunctionType::accept(ASTVisitor &v) { v.visit(*this); }
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
void UnknownExpr::accept(ASTVisitor &v) { v.visit(*this); }
void Identifier::accept(ASTVisitor &v) { v.visit(*this); }
void MemberLookup::accept(ASTVisitor &v) { v.visit(*this); }
void Tuple::accept(ASTVisitor &v) { v.visit(*this); }
void Index::accept(ASTVisitor &v) { v.visit(*this); }
void ModuleDecl::accept(ASTVisitor &v) { v.visit(*this); }
void ImportDecl::accept(ASTVisitor &v) { v.visit(*this); }
void TypeDecl::accept(ASTVisitor &v) { v.visit(*this); }
void ValDecl::accept(ASTVisitor &v) { v.visit(*this); }
void VarDecl::accept(ASTVisitor &v) { v.visit(*this); }


template <typename... Args>
PrimitiveType* MakeTypeHelper<PrimitiveType, Args...>::makeType(Scope *scope, PrimType type, SourceLocation loc)
{
	auto t = typesUsed.find(primTypeMangle[(int)type]);
	if (t != typesUsed.end())
		return static_cast<PrimitiveType*>(t->second);
	PrimitiveType *prim = new PrimitiveType(type, loc);
	typesUsed.insert({ primTypeMangle[(int)type], prim });
	return prim;
}


Statement* makeForEach(DeclList iterators, Expr *range, ScopeStatement *body, SourceLocation loc)
{
	assert(false); // TODO!!

	// only [value], or [key, value] are allowed!
	assert(iterators.length <= 2);

	// assign void initialisation for counters (initialised at head of loop body)
	for (auto &i : iterators)
		((VarDecl*)i)->_init = new PrimitiveLiteralExpr(PrimType::v, 0ull, loc);

	VarDecl *r = new VarDecl("__loop_range", nullptr, range, NodeList::empty(), loc);
	iterators.prepend(r);

	Expr *cond = nullptr; // cast(bool)__loop_range.empty()

	StatementList entry = StatementList::empty();
	if (iterators.length)
	{
		if (iterators.length == 3)
		{
			// iterators[1] = __loop_range.front.key;
			// entry = entry.append(assignment);
		}

		// iterators[iterators.length-1] = __loop_range.front.value;
		// entry = entry.append(assignment);
	}
	body->_statements = entry.append(body->_statements);

	Expr *increment = nullptr; // __loop_range = __loop_range.popFront();

	return new LoopStatement(iterators, cond, ExprList::empty().append(increment), body, loc);
}

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
			if (d && decl)
			{
				// multiple resolved!
				error(nullptr, 0, "'%s': ambiguous lookup", (const char*)name.c_str());
			}
			else
				decl = d;
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

const SharedString& Declaration::mangledName() const
{
	if (_mangledName.empty())
	{
		for (auto a : _attributes)
		{
			Identifier *i = dynamic_cast<Identifier*>(a);
			if (i && i->getName().eq("extern_c"))
			{
				_mangledName = name();
				return _mangledName;
			}
		}
		_mangledName = SharedString(Concat, "_M", mangleof()); // TODO: include scope...
	}
	return _mangledName;
}

Node *Declaration::getMember(String name)
{
	if (name.eq("mangleof"))
		return Tuple::makeStringLiteral(mangledName(), PrimType::c8, false, SourceLocation(0));
	return Statement::getMember(name);
}

raw_ostream &ExpressionStatement::dump(raw_ostream &out, int ind)
{
	return _expression->dump(out, ind);
}

raw_ostream &ReturnStatement::dump(raw_ostream &out, int ind)
{
	if (_expression)
	{
		Statement::dump(out << "return: ", ind);
		return _expression->dump(out, ind + 1);
	}
	else
		return Statement::dump(out << "return\n", ind);
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




//****************
//** Type nodes **
//****************

Expr* PrimitiveType::init() const
{
	return _init;
}

static int magicNumbers[][4] =
{
	{ 3, 6, 15, 33 },  // dig
	{ 11, 24, 53, 113 }, // mant_dig
	{ 4, 38, 308, 4932 }, // max_10_exp
	{ 15, 128, 1024, 16383 }, // max_exp
	{ -4, -37, -307, -4931 }, // min_10_exp
	{ -14, -125, -1021, -16382 }, // min_exp
	{ 5, 8, 11, 15 }, // exp_bits
	{ 10, 23, 52, 112 }, // mant_bits
};
static uint64_t magicNumbers2[][4] =
{
	{ 0x7FF0000000000000ULL, 0x7FF0000000000000ULL, 0x7FF0000000000000ULL, 0x7FF0000000000000ULL }, // infinity
	{ 0x7FF8000000000000ULL, 0x7FF8000000000000ULL, 0x7FF8000000000000ULL, 0x7FF8000000000000ULL }, // nan
	{ 0, 0x47EFFFFFE0000000ULL, 0x7FEFFFFFFFFFFFFFULL, 0 }, // max
	{ 0, 0x3810000000000000ULL, 0x0010000000000000ULL, 0 }, // min_normal
	{ 0, 0x3E80000000000000ULL, 0x3CB0000000000000ULL, 0 }, // epsilon
};
Node *PrimitiveType::getMember(String name)
{
	switch (_type)
	{
	case PrimType::u1:
		break;
	case PrimType::f16:
	case PrimType::f32:
	case PrimType::f64:
	case PrimType::f128:
		if (name.eq("infinity"))
			return new PrimitiveLiteralExpr(_type, magicNumbers2[0][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		if (name.eq("nan"))
			return new PrimitiveLiteralExpr(_type, magicNumbers2[1][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		if (name.eq("max"))
			return new PrimitiveLiteralExpr(_type, magicNumbers2[2][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		if (name.eq("min_normal"))
			return new PrimitiveLiteralExpr(_type, magicNumbers2[3][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		if (name.eq("epsilon"))
			return new PrimitiveLiteralExpr(_type, magicNumbers2[4][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		if (name.eq("dig"))
			return new PrimitiveLiteralExpr(PrimType::i32, (int64_t)magicNumbers[0][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		if (name.eq("mant_dig"))
			return new PrimitiveLiteralExpr(PrimType::i32, (int64_t)magicNumbers[1][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		if (name.eq("max_10_exp"))
			return new PrimitiveLiteralExpr(PrimType::i32, (int64_t)magicNumbers[2][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		if (name.eq("max_exp"))
			return new PrimitiveLiteralExpr(PrimType::i32, (int64_t)magicNumbers[3][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		if (name.eq("min_10_exp"))
			return new PrimitiveLiteralExpr(PrimType::i32, (int64_t)magicNumbers[4][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		if (name.eq("min_exp"))
			return new PrimitiveLiteralExpr(PrimType::i32, (int64_t)magicNumbers[5][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		if (name.eq("exp_bits"))
			return new PrimitiveLiteralExpr(PrimType::i32, (int64_t)magicNumbers[6][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		if (name.eq("mant_bits"))
			return new PrimitiveLiteralExpr(PrimType::i32, (int64_t)magicNumbers[7][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		break;
	case PrimType::u8:
		if (name.eq("max")) return new PrimitiveLiteralExpr(_type, 0xFFULL, SourceLocation(-1));
		if (name.eq("min")) return new PrimitiveLiteralExpr(_type, 0ULL, SourceLocation(-1));
		break;
	case PrimType::u16:
		if (name.eq("max")) return new PrimitiveLiteralExpr(_type, 0xFFFFULL, SourceLocation(-1));
		if (name.eq("min")) return new PrimitiveLiteralExpr(_type, 0ULL, SourceLocation(-1));
		break;
	case PrimType::u32:
		if (name.eq("max")) return new PrimitiveLiteralExpr(_type, 0xFFFFFFFFULL, SourceLocation(-1));
		if (name.eq("min")) return new PrimitiveLiteralExpr(_type, 0ULL, SourceLocation(-1));
		break;
	case PrimType::u64:
		if (name.eq("max")) return new PrimitiveLiteralExpr(_type, 0xFFFFFFFFFFFFFFFFULL, SourceLocation(-1));
		if (name.eq("min")) return new PrimitiveLiteralExpr(_type, 0ULL, SourceLocation(-1));
		break;
	case PrimType::u128:
		if (name.eq("max")) return new PrimitiveLiteralExpr(_type, 0ULL, SourceLocation(-1));
		if (name.eq("min")) return new PrimitiveLiteralExpr(_type, 0ULL, SourceLocation(-1));
		break;
	case PrimType::i8:
		if (name.eq("max")) return new PrimitiveLiteralExpr(_type, 0x7FLL, SourceLocation(-1));
		if (name.eq("min")) return new PrimitiveLiteralExpr(_type, 0x80LL, SourceLocation(-1));
		break;
	case PrimType::i16:
		if (name.eq("max")) return new PrimitiveLiteralExpr(_type, 0x7FFFLL, SourceLocation(-1));
		if (name.eq("min")) return new PrimitiveLiteralExpr(_type, 0x8000LL, SourceLocation(-1));
		break;
	case PrimType::i32:
		if (name.eq("max")) return new PrimitiveLiteralExpr(_type, 0x7FFFFFFFLL, SourceLocation(-1));
		if (name.eq("min")) return new PrimitiveLiteralExpr(_type, 0x80000000LL, SourceLocation(-1));
		break;
	case PrimType::i64:
		if (name.eq("max")) return new PrimitiveLiteralExpr(_type, 0x7FFFFFFFFFFFFFFFLL, SourceLocation(-1));
		if (name.eq("min")) return new PrimitiveLiteralExpr(_type, 0x8000000000000000LL, SourceLocation(-1));
		break;
	case PrimType::i128:
		if (name.eq("max")) return new PrimitiveLiteralExpr(_type, 0LL, SourceLocation(-1));
		if (name.eq("min")) return new PrimitiveLiteralExpr(_type, 0LL, SourceLocation(-1));
		break;
	case PrimType::c8:
	case PrimType::c16:
	case PrimType::c32:
		break;
	default:
		assert(0);
	}
	return TypeExpr::getMember(name);
}

bool PrimitiveType::isSame(const TypeExpr *other) const
{
	const PrimitiveType *pt = dynamic_cast<const PrimitiveType*>(other);
	if (!pt)
		return false;
	return _type == pt->_type;
}
ConvType PrimitiveType::convertible(const TypeExpr *target) const
{
	// primitive types can convert to const
	bool targetConst = false;
	while (target->isConst())
	{
		targetConst = true;
		target = target->asModified()->innerType();
	}

	const PrimitiveType *primt = target->asPrimitive();
	if (primt)
	{
		PrimType pt = primt->type();

		if (isFloat(pt))
		{
			if (isFloat(_type) && tyWidth(_type) <= tyWidth(pt))
				return ConvType::Convertible;
			else if ((isInt(_type) || isChar(_type) || isBool(_type)) && tyWidth(_type) < tyWidth(pt))
				return ConvType::Convertible;
		}
		else if (isSignedInt(pt))
		{
			if (isUnsignedInt(_type) && tyWidth(_type) < tyWidth(pt))
				return ConvType::Convertible;
			else if (isSignedInt(_type) && tyWidth(_type) <= tyWidth(pt))
				return ConvType::Convertible;
		}
		else if (isUnsignedInt(pt))
		{
			if (isUnsignedInt(_type) && tyWidth(_type) <= tyWidth(pt))
				return ConvType::Convertible;
		}
		return ConvType::LosesPrecision;
	}

	// TODO: size_t can explicitly convert to pointers... maybe?

	// TODO: test if target can construct from 'this'

	return ConvType::NoConversion;
}
Expr* PrimitiveType::makeConversion(Expr *expr, TypeExpr *targetType, bool implicit) const
{
	PrimitiveType *primt = targetType->asPrimitive();
	if (primt)
	{
		if (_type == primt->_type)
			return expr;
		return new TypeConvertExpr(expr, targetType, implicit, expr->getLoc());
	}

	PointerType *ptrt = targetType->asPointer();
	if (ptrt)
	{
		if (isIntegral())
			return new TypeConvertExpr(expr, targetType, implicit, expr->getLoc());
	}

	// TODO: can target construct from 'this'?

	error("file", getLine(), "Can't convert ___ to ___.");
	return nullptr;
}

MutableString64 PrimitiveType::stringof() const
{
	return primTypeNames[(size_t)type()];
}
MutableString64 PrimitiveType::mangleof() const
{
	return primTypeMangle[(size_t)type()];
}
raw_ostream &PrimitiveType::dump(raw_ostream &out, int ind)
{
	return out << str_ref(stringof()) << '\n';
}

ModifiedType* ModifiedType::makeModified(TypeMod mods, TypeExpr *type, SourceLocation loc)
{
	// if the type is already modified, we don't want to over-mod it.
	ModifiedType *m = type->asModified();
	if (m)
	{
		if ((m->_mod & mods) == mods)
			return type->asModified();
		return new ModifiedType((TypeMod)(m->_mod | mods), m->innerType(), loc);
	}
	return new ModifiedType(mods, type, loc);
}

Node *ModifiedType::getMember(String name)
{
	// TODO: data members need to be promoted...
	return _type->getMember(name);
}

bool ModifiedType::isSame(const TypeExpr *other) const
{
	const ModifiedType *t = dynamic_cast<const ModifiedType*>(other);
	if (!t)
		return false;
	return _mod == t->_mod && _type->isSame(t->_type);
}
ConvType ModifiedType::convertible(const TypeExpr *target) const
{
	const ModifiedType *mod = target->asModified();
	if (mod && _mod == mod->mod())
		return ConvType::Convertible;
	return ConvType::NoConversion;
}
Expr* ModifiedType::makeConversion(Expr *expr, TypeExpr *targetType, bool implicit) const
{
	return new TypeConvertExpr(expr, targetType, implicit, SourceLocation(0));
}
MutableString64 ModifiedType::stringof() const
{
	if (_mod == TypeMod::Const)
		return MutableString64(Concat, "const(", _type->stringof(), ')');
	return _type->stringof();
}
MutableString64 ModifiedType::mangleof() const
{
	if (_mod == TypeMod::Const)
		return MutableString64(Concat, 'C', _type->stringof());
	return _type->stringof();
}
raw_ostream &ModifiedType::dump(raw_ostream &out, int ind)
{
	TypeExpr::dump(out << (_mod == TypeMod::Const ? "const" : "modify none"), ind) << "\n";
	++ind;
	if (_type)
		_type->dump(indent(out, ind) << "inner: ", ind);
	if (_init)
		_init->dump(indent(out, ind) << "init: ", ind);
	return out;
}


Node *PointerType::getMember(String name)
{
	Node *r = TypeExpr::getMember(name);
	if (r)
		return r;

	// TODO: do pointers have any properties?
	//       - address property?

	return _pointerTarget->getMember(name);
}

static const char *ptrTypeStrings[] = { "*", "^", "&", "#" };
bool PointerType::isSame(const TypeExpr *other) const
{
	const PointerType *t = dynamic_cast<const PointerType*>(other);
	if (!t)
		return false;
	return _type == t->_type && _pointerTarget->isSame(t->_pointerTarget);
}
ConvType PointerType::convertible(const TypeExpr *target) const
{
	const PointerType *ptrt = target->asPointer();
	if (ptrt)
	{
		const TypeExpr *from = this;

		const PointerType *toStack[128];
		const PointerType *fromStack[128];
		size_t toDepth = 0, fromDepth = 0;

		// burrow down to the pointer target
		const PointerType *ptrf;
		do
		{
			ptrf = from->asPointer();
			if (!ptrf)
				return ConvType::NoConversion;
			if (ptrt)
			{
				toStack[toDepth++] = ptrt;
				target = ptrt->targetType();
				ptrt = target->asPointer();
			}
			fromStack[fromDepth++] = ptrf;
			from = ptrf->targetType();
			ptrf = from->asPointer();
		} while (ptrt || ptrf);

		// compare pointer targets are the same type
		ConvType conv = ConvType::Convertible;
		if (!from->isSame(target))
		{
			Tuple *from_tup = (Tuple*)dynamic_cast<const Tuple*>(from);
			Tuple *to_tup = (Tuple*)dynamic_cast<const Tuple*>(target);
			if ((!from_tup || !from_tup->isSequence()) && (!to_tup || !to_tup->isSequence()))
				return ConvType::NoConversion;
			else if (from_tup && to_tup)
			{
				if (to_tup->numElements() > from_tup->numElements() || !from_tup->seqElement()->asType()->isSame(to_tup->seqElement()->asType()))
					return ConvType::NoConversion;
				conv = ConvType::OnlyExplicit;
			}
			else if (from_tup)
			{
				if (!from_tup->seqElement()->asType()->isSame(target))
					return ConvType::NoConversion;
				conv = ConvType::OnlyExplicit;
			}
			else if (to_tup)
			{
				if (!from->isSame(to_tup->seqElement()->asType()))
					return ConvType::NoConversion;
				conv = ConvType::OnlyExplicit;
			}
		}

		// validate each layer of pointer is convertible
		while (fromDepth--, toDepth--)
		{
			if (!fromStack[fromDepth]->canPromote(toStack[toDepth]->ptrType()))
				return ConvType::NoConversion;
		}
		return conv;
	}

	// if target is not a pointer, we will try and convert the pointer target type
	TypeExpr *from = _pointerTarget;
	while (from->asPointer())
		from = ((PointerType*)from)->_pointerTarget;

	return from->convertible(target);
}
Expr* PointerType::makeConversion(Expr *expr, TypeExpr *targetType, bool implicit) const
{
//	expr = expr->resolveExpr();
//	targetType = targetType->resolveType();

	int depth = targetType->ptrDepth();
	if(depth > 0 && ptrDepth() <= depth)
	{
		TypeExpr *from = expr->type();
		TypeExpr *target = targetType;

		bool srcConst = false, targetConst = false;
//		if (target->isConst())
//		{
//			targetConst = true;
//			target = target->asModified()->innerType();
//		}

		PointerType *ptrt = targetType->asPointer();

		PointerType *toStack[128];
		PointerType *fromStack[128];
		size_t toDepth = 0, fromDepth = 0;

		// burrow down to the pointer target
		PointerType *ptrf;
		do
		{
			ptrf = from->asPointer();
			assert(ptrf);
 			if (ptrt)
			{
				toStack[toDepth++] = ptrt;
				target = ptrt->targetType();
//				if (target->isConst())
//				{
//					targetConst = true;
//					target = target->asModified()->innerType();
//				}
				ptrt = target->asPointer();
			}
			fromStack[fromDepth++] = ptrf;
			from = ptrf->targetType();
//			if (from->isConst())
//			{
//				srcConst = true;
//				from = from->asModified()->innerType();
//			}
			ptrf = from->asPointer();
		} while (ptrt || ptrf);

		if (from->isConst())
		{
			srcConst = true;
			from = from->asModified()->innerType();
		}
		if (target->isConst())
		{
			targetConst = true;
			target = target->asModified()->innerType();
		}

		if (srcConst && !targetConst)
			error(nullptr, 0, "invalid const conversion...");

		// compare pointer targets are the same type
		if (!from->isSame(target))
		{
			// only allow explicit conversions of array types
			assert(!implicit);

			Tuple *from_tup = dynamic_cast<Tuple*>(from);
			Tuple *to_tup = dynamic_cast<Tuple*>(target);
			assert((from_tup && from_tup->isSequence()) || (to_tup && to_tup->isSequence()));

			if (from_tup && to_tup)
				assert(to_tup->numElements() <= from_tup->numElements() && from_tup->seqElement()->asType()->isSame(to_tup->seqElement()->asType()));
			else if (from_tup)
				assert(from_tup->seqElement()->asType()->isSame(target));
			else if (to_tup)
				assert(from->isSame(to_tup->seqElement()->asType()));

			expr = new TypeConvertExpr(expr, targetType, false, SourceLocation(0));
		}

		// validate each layer of pointer is convertible
		while (fromDepth--, toDepth--)
			assert(fromStack[fromDepth]->canPromote(toStack[toDepth]->ptrType()));
		return expr;
	}

	Expr *targetConversion = new DerefExpr(expr, expr->getLoc());
	return targetConversion->makeConversion(targetType);
}
bool PointerType::canPromote(PtrType to) const
{
	switch (to)
	{
	case PtrType::RawPtr:
	case PtrType::BorrowedPtr:
		return true;
	case PtrType::UniquePtr:
		return _type == PtrType::UniquePtr;
	case PtrType::LValue:
		return false;
	}
	return false;
}

MutableString64 PointerType::stringof() const
{
	return MutableString64(Concat, _pointerTarget->stringof(), '*');
}
MutableString64 PointerType::mangleof() const
{
	static const char *ty[] = { "P", "U", "R", "" };
	return MutableString64(Concat, ty[(int)_type], _pointerTarget->mangleof());
}
raw_ostream &PointerType::dump(raw_ostream &out, int ind)
{
	TypeExpr::dump(out << ptrTypeStrings[(int)_type], ind) << "\n";
	++ind;
	if (_pointerTarget)
		_pointerTarget->dump(indent(out, ind) << "target: ", ind);
	if (_init)
		_init->dump(indent(out, ind) << "init: ", ind);
	return out;
}

size_t Struct::memberIndex(String name)
{
	size_t i = 0;
	for (; i < _dataMembers.size(); ++i)
	{
		if (_dataMembers[i].decl->name().eq(name))
			break;
	}
	return i;
}

Node *Struct::getMember(String name)
{
	Node *r = TypeExpr::getMember(name);
	if (r)
		return r;

	// TODO: implicit members?
	//       - all members as tuple?

	Declaration *decl = getDecl(name, true);
	if (dynamic_cast<ValDecl*>(decl))
		return ((ValDecl*)decl)->value();
	else if (dynamic_cast<TypeDecl*>(decl))
		return ((TypeDecl*)decl)->type();
	return nullptr;
}

bool Struct::isSame(const TypeExpr *other) const
{
	assert(false);
	return false;
}

ConvType Struct::convertible(const TypeExpr *target) const
{
	return ConvType::NoConversion;
}

Expr* Struct::makeConversion(Expr *expr, TypeExpr *targetType, bool implicit) const
{
	TypeExpr *type = expr->type();
	TypeExpr *from = type->resolveType();
	TypeExpr *to = targetType->resolveType();
	if (from == to)
		return expr;
	assert(false); // can't convert struct type...
	return nullptr;
}


MutableString64 Struct::stringof() const
{
	MutableString64 s = "{ ";
	bool first = true;
	for (auto &m : _dataMembers)
	{
		if (!first)
			s.append(", ");
		first = false;
		s.append(m.decl->targetType()->stringof());
	}
	s.append(" }");
	return s;
}
raw_ostream &Struct::dump(raw_ostream &out, int ind)
{
	out << "struct {\n";
	for (auto m : _members)
		m->dump(indent(out, ind + 1), ind + 1);
	return indent(out, ind) << "}\n";
}

bool FunctionType::isSame(const TypeExpr *other) const
{
	const FunctionType *ft = dynamic_cast<const FunctionType*>(other);
	if (!ft)
		return false;
	if (!_returnType->isSame(ft->_returnType))
		return false;
	if (_argTypes.length != ft->_argTypes.length)
		return false;
	for (size_t i = 0; i < _argTypes.length; ++i)
	{
		if (!_argTypes[i]->isSame(ft->_argTypes[i]))
			return false;
	}
	return true;
}
ConvType FunctionType::convertible(const TypeExpr *target) const
{
	const FunctionType *to = target->asFunction();
	if (to)
	{
		// TODO: handle argument const-promotion, arg pointer type promotion, etc

		return isSame(to) ? ConvType::Convertible : ConvType::NoConversion;
	}
	return ConvType::NoConversion;
}

MutableString64 FunctionType::stringof() const
{
	MutableString64 r = _returnType ? _returnType->stringof() : MutableString64("???");
	r.append('(');
	for (size_t i = 0; i < _argTypes.length; ++i)
		r.append(i > 0 ? "," : "", _argTypes[i]->stringof());
	r.append(')');
	return r;
}
MutableString64 FunctionType::mangleof() const { assert(false); return ""; }

raw_ostream &FunctionType::dump(raw_ostream &out, int ind)
{
	out << "function\n";

	ind++;
	indent(out, ind) << "return: ";
	if (_returnType)
		_returnType->dump(out, ind + 1);
	else
		out << "???\n";
	indent(out, ind) << "args: (\n";
	for (auto a : _argTypes)
		a->dump(indent(out, ind + 1), ind + 1);
	return indent(out, ind) << ")\n";
}


//***********************
//** Declaration nodes **
//***********************

Node *ModuleDecl::getMember(String name)
{
	Declaration *decl = _module->getDecl(name, true);
	if (decl)
		return decl;
	return Declaration::getMember(name);
}

Node *ImportDecl::getMember(String name)
{
	Declaration *decl = _module->getDecl(name, true);
	if (decl)
		return decl;
	return Declaration::getMember(name);
}

raw_ostream &TypeDecl::dump(raw_ostream &out, int ind)
{
	Declaration::dump(out << "type: " << str_ref(_name), ind) << "\n";
	++ind;
	if (_type)
		_type->dump(indent(out, ind) << "as: ", ind + 1);
	return out;
}

MutableString64 ValDecl::stringof() const
{
	return MutableString64(Concat, _owner->module()->stringof(), '.', _name);
}

MutableString64 ValDecl::mangleof() const
{
	MutableString64 m = _owner->module()->mangleof();
	m.append(std::to_string(_name.size()), _name);
	FunctionType *fn = _type->asFunction();
	if (!fn)
		return m;
	for (auto &a : fn->argTypes())
		m.append(a->mangleof());
	return m;
}

raw_ostream &ValDecl::dump(raw_ostream &out, int ind)
{
	Declaration::dump(out << "def: #" << str_ref(_name), ind) << "\n";
	++ind;
	if (_type)
		_type->dump(indent(out, ind) << "type: ", ind + 1);
	if (_value)
		_value->dump(indent(out, ind) << "val: ", ind + 1);
	return out;
}

MutableString64 VarDecl::stringof() const
{
	return MutableString64(Concat, _owner->module()->stringof(), '.', _name);
}

MutableString64 VarDecl::mangleof() const
{
	MutableString64 m = _owner->module()->mangleof();
	m.append(std::to_string(_name.size()), _name);
	FunctionType *fn = _type->asFunction();
	if (!fn)
		return m;
	for (auto &a : fn->argTypes())
		m.append(a->mangleof());
	return m;
}

raw_ostream &VarDecl::dump(raw_ostream &out, int ind)
{
	Declaration::dump(out << "var: #" << str_ref(_name), ind) << "\n";
	++ind;
	if (_valType)
		_valType->dump(indent(out, ind) << "type: ", ind + 1);
	if (_init)
		_init->dump(indent(out, ind) << "init: ", ind + 1);
	return out;
}


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

raw_ostream &ScopeStatement::dump(raw_ostream &out, int ind)
{
	for (auto s : _statements)
		s->dump(out, ind);
	return out;
}

raw_ostream &IfStatement::dump(raw_ostream &out, int ind)
{
	Statement::dump(out << "if\n", ind);
	ind++;
	if (_initStatements.length > 0)
	{
		indent(out, ind) << "init: {\n";
		for (auto s : _initStatements)
			s->dump(indent(out, ind + 1), ind + 1);
		indent(out, ind) << "}\n";
	}
	_cond->dump(indent(out, ind) << "cond: ", ind + 1);
	indent(out, ind) << "then: {\n";
	_then->dump(indent(out, ind + 1), ind + 1);
	indent(out, ind) << "}\n";
	if (_else)
	{
		indent(out, ind) << "else: {\n";
		_else->dump(indent(out, ind + 1), ind + 1);
		indent(out, ind) << "}\n";
	}
	return out;
}

raw_ostream &LoopStatement::dump(raw_ostream &out, int ind)
{
	Statement::dump(out << "loop\n", ind);
	ind++;
	if (_iterators.length)
	{
		indent(out, ind) << "iterators: {\n";
		for (auto i : _iterators)
			i->dump(indent(out, ind + 1), ind + 1);
		indent(out, ind) << "}\n";
	}
	if (_cond)
		_cond->dump(indent(out, ind) << "while: ", ind + 1);
	if (_increments.length)
	{
		indent(out, ind) << "increments: {\n";
		for (auto i : _increments)
			i->dump(indent(out, ind + 1), ind + 1);
		indent(out, ind) << "}\n";
	}
	indent(out, ind) << "body: {\n";
	_body->dump(indent(out, ind + 1), ind + 1);
	indent(out, ind) << "}\n";
	return out;
}

}
