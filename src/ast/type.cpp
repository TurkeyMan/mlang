#include "ast.h"
#include "astvisitor.h"
#include "error.h"

namespace m {

void PrimitiveType::accept(ASTVisitor &v) { v.visit(*this); }
void ModifiedType::accept(ASTVisitor &v) { v.visit(*this); }
void PointerType::accept(ASTVisitor &v) { v.visit(*this); }
void SliceType::accept(ASTVisitor &v) { v.visit(*this); }
void Struct::accept(ASTVisitor &v) { v.visit(*this); }
void FunctionType::accept(ASTVisitor &v) { v.visit(*this); }
void CVarArgType::accept(ASTVisitor &v) { v.visit(*this); }


const char *primTypeNames[(size_t)PrimType::__NumTypes] =
{
	"void", "bool", "byte", "ubyte", "char", "short", "ushort", "wchar", "int", "uint", "dchar", "long", "ulong", "cent", "ucent", "half", "float", "double", "extended"
};
const char *primTypeMangle[(size_t)PrimType::__NumTypes] =
{ // unused lower case chars: aegknrz
	"v", "b", "o", "p", "c", "s", "t", "w", "i", "j", "u", "l", "m", "x", "y", "h", "f", "d", "q"
};


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
		else if (isSignedIntegral(pt))
		{
			if (isUnsignedIntegral(_type) && tyWidth(_type) < tyWidth(pt))
				return ConvType::Convertible;
			else if (isSignedIntegral(_type) && tyWidth(_type) <= tyWidth(pt))
				return ConvType::Convertible;
		}
		else if (isUnsignedIntegral(pt))
		{
			if (isUnsignedIntegral(_type) && tyWidth(_type) <= tyWidth(pt))
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
	if (depth > 0 && ptrDepth() <= depth)
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
static const char *ptr_ty[] = { "P", "U", "R", "" };
MutableString64 PointerType::mangleof() const
{
	return MutableString64(Concat, ptr_ty[(int)_type], _pointerTarget->mangleof());
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

Node *SliceType::getMember(String name)
{
	Node *r = TypeExpr::getMember(name);
	if (r)
		return r;

	// TODO: length property
	//       - address property?

	return nullptr;

}

bool SliceType::isSame(const TypeExpr *other) const
{
	const SliceType *t = dynamic_cast<const SliceType*>(other);
	if (!t)
		return false;
	return _type == t->_type && _elementType->isSame(t->_elementType);
}
ConvType SliceType::convertible(const TypeExpr *target) const
{
	ice("TODO");
	// TODO: if pointer types are convertible AND same indirection depth, then slice types are convertible...?
	return ConvType::NoConversion;
}
Expr* SliceType::makeConversion(Expr *expr, TypeExpr *targetType, bool implicit) const
{
	ice("TODO");
	return nullptr;
}

MutableString64 SliceType::stringof() const
{
	return MutableString64(Concat, '[', _elementType->stringof(), "; ..]");
}
MutableString64 SliceType::mangleof() const
{
//	return MutableString64(Concat, "S", ptr_ty[(int)_type], _elementType->mangleof());
	return MutableString64(Concat, "S", _elementType->mangleof());
}

raw_ostream &SliceType::dump(raw_ostream &out, int ind)
{
	TypeExpr::dump(out << "slice", ind) << "\n";
	++ind;
	if (_elementType)
		_elementType->dump(indent(out, ind) << "target: ", ind);
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
	ice("TODO");
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
	ice("TODO"); // can't convert struct type...
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
MutableString64 FunctionType::mangleof() const { ice("TODO"); return ""; }

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

Expr* CVarArgType::init() const
{
	return new PrimitiveLiteralExpr(PrimType::v, 0ull, getLoc());
}

MutableString64 CVarArgType::stringof() const { return "..."; }
MutableString64 CVarArgType::mangleof() const { ice("TODO"); return ""; }

raw_ostream &CVarArgType::dump(raw_ostream &out, int ind)
{
	return out << "...\n";
}

}
