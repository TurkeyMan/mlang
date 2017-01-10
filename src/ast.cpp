#include "ast.h"
#include "astvisitor.h"
#include "error.h"

std::map<std::string, TypeExpr*> typesUsed;

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
	"void", "u1", "i8", "u8", "c8", "i16", "u16", "c16", "i32", "u32", "c32", "i64", "u64", "i128", "u128", "f16", "f32", "f64", "f128"
};
const char *primTypeMangle[(size_t)PrimType::__NumTypes] =
{ // unused lower case chars: aegknrz
	"v", "b", "o", "p", "c", "s", "t", "w", "i", "j", "u", "l", "m", "x", "y", "h", "f", "d", "q"
};


void Declaration::accept(ASTVisitor &v) { v.visit(*this); }
void ModuleStatement::accept(ASTVisitor &v) { v.visit(*this); }
void ExpressionStatement::accept(ASTVisitor &v) { v.visit(*this); }
void ReturnStatement::accept(ASTVisitor &v) { v.visit(*this); }
void ScopeStatement::accept(ASTVisitor &v) { v.visit(*this); }
void IfStatement::accept(ASTVisitor &v) { v.visit(*this); }
void LoopStatement::accept(ASTVisitor &v) { v.visit(*this); }
void Module::accept(ASTVisitor &v) { v.visit(*this); }
void PrimitiveType::accept(ASTVisitor &v) { v.visit(*this); }
void PointerType::accept(ASTVisitor &v) { v.visit(*this); }
void Struct::accept(ASTVisitor &v) { v.visit(*this); }
void FunctionType::accept(ASTVisitor &v) { v.visit(*this); }
void PrimitiveLiteralExpr::accept(ASTVisitor &v) { v.visit(*this); }
void AggregateLiteralExpr::accept(ASTVisitor &v) { v.visit(*this); }
void ArrayLiteralExpr::accept(ASTVisitor &v) { v.visit(*this); }
void FunctionLiteralExpr::accept(ASTVisitor &v) { v.visit(*this); }
void RefExpr::accept(ASTVisitor &v) { v.visit(*this); }
void DerefExpr::accept(ASTVisitor &v) { v.visit(*this); }
void TypeConvertExpr::accept(ASTVisitor &v) { v.visit(*this); }
void UnaryExpr::accept(ASTVisitor &v) { v.visit(*this); }
void BinaryExpr::accept(ASTVisitor &v) { v.visit(*this); }
void CallExpr::accept(ASTVisitor &v) { v.visit(*this); }
void AssignExpr::accept(ASTVisitor &v) { v.visit(*this); }
void BindExpr::accept(ASTVisitor &v) { v.visit(*this); }
void Identifier::accept(ASTVisitor &v) { v.visit(*this); }
void MemberLookup::accept(ASTVisitor &v) { v.visit(*this); }
void Tuple::accept(ASTVisitor &v) { v.visit(*this); }
void UnknownIndex::accept(ASTVisitor &v) { v.visit(*this); }
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

	VarDecl *r = new VarDecl("__loop_range", nullptr, range, loc);
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


Declaration *Scope::getDecl(const std::string& name, bool onlyLocal)
{
	auto i = _symbols.find(name);
	if (i != _symbols.end())
		return i->second;
	if (!onlyLocal)
	{
		for (auto import : _imports)
		{
			Declaration *decl = import.second->getDecl(name);
			if (decl)
				return decl;
		}
		if (!onlyLocal && _parentScope)
			return _parentScope->getDecl(name, false);
	}
	return nullptr;
}
void Scope::addDecl(const std::string& name, Declaration *decl)
{
	_symbols.insert({ name, decl });
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


Node *Node::getMember(const std::string &name)
{
	if (name == "stringof")
	{
		// TODO: need slice + string literals
		assert(false);
		return nullptr;
	}
	if (name == "mangleof")
	{
		// TODO: need slice + string literals
		assert(false);
		return nullptr;
	}
	return nullptr;
}

Node *Expr::getMember(const std::string &name)
{
	if (name == "sizeof")
		return new PrimitiveLiteralExpr(SizeT_Type, type()->size(), getLoc());
	if (name == "alignof")
		return new PrimitiveLiteralExpr(SizeT_Type, type()->alignment(), getLoc());
	return Node::getMember(name);
}

int TypeExpr::ptrDepth() const
{
	int depth = 0;
	const PointerType *ptr = asPointer();
	while (ptr)
	{
		++depth;
		ptr = ptr->targetType()->asPointer();
	}
	return depth;
}

Node *TypeExpr::getMember(const std::string &name)
{
	if (name == "init")
		return init();
	if (name == "sizeof")
		return new PrimitiveLiteralExpr(SizeT_Type, size(), getLoc());
	if (name == "alignof")
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
Node *PrimitiveType::getMember(const std::string &name)
{
	switch (_type)
	{
	case PrimType::u1:
		break;
	case PrimType::f16:
	case PrimType::f32:
	case PrimType::f64:
	case PrimType::f128:
		if (name == "infinity")
			return new PrimitiveLiteralExpr(_type, magicNumbers2[0][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		if (name == "nan")
			return new PrimitiveLiteralExpr(_type, magicNumbers2[1][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		if (name == "max")
			return new PrimitiveLiteralExpr(_type, magicNumbers2[2][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		if (name == "min_normal")
			return new PrimitiveLiteralExpr(_type, magicNumbers2[3][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		if (name == "epsilon")
			return new PrimitiveLiteralExpr(_type, magicNumbers2[4][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		if (name == "dig")
			return new PrimitiveLiteralExpr(PrimType::i32, (int64_t)magicNumbers[0][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		if (name == "mant_dig")
			return new PrimitiveLiteralExpr(PrimType::i32, (int64_t)magicNumbers[1][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		if (name == "max_10_exp")
			return new PrimitiveLiteralExpr(PrimType::i32, (int64_t)magicNumbers[2][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		if (name == "max_exp")
			return new PrimitiveLiteralExpr(PrimType::i32, (int64_t)magicNumbers[3][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		if (name == "min_10_exp")
			return new PrimitiveLiteralExpr(PrimType::i32, (int64_t)magicNumbers[4][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		if (name == "min_exp")
			return new PrimitiveLiteralExpr(PrimType::i32, (int64_t)magicNumbers[5][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		if (name == "exp_bits")
			return new PrimitiveLiteralExpr(PrimType::i32, (int64_t)magicNumbers[6][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		if (name == "mant_bits")
			return new PrimitiveLiteralExpr(PrimType::i32, (int64_t)magicNumbers[7][(int)_type - (int)PrimType::f16], SourceLocation(-1));
		break;
	case PrimType::u8:
		if (name == "max") return new PrimitiveLiteralExpr(_type, 0xFFULL, SourceLocation(-1));
		if (name == "min") return new PrimitiveLiteralExpr(_type, 0ULL, SourceLocation(-1));
		break;
	case PrimType::u16:
		if (name == "max") return new PrimitiveLiteralExpr(_type, 0xFFFFULL, SourceLocation(-1));
		if (name == "min") return new PrimitiveLiteralExpr(_type, 0ULL, SourceLocation(-1));
		break;
	case PrimType::u32:
		if (name == "max") return new PrimitiveLiteralExpr(_type, 0xFFFFFFFFULL, SourceLocation(-1));
		if (name == "min") return new PrimitiveLiteralExpr(_type, 0ULL, SourceLocation(-1));
		break;
	case PrimType::u64:
		if (name == "max") return new PrimitiveLiteralExpr(_type, 0xFFFFFFFFFFFFFFFFULL, SourceLocation(-1));
		if (name == "min") return new PrimitiveLiteralExpr(_type, 0ULL, SourceLocation(-1));
		break;
	case PrimType::u128:
		if (name == "max") return new PrimitiveLiteralExpr(_type, 0ULL, SourceLocation(-1));
		if (name == "min") return new PrimitiveLiteralExpr(_type, 0ULL, SourceLocation(-1));
		break;
	case PrimType::i8:
		if (name == "max") return new PrimitiveLiteralExpr(_type, 0x7FLL, SourceLocation(-1));
		if (name == "min") return new PrimitiveLiteralExpr(_type, 0x80LL, SourceLocation(-1));
		break;
	case PrimType::i16:
		if (name == "max") return new PrimitiveLiteralExpr(_type, 0x7FFFLL, SourceLocation(-1));
		if (name == "min") return new PrimitiveLiteralExpr(_type, 0x8000LL, SourceLocation(-1));
		break;
	case PrimType::i32:
		if (name == "max") return new PrimitiveLiteralExpr(_type, 0x7FFFFFFFLL, SourceLocation(-1));
		if (name == "min") return new PrimitiveLiteralExpr(_type, 0x80000000LL, SourceLocation(-1));
		break;
	case PrimType::i64:
		if (name == "max") return new PrimitiveLiteralExpr(_type, 0x7FFFFFFFFFFFFFFFLL, SourceLocation(-1));
		if (name == "min") return new PrimitiveLiteralExpr(_type, 0x8000000000000000LL, SourceLocation(-1));
		break;
	case PrimType::i128:
		if (name == "max") return new PrimitiveLiteralExpr(_type, 0LL, SourceLocation(-1));
		if (name == "min") return new PrimitiveLiteralExpr(_type, 0LL, SourceLocation(-1));
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
		else if (isSigned(pt))
		{
			if (isUnsigned(_type) && tyWidth(_type) < tyWidth(pt))
				return ConvType::Convertible;
			else if (isSigned(_type) && tyWidth(_type) <= tyWidth(pt))
				return ConvType::Convertible;
		}
		else if (isUnsigned(pt))
		{
			if (isUnsigned(_type) && tyWidth(_type) <= tyWidth(pt))
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

	// TODO: can target construct from 'this'?

	error("file", getLine(), "Can't convert ___ to ___.");
	return nullptr;
}

std::string PrimitiveType::stringof() const
{
	return std::string("@") + primTypeNames[(size_t)type()];
}
std::string PrimitiveType::mangleof() const
{
	return primTypeMangle[(size_t)type()];
}
raw_ostream &PrimitiveType::dump(raw_ostream &out, int ind)
{
	return out << stringof() << '\n';
}

Node *PointerType::getMember(const std::string &name)
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
		if (!from->isSame(target))
			return ConvType::NoConversion;

		// validate each layer of pointer is convertible
		while (fromDepth--, toDepth--)
		{
			if (!fromStack[fromDepth]->canPromote(toStack[toDepth]->ptrType()))
				return ConvType::NoConversion;
		}
		return ConvType::Convertible;
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
		assert(expr->type()->convertible(targetType) == ConvType::Convertible);
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

std::string PointerType::stringof() const
{
	return _pointerTarget->stringof() + "*";
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

size_t Struct::memberIndex(const std::string &name)
{
	size_t i = 0;
	for (; i < _dataMembers.size(); ++i)
	{
		if (_dataMembers[i].decl->name().compare(name) == 0)
			break;
	}
	return i;
}

Node *Struct::getMember(const std::string &name)
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


std::string Struct::stringof() const
{
	std::string s = "{ ";
	bool first = true;
	for (auto &m : _dataMembers)
	{
		if (!first)
			s.append(", ");
		first = false;
		s.append(m.decl->targetType()->stringof());
	}
	s.append(" }");
	return std::move(s);
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
	if (_args.length != ft->_args.length)
		return false;
	for (size_t i = 0; i < _args.length; ++i)
	{
		if (!_args[i]->isSame(ft->_args[i]))
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

std::string FunctionType::stringof() const
{
	std::string r = _returnType ? _returnType->stringof() : std::string("???");
	r += "(";
	for (size_t i = 0; i < _args.length; ++i)
		r += (i > 0 ? std::string(",") : std::string()) + _args[i]->stringof();
	r += ")";
	return r;
}
std::string FunctionType::mangleof() const { assert(false); return ""; }

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
	for (auto a : _args)
		a->dump(indent(out, ind + 1), ind + 1);
	return indent(out, ind) << ")\n";
}


//***********************
//** Declaration nodes **
//***********************

raw_ostream &TypeDecl::dump(raw_ostream &out, int ind)
{
	Declaration::dump(out << "type: " << _name, ind) << "\n";
	++ind;
	if (_type)
		_type->dump(indent(out, ind) << "as: ", ind + 1);
	return out;
}

raw_ostream &ValDecl::dump(raw_ostream &out, int ind)
{
	Declaration::dump(out << "def: #" << _name, ind) << "\n";
	++ind;
	if (_type)
		_type->dump(indent(out, ind) << "type: ", ind + 1);
	if (_value)
		_value->dump(indent(out, ind) << "val: ", ind + 1);
	return out;
}

raw_ostream &VarDecl::dump(raw_ostream &out, int ind)
{
	Declaration::dump(out << "var: #" << _name, ind) << "\n";
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

raw_ostream &PrimitiveLiteralExpr::dump(raw_ostream &out, int ind)
{
	switch (_type)
	{
	case PrimType::v:
		return Expr::dump(out << "void", ind) << '\n';
	case PrimType::u1:
		return Expr::dump(out << (u ? "true" : "false"), ind) << '\n';
	case PrimType::f16:
	case PrimType::f32:
	case PrimType::f64:
	case PrimType::f128:
		return Expr::dump(out << f, ind) << '\n';
	case PrimType::u8:
	case PrimType::u16:
	case PrimType::u32:
	case PrimType::u64:
	case PrimType::u128:
		return Expr::dump(out << u, ind) << '\n';
	case PrimType::i8:
	case PrimType::i16:
	case PrimType::i32:
	case PrimType::i64:
	case PrimType::i128:
		return Expr::dump(out << i, ind) << '\n';
	case PrimType::c8:
	case PrimType::c16:
	case PrimType::c32:
		return Expr::dump(out << "'" << c << "'", ind) << '\n';
	default:
		assert(0); return out;
	}
}

raw_ostream &AggregateLiteralExpr::dump(raw_ostream &out, int ind)
{
	Expr::dump(out << "aggregate\n", ind);
	indent(out, ind) << "type: " << _type->stringof() << '\n';
	indent(out, ind) << "values: {\n";
	for (auto i : _items)
		i->dump(indent(out, ind + 1), ind + 1);
	indent(out, ind) << "}\n";
	return out;
}

Node *RefExpr::getMember(const std::string &name)
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
		indent(out, ind) << "target: #" << _target->name() << '\n';
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

Node *CallExpr::getMember(const std::string &name)
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

Node *Identifier::getMember(const std::string &name)
{
	// base class member?
	Node *r = AmbiguousExpr::getMember(name);
	if (r)
		return r;

	return resolve()->getMember(name);
}

raw_ostream &Identifier::dump(raw_ostream &out, int ind)
{
	const char *ty = _var ? "#" : (_type ? "@" : "");
	return Node::dump(out << ty << _name << "\n", ind);
}

Node *MemberLookup::getMember(const std::string &name)
{
	// base class member?
	Node *r = AmbiguousExpr::getMember(name);
	if (r)
		return r;

	return resolve()->getMember(name);
}

raw_ostream &MemberLookup::dump(raw_ostream &out, int ind)
{
	Node::dump(out << "lookup\n", ind);
	indent(out, ind) << "member: " << _member << "\n";
	_node->dump(indent(out, ind) << "node: ", ind + 1);
	return out;
}

void Tuple::analyse()
{
#define alignto(x, a) (((x) + ((a)-1)) & ~((a)-1))

	// compose the struct
	_size = 0;
	_alignment = 0;

	allExpr = true, allTypes = _elements.length > 0;
	for (auto e : _elements)
	{
		size_t size = 0;
		size_t align = 0;

		Expr *expr = dynamic_cast<Expr*>(e);
		if (expr)
		{
			size = expr->type()->size();
			align = expr->type()->alignment();

			allTypes = false;
		}
		else
		{
			TypeExpr *type = dynamic_cast<TypeExpr*>(e);
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

TypeExpr* Tuple::type()
{
	if (!_type)
	{
		assert(allExpr);
		if (!allExpr)
			return nullptr; // error?

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
	return _type;
}

Expr* Tuple::init() const
{
	if (!_init)
	{
		assert(allTypes);
		if (!allTypes)
			return nullptr; // error?

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
	return _init;
}

bool Tuple::isSame(const TypeExpr *other) const
{
	const Tuple *t = dynamic_cast<const Tuple*>(other);
	if (t == nullptr)
		return false;
	if (_elements.length != t->_elements.length)
		return false;
	for (size_t i = 0; i < _elements.length; ++i)
	{
		const TypeExpr *e1 = dynamic_cast<const TypeExpr*>(_elements[i]);
		const TypeExpr *e2 = dynamic_cast<const TypeExpr*>(t->_elements[i]);
		if (!e1 || !e2)
			return false;
		if (!e1->isSame(e2))
			return false;
	}
	return true;
}

ConvType Tuple::convertible(const TypeExpr *target) const
{
	const Tuple *t = dynamic_cast<const Tuple*>(target);
	if (t == nullptr)
		return ConvType::NoConversion;

	// TODO: tuples can convert to arrays? structs?

	if (_elements.length != t->_elements.length)
		return ConvType::NoConversion;

	ConvType conv = ConvType::Convertible;
	for (size_t i = 0; i < _elements.length; ++i)
	{
		const TypeExpr *e1 = dynamic_cast<const TypeExpr*>(_elements[i]);
		const TypeExpr *e2 = dynamic_cast<const TypeExpr*>(t->_elements[i]);
		if (!e1 || !e2)
			return ConvType::NoConversion;
		ConvType c = e1->convertible(e2);
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

	NodeList nodes = NodeList::empty();
	const Tuple *t = dynamic_cast<const Tuple*>(targetType);
	for (size_t i = 0; i < _elements.length; ++i)
	{
		Expr *expr = dynamic_cast<Expr*>(tup->_elements[i]);
		TypeExpr *ty = dynamic_cast<TypeExpr*>(t->_elements[i]);
		nodes = nodes.append(expr->makeConversion(ty, implicit));
	}
	return new Tuple(nodes, expr->getLoc());
}

Node *Tuple::getMember(const std::string &name)
{
	if (name == "length")
		return new PrimitiveLiteralExpr(SizeT_Type, _elements.length, getLoc());
	if (isType())
	{
		if (name == "init")
			return init();
	}
	if (isType() || isExpr())
	{
		// TODO: can we support these properties on mixed tuples?
		if (name == "sizeof")
			return new PrimitiveLiteralExpr(SizeT_Type, size(), getLoc());
		if (name == "alignof")
			return new PrimitiveLiteralExpr(SizeT_Type, alignment(), getLoc());
	}
	return Node::getMember(name);
}

std::string Tuple::stringof() const
{
	std::string s = "[ ";
	bool first = true;
	for (auto &e : _elements)
	{
		if (!first)
			s.append(", ");
		first = false;
		s.append(e->stringof());
	}
	s.append(" ]");
	return std::move(s);
}
raw_ostream &Tuple::dump(raw_ostream &out, int ind)
{
	out << "[\n";
	for (auto e : _elements)
		e->dump(indent(out, ind + 1), ind + 1);
	return indent(out, ind) << "]\n";
}

Node *UnknownIndex::getMember(const std::string &name)
{
	return dynamic_cast<AmbiguousExpr*>(_result)->resolve()->getMember(name);
}

raw_ostream &UnknownIndex::dump(raw_ostream &out, int ind)
{
	out << "index: " << _node->stringof() << "\n";
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
