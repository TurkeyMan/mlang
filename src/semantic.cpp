#include "semantic.h"
#include "error.h"

namespace m {

void Semantic::run()
{
	for (auto module : compiler.modules)
	{
		currentModule = module;
		currentModule->accept(*this);
	}
}

TypeExpr* Semantic::typeForUnaryExpression(UnaryOp op, TypeExpr *type)
{
	switch (op)
	{
		case UnaryOp::LogicNot:
			return PrimitiveType::get(PrimType::u1, SourceLocation(-1));
		default:
			break;
	}
	return type->evalType();
}
TypeExpr* Semantic::typeForBinaryExpression(BinOp op, TypeExpr *left, TypeExpr *right)
{
	switch (op)
	{
		case BinOp::LogicAnd:
		case BinOp::LogicOr:
		case BinOp::LogicXor:
			return PrimitiveType::get(PrimType::u1, SourceLocation(-1));
		case BinOp::Is:
		case BinOp::IsNot:
			assert(false); // TODO: 'is' operator doesn't need matcing types on either side... maybe another node type?
	}

	TypeExpr *let = left->evalType();
	TypeExpr *ret = right->evalType();
	PrimitiveType *pl = let->asPrimitive();
	PrimitiveType *pr = ret->asPrimitive();

	if (!pl || !pr)
	{
		// this gets hard...
	}

	switch (op)
	{
		case BinOp::BitAnd:
		case BinOp::BitOr:
		case BinOp::BitXor:
			assert((!pl || !pl->isFloatingPoint()) && (!pr || !pr->isFloatingPoint()));
			if (pl && pr)
			{
				// type is biggest integer arg
				return typeWidth[(int)pl->type()] > typeWidth[(int)pr->type()] ? let : ret;
			}
			else
				assert(false); // dunno...
			break;
		case BinOp::SHL:
		case BinOp::ASR:
		case BinOp::LSR:
			assert((!pl || !pl->isFloatingPoint()) && (!pr || pr->isIntegral()));
			return let;
		case BinOp::Eq:
		case BinOp::Ne:
		case BinOp::Gt:
		case BinOp::Ge:
		case BinOp::Lt:
		case BinOp::Le:
			// TODO: these need careful promotion rules, or error on bad comparisons
			// ...but for now, just use normal promotion
		case BinOp::Add:
		case BinOp::Sub:
		case BinOp::Mul:
		case BinOp::Div:
		case BinOp::Mod:
		case BinOp::Pow:
		{
			// TODO: Sub should promote to signed with 2 unsigned args

			if (pl && pr)
			{
				assert(!pl->isVoid() && !pr->isVoid());
				if (pl->isFloatingPoint() || pr->isFloatingPoint())
				{
					// TODO: emit precision loss warnings!
					if (pl->isFloatingPoint())
					{
						if (pr->isFloatingPoint())
							return (int)pl->type() > (int)pr->type() ? let : ret;
						return let;
					}
					else
						return ret;
				}
				else
				{
					// TODO: this logic needs a lot of work!
					return typeWidth[(int)pl->type()] > typeWidth[(int)pr->type()] ? let : ret;
				}
			}
			break;
		}
		case BinOp::Cat:
			assert(false);
		default:
			assert(false); // what op?
	}
	return nullptr; // dunno?!
}

void Semantic::visit(Declaration &n)
{
	for (auto attr : n._attributes)
		attr->accept(*this);
}

void Semantic::visit(Module &n)
{
	if (n.doneSemantic()) return;

	n._module = &n;

	n._declaration->accept(*this);

	pushScope(&n);

	for (auto s : n.statements())
		s->accept(*this);

	popScope();
}

void Semantic::visit(ExpressionStatement &n)
{
	if (n.doneSemantic()) return;

	n._expression->accept(*this);
}

void Semantic::visit(ReturnStatement &n)
{
	if (n.doneSemantic()) return;

	Expr *expr = n.expression();
	if (expr)
		expr->accept(*this);

	// if we're infering the function return type, we should take note now
	if (function()->inferReturnType)
	{
		// if this is the first return statement we've encountered, take this as the return type
		if (!function()->returnType)
		{
			if (expr)
				function()->returnType = expr->type();
			else
				function()->returnType = PrimitiveType::get(PrimType::v, n.getLoc());
		}
		else
		{
			// TODO: validate expr->type() == function->returnType or error

			n._expression = expr->makeConversion(function()->returnType);
			n._expression->accept(*this);
		}
	}
	else
	{
		n._expression = expr->makeConversion(function()->returnType);
		n._expression->accept(*this);
	}
}

void Semantic::visit(PrimitiveType &n)
{
	if (n.doneSemantic()) return;

	n._init = new PrimitiveLiteralExpr(n._type, (uint64_t)0, n.getLoc());
	n._init->accept(*this);
}

void Semantic::visit(PointerType &n)
{
	if (n.doneSemantic()) return;

	TypeExpr *type = n.targetType();
	if (!type)
	{
		// TODO: deduce pointer target type...?
		assert(0);
	}
	type->accept(*this);

	n._init = new PrimitiveLiteralExpr(SizeT_Type, 0ull, n.getLoc());
	n._init->accept(*this);
}

void Semantic::visit(Struct &n)
{
	if (n.doneSemantic()) return;

	n._parentScope = scope();
	n._module = n._parentScope->_module;
	pushScope(&n);

#define alignto(x, a) (((x) + ((a)-1)) & ~((a)-1))

	// compose the struct
	size_t offset = 0;
	size_t structAlign = 0;
	for (auto m : n._members)
	{
		m->accept(*this);

		// TODO: static if...

		// any declarations should appear in the scope
		Declaration *decl = dynamic_cast<Declaration*>(m);
		scope()->addDecl(decl->name(), decl);

		// data members need to be arranged
		VarDecl *var = dynamic_cast<VarDecl*>(decl);
		if (var)
		{
			size_t size = var->targetType()->size();
			size_t align = var->targetType()->alignment();
			offset = alignto(offset, align);
			n._dataMembers.push_back({ offset, var });
			offset += size;
			structAlign = align > structAlign ? align : structAlign;
		}
	}

	popScope();

	n._sizeof = alignto(offset, structAlign);
	n._alignment = structAlign;

	// create init node
	ExprList members = ExprList::empty();
	for (auto &m : n._dataMembers)
	{
		Expr *expr = m.decl->init();
		members = members.append(expr);
	}

	n._init = new AggregateLiteralExpr(members, &n, n.getLoc());
}

void Semantic::visit(FunctionType &n)
{
	if (n.doneSemantic()) return;

	if (n._args.length > 0)
	{
		pushScope(nullptr);

		for (size_t i = 0; i < n._args.length; ++i)
		{
			n._args[i]->accept(*this);
			n._argTypes = n._argTypes.append(n._args[i]->type()->asPointer()->targetType());
		}

		popScope();
	}
	else
	{
		for (size_t i = 0; i < n._argTypes.length; ++i)
			n._argTypes[i]->accept(*this);
	}
}

void Semantic::visit(PrimitiveLiteralExpr &n)
{
	if (n.doneSemantic()) return;

	n._typeExpr->accept(*this);

	// todo: validate that 'value' is within 'type's precision limits
}

void Semantic::visit(AggregateLiteralExpr &n)
{
	if (n.doneSemantic()) return;

	assert(false);
}

void Semantic::visit(FunctionLiteralExpr &n)
{
	if (n.doneSemantic()) return;

	n._parentScope = scope();
	n._module = n._parentScope->_module;

	pushScope(&n);
	pushFunction(&n);

	// resolve parent scope...

	// if is pure, no parent scope
	// if is method, parent is struct
	// if is static method, parent is struct static members
	// if is local function, parent is parent function
	// else parent is module scope
	scope()->_parentScope = scope()->_module = currentModule;

	for (auto a : n.args())
	{
		VarDecl *decl = dynamic_cast<VarDecl*>(a);
		iceAssert(decl, "var declaration expected!");

		decl->_init = new PrimitiveLiteralExpr(PrimType::v, 0ull, n.getLoc());
		decl->accept(*this);

		n.argTypes = n.argTypes.append(decl->targetType());
	}

	bool didReturn = false;
	bool didWarnUnreachable = false;
	for (auto s : n.statements())
	{
		if (didReturn)
		{
			if (!didWarnUnreachable)
			{
				emitWarning(scopeFilename().c_str(), s->getLine(), "statement unreachable");
				didWarnUnreachable = true;
			}
		}

		s->accept(*this);

		if (s->didReturn())
			didReturn = true;
	}

	popFunction();
	popScope();

	if (n.inferReturnType && !n.returnType)
		n.returnType = PrimitiveType::get(PrimType::v, n.getLoc());

	n._type = new FunctionType(n.returnType, n.argTypes, n.getLoc());
	n._type->accept(*this);

	if (!didReturn && !n.type()->returnType()->isVoid())
		error(scopeFilename().c_str(), n.getLine(), "function must return a value");
}

void Semantic::visit(RefExpr &n)
{
	if (n.doneSemantic()) return;

	if (n._target)
		n._target->accept(*this);
	if (n._owner)
		n._owner->accept(*this);

	if (!n._type)
	{
		if (n._refType == RefExpr::Type::Index)
		{
			Tuple *tup = dynamic_cast<Tuple*>(n._owner->type()->asPointer()->targetType());
			if (tup->isSequence())
			{
				TypeExpr *ty = dynamic_cast<TypeExpr*>(tup->_element);
				n._type = new PointerType(PtrType::LValue, ty, n.getLoc()); // TODO: is LValue correct? should it be the same as the owner?
				n._type->accept(*this);
			}
			else
			{
				TypeExpr *ty = dynamic_cast<TypeExpr*>(tup->elements()[n._element]);
				n._type = new PointerType(PtrType::LValue, ty, n.getLoc()); // TODO: is LValue correct? should it be the same as the owner?
				n._type->accept(*this);
			}
		}
		else
		{
			n._type = n._target->type()->asPointer();
		}

		if (!n._type)
			error(scopeFilename().c_str(), n.getLine(), "Couldn't deduce type for expression %s.", n.stringof().c_str());
	}
	else
		n._type->accept(*this);
}

void Semantic::visit(DerefExpr &n)
{
	if (n.doneSemantic()) return;

	n._expr->accept(*this);
}

void Semantic::visit(TypeConvertExpr &n)
{
	if (n.doneSemantic()) return;

	n._expr->accept(*this);
	n._newType->accept(*this);

	TypeExpr *pt = n._expr->type();
	while (pt->ptrDepth() > n._newType->ptrDepth())
	{
		n._expr = new DerefExpr(n._expr, n.getLoc());
		pt = n._expr->type();
	}

	// TODO: validate conversion is possible...
}

void Semantic::visit(UnaryExpr &n)
{
	if (n.doneSemantic()) return;

	n._operand->accept(*this);

	// TODO: choose proper result type
	//       cast operant to result type
	n._type = typeForUnaryExpression(n.op(), n._operand->type());
	n._operand = n._operand->makeConversion(n._type);
	n._operand->accept(*this);
}

void Semantic::visit(BinaryExpr &n)
{
	if (n.doneSemantic()) return;

	n.lhs()->accept(*this);
	n.rhs()->accept(*this);

	switch (n._op)
	{
		case BinOp::Pow:
		{
			assert(false); // call the pow intrinsic...
			PrimitiveType *lhpt = dynamic_cast<PrimitiveType*>(n.lhs()->type());
			PrimitiveType *rhpt = dynamic_cast<PrimitiveType*>(n.rhs()->type());
			if (lhpt && rhpt)
			{
				PrimType lh = rhpt->type();
				PrimType rh = rhpt->type();
				if (isInt(lh) && isInt(rh))
				{
					// int pow
					// HACK, powi for now...
					n._lhs = n.lhs()->makeConversion(PrimitiveType::get(PrimType::f64, n.lhs()->getLoc()));
					n._lhs->accept(*this);
					n._rhs = n.rhs()->makeConversion(PrimitiveType::get(PrimType::i32, n.rhs()->getLoc()));
					n._rhs->accept(*this);
				}
				else if (isFloat(lh) && isInt(rh))
				{
					n._rhs = n.rhs()->makeConversion(PrimitiveType::get(PrimType::i32, n.rhs()->getLoc()));
					n._rhs->accept(*this);
				}
				else if (isFloat(lh) && isFloat(rh))
				{
					// int pow
					// HACK, powi for now...
				}
			}

			break;
		}
	}

	// get type for operation
	TypeExpr *type = n._type = typeForBinaryExpression(n.op(), n.lhs()->type(), n.rhs()->type());

	// get result type
	switch (n._op)
	{
		case BinOp::LogicAnd:
		case BinOp::LogicOr:
		case BinOp::LogicXor:
		case BinOp::Eq:
		case BinOp::Ne:
		case BinOp::Gt:
		case BinOp::Ge:
		case BinOp::Lt:
		case BinOp::Le:
		case BinOp::Is:
		case BinOp::IsNot:
			n._type = PrimitiveType::get(PrimType::u1, n.getLoc());
			break;
		default:
			n._type = type;
			break;
	}

	// do type conversion for operands
	n._lhs = n._lhs->makeConversion(type);
	n._lhs->accept(*this);
	n._rhs = n._rhs->makeConversion(type);
	n._rhs->accept(*this);
}

void Semantic::visit(CallExpr &n)
{
	if (n.doneSemantic()) return;

	n._func->accept(*this);

	FunctionType *funcType = n._func->type()->asFunction();
	while (!funcType)
	{
		PointerType *ptr = n._func->type()->asPointer();

		if (!ptr)
			assert(false); // not call-able!

		n._func = new DerefExpr(n._func, n.getLoc());
		n._func->accept(*this);

		funcType = n._func->type()->asFunction();
	}

	TypeExprList args = funcType->argTypes();

	for (size_t i = 0; i < args.length; ++i)
	{
		if (i < n._callArgs.length)
		{
			n._callArgs[i]->accept(*this);
			n._callArgs[i] = n._callArgs[i]->makeConversion(args[i]);
			n._callArgs[i]->accept(*this);
		}
		else
		{
			// TODO: handle default args...
			assert(false);
		}
	}
}

void Semantic::visit(AssignExpr &n)
{
	if (n.doneSemantic()) return;

	n._target->accept(*this);
	n._expr->accept(*this);

	PointerType *ptr = n._target->type()->asPointer();
	assert(ptr);

	while (ptr->ptrDepth() > 1)
	{
		n._target = new DerefExpr(n._target, n.getLoc());
		n._target->accept(*this);
		ptr = n._target->type()->asPointer();
	}

	if (n._op != BinOp::None)
	{
		// if call opOpAssign()
		// opOpAssign(_target, _expr);
		// return;

		// calculate rh operand
		n._expr = new BinaryExpr(n._op, n._target, n._expr, n.getLoc());
		n._expr->accept(*this);
	}

	n._expr = n._expr->makeConversion(n._target->type()->evalType());
	n._expr->accept(*this);
}

void Semantic::visit(BindExpr &n)
{
	if (n.doneSemantic()) return;

	assert(false);
}

void Semantic::visit(UnknownExpr &n)
{
	if (n.doneSemantic()) return;

	n._node->accept(*this);
}

void Semantic::visit(Identifier &n)
{
	if (n.doneSemantic()) return;

	Declaration *_target = scope()->getDecl(n.getName());

	if (!_target)
		error(scopeFilename().c_str(), n.getLine(), "'%s': undeclared identifier", n.getName().c_str());

	_target->accept(*this);

	n._var = dynamic_cast<ValDecl*>(_target);
	n._type = dynamic_cast<TypeDecl*>(_target);
	n._module = dynamic_cast<ModuleDecl*>(_target);
}

void Semantic::visit(MemberLookup &n)
{
	if (n.doneSemantic()) return;

	n._node->accept(*this);

	n._result = n._node->getMember(n._member);
	if (!n._result)
	{
		// UFCS lookup
		// find _member(typeof(_expr) arg, ...) function.
	}

	if (!n._result)
		error(scopeFilename().c_str(), n.getLine(), "expression '%s' has no member or property '%s'", n._node->stringof().c_str(), n._member.c_str());

	n._result->accept(*this);
}

void Semantic::visit(Tuple &n)
{
	if (n.doneSemantic()) return;

	for (auto e : n._elements)
		e->accept(*this);

	if (n._element)
		n._element->accept(*this);

	for (auto e : n._shape)
		e->accept(*this);

	n.analyse();

	if (n.isExpr())
		n.type()->accept(*this);
	if (n.isType())
		n.init()->accept(*this);

	if (n.isDynamicSize())
	{
		n._numElements = n._shape[0];
		for (size_t i = 1; i < n._shape.length; ++i)
			n._numElements = new BinaryExpr(BinOp::Mul, n._numElements, n._shape[1], n.getLoc());
		n._numElements = n._numElements->makeConversion(PrimitiveType::get(SizeT_Type, n.getLoc()), false);
		n._numElements->accept(*this);
	}
}

void Semantic::visit(Index &n)
{
	if (n.doneSemantic()) return;

	n._node->accept(*this);

	for (auto i : n._indices)
		i->accept(*this);

	Node *val = nullptr;
	AmbiguousExpr *ambig = dynamic_cast<AmbiguousExpr*>(n._node);
	if (ambig)
		val = ambig->resolve();
	else
	{
		val = dynamic_cast<Expr*>(n._node);
		if (!val)
			val = dynamic_cast<TypeExpr*>(n._node);
	}
	if (!val)
		error(scopeFilename().c_str(), n.getLine(), "can't index expression: %s", n._node->stringof().c_str());

	Tuple *tup = dynamic_cast<Tuple*>(val);
	if (tup)
	{
		if (tup->isSequence())
		{
			// validate index
			if (n._indices.length != tup->dimensions())
				error(scopeFilename().c_str(), n.getLine(), "array index has incorrect number of dimensions");
			for (size_t i = 0; i < n._indices.length; ++i)
			{
				Expr *index = n._indices[i];

//				// HACK: REMOVE ME!!! cast all indices to size_t in semantic!
//				i = i->makeConversion(PrimitiveType::get(SizeT_Type, n.getLoc()), true);
//				i->accept(*this);

				if (!index->type()->isIntegral())
					error(scopeFilename().c_str(), n.getLine(), "array index must be integral type");

				int64_t offset = index->getIntValue();
				if (offset < 0 || (ptrdiff_t)offset >= tup->numElements(i))
					error(scopeFilename().c_str(), n.getLine(), "array index out of bounds");
			}

			// get array element
			n._result = tup->seqElement();
		}
		else
		{
			// validate index
			if (n._indices.length != 1)
				error(scopeFilename().c_str(), n.getLine(), "invalid tuple index");
			if (!n._indices[0]->type()->isIntegral())
				error(scopeFilename().c_str(), n.getLine(), "tuple index must be integral type");

			int64_t i = n._indices[0]->getIntValue();
			if (i < 0 || (size_t)i >= tup->elements().length)
				error(scopeFilename().c_str(), n.getLine(), "tuple index out of bounds");

			// get tuple element
			n._result = tup->elements()[i];
		}
		return;
	}

	RefExpr *ref = dynamic_cast<RefExpr*>(val);
	if (ref)
	{
		Expr *ptr = ref;
		while (ptr->type()->ptrDepth() > 1)
		{
			ptr = new DerefExpr(ptr, n.getLoc());
			ptr->accept(*this);
		}

		// make appropriate deref...
		TypeExpr *ty = ptr->type()->asPointer()->targetType();

		if (ty->asStruct())
		{
			error(scopeFilename().c_str(), n.getLine(), "TODO: struct can provide index operator...");
		}
		else if (ty->asTuple())
		{
			Tuple *tup = ty->asTuple();

			if (tup->isSequence())
			{
				// validate index
				if (n._indices.length != tup->_shape.length)
					error(scopeFilename().c_str(), n.getLine(), "array index has incorrect number of dimensions");
				for (auto &i : n._indices)
				{
					// HACK: REMOVE ME!!! cast all indices to size_t in semantic!
					i = i->makeConversion(PrimitiveType::get(SizeT_Type, n.getLoc()), true);
					i->accept(*this);

					if (!i->type()->isIntegral())
						error(scopeFilename().c_str(), n.getLine(), "array index must be integral type");
				}

				// generate array offset
				Expr *index = n._indices[0];
				if (n._indices.length > 1)
				{
					// matrix indexing requires offset calculation...
					Expr *stride = tup->_shape[0];
					for (size_t i = 1; i < n._indices.length; ++i)
					{
						Expr *mul = new BinaryExpr(BinOp::Mul, n._indices[i], stride, n.getLoc());
						index = new BinaryExpr(BinOp::Add, index, mul, n.getLoc());
						if (i < n._indices.length - 1)
							stride = new BinaryExpr(BinOp::Mul, stride, tup->_shape[i], n.getLoc());
					}
					index->accept(*this);
				}

				// create array index...
				n._result = new RefExpr(ptr, index, n.getLoc());
				n._result->accept(*this);
			}
			else
			{
				// validate index
				if (n._indices.length != 1)
					error(scopeFilename().c_str(), n.getLine(), "expected 1-dimensional index");
				Expr *e = n._indices[0]->constEval();
				if (!e)
					error(scopeFilename().c_str(), n.getLine(), "tuple index must be constant");
				if (!e->type()->isIntegral())
					error(scopeFilename().c_str(), n.getLine(), "tuple index must be integral type");
				int64_t i = e->getIntValue();
				if (i < 0 || (size_t)i >= tup->elements().length)
					error(scopeFilename().c_str(), n.getLine(), "tuple index out of bounds");

				// create tuple ref...
				n._result = new RefExpr(ptr, i, n.getLoc());
				n._result->accept(*this);
			}
		}
		else
		{
			error(scopeFilename().c_str(), n.getLine(), "can't index expression of type: %s", ty->stringof().c_str());
		}

		return;
	}

	error(scopeFilename().c_str(), n.getLine(), "invalid index...");
}

void Semantic::visit(ScopeStatement &n)
{
	if (n.doneSemantic()) return;

	n._parentScope = scope();
	n._module = n._parentScope->_module;

	Array<VarDecl*> varStack;

	pushScope(&n);
	bool didWarnUnreachable = false;
	for (auto s : n._statements)
	{
		if (n._didReturn)
		{
			if (!didWarnUnreachable)
			{
				emitWarning(scopeFilename().c_str(), s->getLine(), "statement unreachable");
				didWarnUnreachable = true;
			}
		}

		s->accept(*this);

		if (s->didReturn())
			n._didReturn = true;

		VarDecl *decl = dynamic_cast<VarDecl*>(s);
		if (decl)
			varStack.push_back(decl);
	}

	// destruct all locals
	for (auto v : varStack)
	{
		// TODO: destruct v
	}

	popScope();
}

void Semantic::visit(IfStatement &n)
{
	if (n.doneSemantic()) return;

	Scope *s = scope();

	if (n._initStatements.length > 0)
	{
		n._parentScope = s;
		n._module = n._parentScope->_module;
		pushScope(&n);

		for (auto s : n._initStatements)
			s->accept(*this);
	}

	n._cond->accept(*this);
	n._cond = n._cond->makeConversion(PrimitiveType::get(PrimType::u1, n._cond->getLoc()));
	n._cond->accept(*this);

	if (n._then)
		n._then->accept(*this);
	if (n._else)
		n._else->accept(*this);

	if (n._initStatements.length > 0)
	{
		// TODO: destruct init block locals

		popScope();
	}
}

void Semantic::visit(LoopStatement &n)
{
	if (n.doneSemantic()) return;

	Scope *s = scope();

	if (n._iterators.length > 0)
	{
		n._parentScope = s;
		n._module = n._parentScope->_module;
		pushScope(&n);

		for (auto i : n._iterators)
			i->accept(*this);
	}

	if (n._cond)
	{
		n._cond->accept(*this);
		n._cond = n._cond->makeConversion(PrimitiveType::get(PrimType::u1, n._cond->getLoc()));
		n._cond->accept(*this);
	}

	n._body->accept(*this);
	if (n._body->didReturn())
	{
		// warn: unreachable statements!
		// returning from loop should happen in an if/else??
		emitWarning(scopeFilename().c_str(), n._body->getLine(), "statement unreachable");
	}

	for (auto i : n._increments)
		i->accept(*this);

	if (n._iterators.length > 0)
	{
		// TODO: destruct init block locals
		//...

		popScope();
	}
}

void Semantic::visit(ModuleDecl &n)
{
	if (n.doneSemantic()) return;

	n._module->accept(*this);
}

void Semantic::visit(TypeDecl &n)
{
	if (n.doneSemantic()) return;

	visit((Declaration&)n);

	Scope *s = scope();
	n._owner = s;

	Declaration *decl = s->getDecl(n.name(), true);
	if (decl)
	{
		// already declared!
		return;
	}

	n._type->accept(*this);

	s->addDecl(n.name(), &n);
}

void Semantic::visit(ValDecl &n)
{
	if (n.doneSemantic()) return;

	visit((Declaration&)n);

	Scope *s = scope();
	n._owner = s;

	Declaration *decl = s->getDecl(n._name, true);
	if (decl)
	{
		// already declared!
		return;
	}

	if (!n._value)
		assert(false);// , "def statement needs a value!");

	n._value->accept(*this);

	if (!n._type)
		n._type = n._value->type();
	else
	{
		n._type->accept(*this);

		if (!n._value->type()->isVoid())
		{
			n._value = n._value->makeConversion(n._type);
			n._value->accept(*this);
		}
	}

	// HACK: since the names are needed during codegen, we need to supply them in advance...
	if (n._value->type()->asFunction())
	{
		FunctionLiteralExpr *fn = dynamic_cast<FunctionLiteralExpr*>(n.value());
		fn->defLoc() = n.getLoc();
		fn->givenName() = n.name();
	}

	s->addDecl(n._name, &n);
}

void Semantic::visit(VarDecl &n)
{
	if (n.doneSemantic()) return;

	visit((Declaration&)n);

	if (!n._valType && !n._init)
		assert(false);// , "var statement needs either type or init value!");

	Scope *s = scope();
	n._owner = s;

	if (s)
	{
		Declaration *decl = s->getDecl(n._name, true);
		if (decl)
		{
			// already declared!
			return;
		}
	}

	if (n._init)
		n._init->accept(*this);
	if (n._valType)
	{
		n._valType->accept(*this);

		if (n._init)
		{
			PrimitiveType *pt = n._init->type()->asPrimitive();
			if (!pt || pt->type() != PrimType::v)
			{
				n._init = n._init->makeConversion(n._valType->resolveType());
				n._init->accept(*this);
			}
		}
	}

	if (!n._init)
		n._init = n._valType->init();
	if (!n._valType)
		n._valType = n._init->type();

	n._type = new PointerType(PtrType::LValue, n._valType, n.getLoc());
	n._type->accept(*this);

	if (s)
	{
		Node *owner = scope();
		if (!dynamic_cast<Struct*>(owner))
		{
			n._value = new RefExpr(&n, nullptr, n.getLoc());
			n._value->accept(*this);
		}

		scope()->addDecl(n._name, &n);
	}
}

}
