#include "semantic.h"


Semantic::Semantic()
{
}

void Semantic::run(const std::string &srcFile, StatementList moduleStatements)
{
	// TODO: make default module name from filename without extension
	size_t dot = srcFile.find('.');
	std::string moduleName = srcFile.substr(0, dot);

	module = new Module(srcFile, moduleName, moduleStatements);
	module->accept(*this);
}

TypeExpr* Semantic::typeForUnaryExpression(UnaryOp op, TypeExpr *type)
{
	switch (op)
	{
		case UnaryOp::LogicNot:
			return PrimitiveType::get(PrimType::u1);
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
			return PrimitiveType::get(PrimType::u1);
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

void Semantic::visit(Node &n)
{
	if (n.doneSemantic()) return;
}

void Semantic::visit(Statement &n)
{
	if (n.doneSemantic()) return;
}

void Semantic::visit(Declaration &n)
{
	if (n.doneSemantic()) return;
}

void Semantic::visit(Module &n)
{
	if (n.doneSemantic()) return;

	pushScope(n.scope());

	for (auto s : n.statements())
		s->accept(*this);

	popScope();
}

void Semantic::visit(ModuleStatement &n)
{
	if (n.doneSemantic()) return;

	module->name() = n.name();
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

	if (function()->inferReturnType)
	{
		if (!function()->returnType)
			function()->returnType = expr->type();
		else
		{
			// TODO: validate expr->type() == function->returnType or error

			n._expression = expr->makeConversion(function()->returnType);
		}
	}
	else
	{
		// TODO: only do conversion if we need to!
		n._expression = expr->makeConversion(function()->returnType);
	}
}

void Semantic::visit(TypeExpr &n)
{
	if (n.doneSemantic()) return;

}

void Semantic::visit(AsType &n)
{
	if (n.doneSemantic()) return;

	n._node->accept(*this);

	n._type = n._node->type();

	assert(n._type);
}

void Semantic::visit(PrimitiveType &n)
{
	if (n.doneSemantic()) return;

	n._init = new PrimitiveLiteralExpr(n._type, (uint64_t)0);
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

	n._init = new PrimitiveLiteralExpr(SizeT_Type, 0ull);
	n._init->accept(*this);
}

void Semantic::visit(TupleType &n)
{
	if (n.doneSemantic()) return;
}


void Semantic::visit(Struct &n)
{
	if (n.doneSemantic()) return;

	n.scope()->_parent = scope();
	pushScope(n.scope());

	for (auto m : n._members)
	{
		m->accept(*this);

		// TODO: static if...

		Declaration *decl = dynamic_cast<Declaration*>(m);
		scope()->addDecl(decl->name(), decl);

		VarDecl *var = dynamic_cast<VarDecl*>(decl);
		if (var)
		{
			n._dataMembers.append(var);

			// TODO: accumulate sizeof...
		}
	}

	popScope();
}

void Semantic::visit(FunctionType &n)
{
	if (n.doneSemantic()) return;
}

void Semantic::visit(Expr &n)
{
	if (n.doneSemantic()) return;
}

void Semantic::visit(AsExpr &n)
{
	if (n.doneSemantic()) return;

	n._node->accept(*this);

	n._expr = n._node->expr();

	assert(n._expr);
}

void Semantic::visit(Generic &n)
{
	if (n.doneSemantic()) return;

	switch (n.type)
	{
	case Generic::Type::List:
		n.l->accept(*this);
		if (n.r)
			n.r->accept(*this);
		break;
	case Generic::Type::Module:
		module->name() = ((Generic*)n.l)->s;
		break;
	default:
		break;
	}
}

void Semantic::visit(PrimitiveLiteralExpr &n)
{
	if (n.doneSemantic()) return;

	n._typeExpr->accept(*this);

	// todo: validate that 'value' is within 'type's precision limits
}

void Semantic::visit(ArrayLiteralExpr &n)
{
	if (n.doneSemantic()) return;

}

void Semantic::visit(FunctionLiteralExpr &n)
{
	if (n.doneSemantic()) return;

	pushScope(n.scope());
	pushFunction(&n);

	// resolve parent scope...

	// if is pure, no parent scope
	// if is method, parent is struct
	// else parent is module scope
	scope()->_parent = module->scope();

	for (auto a : n.args())
	{
		VarDecl *decl = dynamic_cast<VarDecl*>(a);
		assert(decl);

		decl->_init = new PrimitiveLiteralExpr(PrimType::v, 0ull);
		decl->accept(*this);

		n.argTypes.append(decl->targetType());
	}

	bool bDidReturn = false;
	for (auto s : n.statements())
	{
		if (bDidReturn)
		{
			// statement unreachable warning!
		}

		s->accept(*this);

		if (s->didReturn())
			bDidReturn = true;
	}

	popFunction();
	popScope();

	if (n.inferReturnType && !n.returnType)
		n.returnType = PrimitiveType::get(PrimType::v);

	n._type = new FunctionType(n.returnType, n.argTypes);
	n._type->accept(*this);

	if (!bDidReturn && !n.type()->returnType()->isVoid())
	{
		// function did not return a value!
		assert(false);
	}
}

void Semantic::visit(RefExpr &n)
{
	if (n.doneSemantic()) return;

	if (n._target)
		n._target->accept(*this);

	if (!n._type)
	{
		n._type = n._target->type()->asPointer();
		assert(n._type);
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

	PointerType *pt = n._expr->type()->asPointer();
	while (pt)
	{
		n._expr = new DerefExpr(n._expr);
		pt = n._expr->type()->asPointer();
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
					n._lhs = n.lhs()->makeConversion(PrimitiveType::get(PrimType::f64));
					n._rhs = n.rhs()->makeConversion(PrimitiveType::get(PrimType::i32));
				}
				else if (isFloat(lh) && isInt(rh))
				{
					n._rhs = n.rhs()->makeConversion(PrimitiveType::get(PrimType::i32));
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
			n._type = PrimitiveType::get(PrimType::u1);
			break;
		default:
			n._type = type;
			break;
	}

	// do type conversion for operands
	n._lhs = n._lhs->makeConversion(type);
	n._rhs = n._rhs->makeConversion(type);
}

void Semantic::visit(IndexExpr &n)
{
	if (n.doneSemantic()) return;

}

void Semantic::visit(CallExpr &n)
{
	if (n.doneSemantic()) return;

	n._func->accept(*this);

	FunctionType *funcType = n._func->type()->asFunction();
	while (!funcType)
	{
		::PointerType *ptr = n._func->type()->asPointer();

		if (!ptr)
			assert(false); // not call-able!

		n._func = new DerefExpr(n._func);
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
		n._target = new DerefExpr(n._target);
		n._target->accept(*this);
		ptr = n._target->type()->asPointer();
	}

	if (n._op != BinOp::None)
	{
		// if call opOpAssign()
		// opOpAssign(_target, _expr);
		// return;

		// calculate rh operand
		n._expr = new BinaryExpr(n._op, n._target, n._expr);
		n._expr->accept(*this);
	}

	n._expr = n._expr->makeConversion(n._target->type()->evalType());
}

void Semantic::visit(BindExpr &n)
{
	if (n.doneSemantic()) return;

	assert(false);
}

void Semantic::visit(Identifier &n)
{
	if (n.doneSemantic()) return;

	n._target = scope()->getDecl(n.getName());

	ValDecl *pVal = dynamic_cast<ValDecl*>(n._target);
	if (pVal)
	{
		n._eval = pVal->value();
	}
	else
	{
		TypeDecl *pType = dynamic_cast<TypeDecl*>(n._target);
		if (pType)
			n._eval = pType->type();
		else
			assert(false);
	}
}

void Semantic::visit(MemberLookup &n)
{
	if (n.doneSemantic()) return;

	n._node->accept(*this);

	// lookup member...
	Expr *expr = dynamic_cast<Expr*>(n._node);
	if (expr)
	{
		n._eval = dynamic_cast<Expr*>(expr->getMember(n._member));

		if (!n._eval)
		{
			// UFCS lookup
			// find _member(typeof(_expr) arg, ...) function.
		}
	}
	else
	{
		TypeExpr *type = dynamic_cast<TypeExpr*>(n._node);
		if (type)
			n._eval = dynamic_cast<Expr*>(type->getMember(n._member, nullptr));
	}

	assert(n._eval);

	n._eval->accept(*this);
}


void Semantic::visit(IfStatement &n)
{
	if (n.doneSemantic()) return;

	if (n._initStatements.length > 0)
	{
		n._init = new Scope(scope(), &n);
		pushScope(n._init);

		for (auto s : n._initStatements)
			s->accept(*this);
	}

	n._cond->accept(*this);
	n._cond = n._cond->makeConversion(PrimitiveType::get(PrimType::u1));

	if (n._thenStatements.length > 0)
		n._then = new Scope(scope(), &n);

	if (n._elseStatements.length > 0)
		n._else = new Scope(scope(), &n);

	pushScope(n._then);
	for (auto s : n._thenStatements)
	{
		if (n._thenReturned)
		{
			// statement unreachable warning!
		}

		s->accept(*this);

		if (s->didReturn())
			n._thenReturned = true;
	}
	popScope();

	pushScope(n._else);
	for (auto s : n._elseStatements)
	{
		if (n._elseReturned)
		{
			// statement unreachable warning!
		}

		s->accept(*this);

		if (s->didReturn())
			n._elseReturned = true;
	}
	popScope();

	if (n._initStatements.length > 0)
		popScope();
}

void Semantic::visit(LoopStatement &n)
{
	if (n.doneSemantic()) return;

	n._body = new Scope(scope(), &n);
	pushScope(n._body);

	for (auto i : n._iterators)
		i->accept(*this);

	if (n._cond)
	{
		n._cond->accept(*this);
		n._cond = n._cond->makeConversion(PrimitiveType::get(PrimType::u1));
	}

	for (auto s : n._loopEntry)
		s->accept(*this);

	for (auto s : n._bodyStatements)
	{
		s->accept(*this);

		if (s->didReturn())
		{
			// warn: unreachable statements!
			// returning from loop should happen in an if/else??
		}
	}

	for (auto i : n._increments)
		i->accept(*this);

	popScope();
}

void Semantic::visit(TypeDecl &n)
{
	if (n.doneSemantic()) return;

	Declaration *decl = scope()->getDecl(n.name(), true);
	if (decl)
	{
		// already declared!
		return;
	}

	n._type->accept(*this);

	scope()->addDecl(n.name(), &n);
}

void Semantic::visit(ValDecl &n)
{
	if (n.doneSemantic()) return;

	Declaration *decl = scope()->getDecl(n._name, true);
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

		n._value = n._value->makeConversion(n._type);
	}

	scope()->addDecl(n._name, &n);
}

void Semantic::visit(VarDecl &n)
{
	if (n.doneSemantic()) return;

	if (!n._valType && !n._init)
		assert(false);// , "var statement needs either type or init value!");

	Declaration *decl = scope()->getDecl(n._name, true);
	if (decl)
	{
		// already declared!
		return;
	}

	if (n._init)
		n._init->accept(*this);
	if (n._valType)
	{
		n._valType->accept(*this);

		if (n._init)
		{
			PrimitiveType *pt = n._init->type()->asPrimitive();
			if(!pt || pt->type() != PrimType::v)
				n._init = n._init->makeConversion(n._valType);
		}
	}

	if (!n._init)
		n._init = n._valType->init();
	if (!n._valType)
		n._valType = n._init->type();

	n._type = new PointerType(PtrType::LValue, n._valType);
	n._type->accept(*this);

	n._value = new RefExpr(&n);
	n._value->accept(*this);

	scope()->addDecl(n._name, &n);
}

void Semantic::visit(PrototypeDecl &n)
{
	if (n.doneSemantic()) return;

	Declaration *decl = scope()->getDecl(n.name(), true);
	if (decl)
	{
		// error!
		return;
	}
	scope()->addDecl(n.name(), &n);
}

void Semantic::visit(FunctionDecl &n)
{
	if (n.doneSemantic()) return;

}
