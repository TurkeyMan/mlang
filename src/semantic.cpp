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
			return new PrimitiveType(PrimType::u1);
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
		case BinOp::Eq:
		case BinOp::Ne:
		case BinOp::Gt:
		case BinOp::Ge:
		case BinOp::Lt:
		case BinOp::Le:
		case BinOp::Is:
		case BinOp::IsNot:
			return new PrimitiveType(PrimType::u1);
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
		case BinOp::ASL:
		case BinOp::ASR:
		case BinOp::LSR:
			assert((!pl || !pl->isFloatingPoint()) && (!pr || pr->isIntegral()));
			return let;
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
}

void Semantic::visit(Statement &n)
{
}

void Semantic::visit(Declaration &n)
{
}

void Semantic::visit(Module &n)
{
	pushScope(n.scope());

	for (auto s : n.statements())
		s->accept(*this);

	popScope();
}

void Semantic::visit(ModuleStatement &n)
{
	module->name() = n.name();
}

void Semantic::visit(ExpressionStatement &n)
{
	n._expression->accept(*this);
}

void Semantic::visit(ReturnStatement &n)
{
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
}

void Semantic::visit(PrimitiveType &n)
{
	if (n.doneSemantic()) return;

	n._init = new PrimitiveLiteralExpr(n._type, (uint64_t)0);
	n._init->accept(*this);
}

void Semantic::visit(TypeIdentifier &n)
{
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

	n._init = new PrimitiveLiteralExpr(PrimType::uz, 0ull);
	n._init->accept(*this);
}

void Semantic::visit(TupleType &n)
{
}


void Semantic::visit(Struct &n)
{
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
}

void Semantic::visit(Expr &n)
{
}

void Semantic::visit(Generic &n)
{
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
	n._typeExpr->accept(*this);

	// todo: validate that 'value' is within 'type's precision limits
}

void Semantic::visit(ArrayLiteralExpr &n)
{
}

void Semantic::visit(FunctionLiteralExpr &n)
{
	pushScope(n.scope());
	pushFunction(&n);

	// resolve parent scope...

	// if is pure, no parent scope
	// if is method, parent is struct
	// else parent is module scope
	scope()->_parent = module->scope();

	for (auto a : n.args())
	{
		a->accept(*this);

		VarDecl *decl = dynamic_cast<VarDecl*>(a);
		assert(decl);
		n.argTypes.append(decl->dataType());
	}

	for (auto s : n.statements())
		s->accept(*this);

	popFunction();
	popScope();

	if (n.inferReturnType && !n.returnType)
		n.returnType = new PrimitiveType(PrimType::v);

	n._type = new FunctionType(n.returnType, n.argTypes);
}

void Semantic::visit(IdentifierExpr &n)
{
	if(!n._target)
		n._target = scope()->getDecl(n.getName());
}

void Semantic::visit(DerefExpr &n)
{
	n._expr->accept(*this);
}

void Semantic::visit(TypeConvertExpr &n)
{
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
	n._operand->accept(*this);

	// TODO: choose proper result type
	//       cast operant to result type
	n._type = typeForUnaryExpression(n.op(), n._operand->type());
	n._operand = n._operand->makeConversion(n._type);
}

void Semantic::visit(BinaryExpr &n)
{
	n.lhs()->accept(*this);
	n.rhs()->accept(*this);

	switch (n.op())
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
					n._lhs = n.lhs()->makeConversion(new PrimitiveType(PrimType::f64));
					n._rhs = n.rhs()->makeConversion(new PrimitiveType(PrimType::i32));
				}
				else if (isFloat(lh) && isInt(rh))
				{
					n._rhs = n.rhs()->makeConversion(new PrimitiveType(PrimType::i32));
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

	n._type = typeForBinaryExpression(n.op(), n.lhs()->type(), n.rhs()->type());
	n._lhs = n._lhs->makeConversion(n._type);
	n._rhs = n._rhs->makeConversion(n._type);
}

void Semantic::visit(IndexExpr &n)
{
}

void Semantic::visit(CallExpr &n)
{
	n._func->accept(*this);

	// get function type...
	FunctionType *funcType = nullptr;
	IdentifierExpr *ident = dynamic_cast<IdentifierExpr*>(n._func);
	if (ident)
	{
		ValDecl *decl = dynamic_cast<ValDecl*>(ident->target());
		if (decl)
			funcType = decl->type()->asFunction();
	}

	assert(funcType);

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

void Semantic::visit(IfStatement &n)
{
	if (n._initStatements.length > 0)
	{
		n._init = new Scope(scope(), &n);
		pushScope(n._init);

		for (auto s : n._initStatements)
			s->accept(*this);
	}

	n._cond->accept(*this);
	n._cond = n._cond->makeConversion(new PrimitiveType(PrimType::u1));

	if (n._thenStatements.length > 0)
		n._then = new Scope(scope(), &n);

	if (n._elseStatements.length > 0)
		n._else = new Scope(scope(), &n);

	pushScope(n._then);
	for (auto s : n._thenStatements)
		s->accept(*this);
	popScope();

	pushScope(n._else);
	for (auto s : n._elseStatements)
		s->accept(*this);
	popScope();

	if (n._initStatements.length > 0)
		popScope();
}

void Semantic::visit(LoopStatement &n)
{
	n._body = new Scope(scope(), &n);
	pushScope(n._body);

	for (auto i : n._iterators)
		i->accept(*this);

	if (n._cond)
	{
		n._cond->accept(*this);
		n._cond = n._cond->makeConversion(new PrimitiveType(PrimType::u1));
	}

	for (auto s : n._loopEntry)
		s->accept(*this);

	for (auto s : n._bodyStatements)
		s->accept(*this);

	for (auto i : n._increments)
		i->accept(*this);

	popScope();
}

void Semantic::visit(TypeDecl &n)
{
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
	Declaration *decl = scope()->getDecl(n._name, true);
	if (decl)
	{
		// already declared!
		return;
	}

	n._init->accept(*this);

	if (!n._type)
		n._type = n._init->type();
	else
	{
		n._type->accept(*this);

		// TODO: assert init.type -> type
	}

	scope()->addDecl(n._name, &n);
}

void Semantic::visit(VarDecl &n)
{
	if (!n._type && !n._init)
		assert(false);// , "var statement needs either type or init value!");

	Declaration *decl = scope()->getDecl(n._name, true);
	if (decl)
	{
		// already declared!
		return;
	}

	if (n._init)
		n._init->accept(*this);

	if (!n._type)
		n._type = new PointerType(PtrType::ImplicitPointer, n._init->type());
	else
	{
		n._type->accept(*this);

		if (!n.init())
			n._init = n._type->init();
		else
			n._init = n._init->makeConversion(n._type);

		n._type = new PointerType(PtrType::ImplicitPointer, n._type);
	}

	scope()->addDecl(n._name, &n);
}

void Semantic::visit(PrototypeDecl &n)
{
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
}
