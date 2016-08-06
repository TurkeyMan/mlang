#include "semantic.h"


Semantic::Semantic()
{
}

void Semantic::run(const char *pFilename, StatementList moduleStatements)
{
	// TODO: make default module name from filename without extension
	const char *pDot = strchr(pFilename, '.');
	std::string s(pFilename);
	if (pDot)
		s.resize(pDot - pFilename);

	module = new Module(pFilename, s, moduleStatements);
	module->accept(*this);
}

TypeExpr* Semantic::typeForUnaryExpression(UnaryOp op, Expr *expr)
{
	switch (op)
	{
		case UnaryOp::LogicNot:
			return new PrimitiveType(PrimType::u1);
		default:
			break;
	}
	return expr->type();
}
TypeExpr* Semantic::typeForBinaryExpression(BinOp op, Expr *left, Expr *right)
{
	TypeExpr *lt = left->type();
	TypeExpr *rt = right->type();

	PrimitiveType *pl = lt->asPrimitive();
	PrimitiveType *pr = rt->asPrimitive();

	if (!pl || !pr)
	{
		// this gets hard...
	}

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
			return new PrimitiveType(PrimType::u1);
		case BinOp::BitAnd:
		case BinOp::BitOr:
		case BinOp::BitXor:
			assert((!pl || !pl->isFloatingPoint()) && (!pr || !pr->isFloatingPoint()));
			if (pl && pr)
			{
				// type is biggest integer arg
				return typeWidth[(int)pl->type()] > typeWidth[(int)pr->type()] ? lt : rt;
			}
			else
				assert(false); // dunno...
			break;
		case BinOp::ASL:
		case BinOp::ASR:
		case BinOp::LSR:
			assert((!pl || !pl->isFloatingPoint()) && (!pr || pr->isIntegral()));
			return lt;
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
							return (int)pl->type() > (int)pr->type() ? lt : rt;
						return lt;
					}
					else
						return rt;
				}
				else
				{
					// TODO: this logic needs a lot of work!
					return typeWidth[(int)pl->type()] > typeWidth[(int)pr->type()] ? lt : rt;
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

Expr* Semantic::makeConversion(Expr *expr, TypeExpr *newType, bool implicit = true)
{
	if (expr->type()->isSame(newType))
		return expr;
	return new TypeConvertExpr(expr, newType, implicit);
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
	Scope *old = scope;
	scope = n.scope();

	for (auto s : n.statements())
		s->accept(*this);

	scope = old;
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

	if (function->inferReturnType)
	{
		if (!function->returnType)
			function->returnType = expr->type();
		else
		{
			// TODO: validate expr->type() == function->returnType or error

			n._expression = makeConversion(expr, function->returnType);
		}
	}
	else
	{
		// TODO: only do conversion if we need to!
		n._expression = makeConversion(expr, function->returnType);
	}
}

void Semantic::visit(TypeExpr &n)
{
}

void Semantic::visit(PrimitiveType &n)
{
}

void Semantic::visit(TypeIdentifier &n)
{
}

void Semantic::visit(TupleType &n)
{
}

void Semantic::visit(Struct &n)
{
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
	// todo: validate that 'value' is within 'type's precision limits
}

void Semantic::visit(ArrayLiteralExpr &n)
{
}

void Semantic::visit(FunctionLiteralExpr &n)
{
	Scope *old = scope;
	scope = n.scope();
	FunctionLiteralExpr *oldfn = function;
	function = &n;

	// resolve parent scope...

	// if is pure, no parent scope
	// if is method, parent is struct
	// else parent is module scope
	scope->_parent = module->scope();

	for (auto a : n.args())
	{
		a->accept(*this);

		VarDecl *decl = dynamic_cast<VarDecl*>(a);
		assert(decl);
		n.argTypes.append(decl->type());
	}

	for (auto s : n.statements())
		s->accept(*this);

	function = oldfn;
	scope = old;

	if (n.inferReturnType && !n.returnType)
		n.returnType = new PrimitiveType(PrimType::v);

	n._type = new FunctionType(n.returnType, n.argTypes);
}

void Semantic::visit(IdentifierExpr &n)
{
	if(!n._target)
		n._target = scope->getDecl(n.getName());
}

void Semantic::visit(TypeConvertExpr &n)
{
	n._expr->accept(*this);
	n._newType->accept(*this);

	// TODO: validate conversion is possible...
}

void Semantic::visit(UnaryExpr &n)
{
	n.operand()->accept(*this);

	// TODO: choose proper result type
	//       cast operant to result type
	n._type = typeForUnaryExpression(n.op(), n.operand());
}

void Semantic::visit(BinaryExpr &n)
{
	n.lhs()->accept(*this);
	n.rhs()->accept(*this);

	switch (n.op())
	{
		case BinOp::Pow:
		{
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
					n._lhs = makeConversion(n.lhs(), new PrimitiveType(PrimType::f64));
					n._rhs = makeConversion(n.rhs(), new PrimitiveType(PrimType::i32));
				}
				else if (isFloat(lh) && isInt(rh))
				{
					n._rhs = makeConversion(n.rhs(), new PrimitiveType(PrimType::i32));
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

	n._type = typeForBinaryExpression(n.op(), n.lhs(), n.rhs());
	n._lhs = makeConversion(n._lhs, n._type);
	n._rhs = makeConversion(n._rhs, n._type);
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
			n._callArgs[i] = makeConversion(n._callArgs[i], args[i]);
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
	Scope *old = scope;

	if (n._initStatements.length > 0)
	{
		n._init = new Scope(scope, &n);
		scope = n._init;

		for (auto s : n._initStatements)
			s->accept(*this);
	}

	n._cond->accept(*this);
	n._cond = makeConversion(n._cond, new PrimitiveType(PrimType::u1));

	if (n._thenStatements.length > 0)
		n._then = new Scope(scope, &n);

	if (n._elseStatements.length > 0)
		n._else = new Scope(scope, &n);

	scope = n._then;
	for (auto s : n._thenStatements)
		s->accept(*this);

	scope = n._else;
	for (auto s : n._elseStatements)
		s->accept(*this);

	scope = old;
}

void Semantic::visit(LoopStatement &n)
{
	n._body = new Scope(scope, &n);

	Scope *old = scope;
	scope = n._body;

	for (auto i : n._iterators)
		i->accept(*this);

	if (n._cond)
	{
		n._cond->accept(*this);
		n._cond = makeConversion(n._cond, new PrimitiveType(PrimType::u1));
	}

	for (auto s : n._loopEntry)
		s->accept(*this);

	for (auto s : n._bodyStatements)
		s->accept(*this);

	for (auto i : n._increments)
		i->accept(*this);

	scope = old;
}

void Semantic::visit(TypeDecl &n)
{
	Declaration *decl = scope->getDecl(n.name(), true);
	if (decl)
	{
		// already declared!
		return;
	}

	n._type->accept(*this);

	scope->addDecl(n.name(), &n);
}

void Semantic::visit(ValDecl &n)
{
	Declaration *decl = scope->getDecl(n._name, true);
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

	scope->addDecl(n._name, &n);
}

void Semantic::visit(VarDecl &n)
{
	if (!n._type && !n._init)
		assert(false);// , "var statement needs either type or init value!");

	Declaration *decl = scope->getDecl(n._name, true);
	if (decl)
	{
		// already declared!
		return;
	}

	if (n._init)
		n._init->accept(*this);

	if (!n._type)
		n._type = n._init->type();
	else
	{
		n._type->accept(*this);

		if (!n.init())
			n._init = n._type->init();
		else
			n._init = makeConversion(n._init, n._type);
	}

	scope->addDecl(n._name, &n);
}

void Semantic::visit(PrototypeDecl &n)
{
	Declaration *decl = scope->getDecl(n.name(), true);
	if (decl)
	{
		// error!
		return;
	}
	scope->addDecl(n.name(), &n);
}

void Semantic::visit(FunctionDecl &n)
{
}
