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

void Semantic::visit(ReturnStatement &n)
{
	if (n.expression())
		n.expression()->accept(*this);
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

	// resolve parent scope...

	// if is pure, no parent scope
	// if is method, parent is struct
	// else parent is module scope
	scope->_parent = module->scope();

	for (auto a : n.args())
		a->accept(*this);
	for (auto s : n.statements())
		s->accept(*this);

	scope = old;
}

void Semantic::visit(IdentifierExpr &n)
{
	if(!n._target)
		n._target = scope->getDecl(n.getName());
}

void Semantic::visit(TypeConvertExpr &n)
{
	// TODO: validate conversion is possible...
}

void Semantic::visit(UnaryExpr &n)
{
	n.operand()->accept(*this);
}

void Semantic::visit(BinaryExpr &n)
{
	n.lhs()->accept(*this);
	n.rhs()->accept(*this);
}

void Semantic::visit(IndexExpr &n)
{
}

void Semantic::visit(CallExpr &n)
{
}

void Semantic::visit(IfExprAST &n)
{
}

void Semantic::visit(ForExprAST &n)
{
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
		{
			// TODO: assert init.type -> type
		}
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
