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

	module = new Module(pFilename, s);
	scope = module;

	for (auto s : moduleStatements)
		s->accept(*this);
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

void Semantic::visit(Scope &n)
{
}

void Semantic::visit(Module &n)
{
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
}

void Semantic::visit(ArrayLiteralExpr &n)
{
}

void Semantic::visit(FunctionLiteralExpr &n)
{
}

void Semantic::visit(VariableExprAST &n)
{
}

void Semantic::visit(UnaryExprAST &n)
{
}

void Semantic::visit(BinaryExprAST &n)
{
}

void Semantic::visit(IndexExprAST &n)
{
}

void Semantic::visit(CallExprAST &n)
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
	scope->addDecl(n.name(), &n);
}

void Semantic::visit(ValDecl &n)
{
	if (!n._type && !n._init)
		assert(false);// , "def statement needs either type or value!");

	Declaration *decl = scope->getDecl(n._name, true);
	if (decl)
	{
		// already declared!
		return;
	}

	if (!n._type)
		n._type = n._init->type();
	else if (!n.init())
		n._init = n._type->init();
	else
	{
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

	if (!n._type)
		n._type = n._init->type();
	else if (!n.init())
		n._init = n._type->init();
	else
	{
		// TODO: assert init.type -> type
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
