#include "semantic.h"


Semantic::Semantic()
{
}

void Semantic::run(const char *pFilename, Node *pParseTree)
{
	// TODO: make default module name from filename without extension
	module = new Module(pFilename, pFilename);
	scope = module;

	pParseTree->accept(*this);
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

void Semantic::visit(Struct &n)
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

void Semantic::visit(ArrayLiteralExprAST &n)
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

void Semantic::visit(VarDecl &n)
{
	Declaration *decl = scope->getDecl(n.name(), true);
	if (decl)
	{
		// already declared!
		return;
	}
	TypeExpr *t = n.type();
	Expr *i = n.init();
	if (!t && !i)
	{
		// error, can not deduce type with no initialisation!
		return;
	}

	// TODO: ASSIGN missing info...
	if(!t)
	{
		// type if unknown, deduce type...
		i->type();
	}
	if (!i)
	{
		// init value not given, use types default
		t->init();
	}
	scope->addDecl(n.name(), &n);
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

