#pragma once

#include "ast.h"

class ASTVisitor
{
public:
	virtual void visit(Node &n) {}
	virtual void visit(Statement &n) {}
	virtual void visit(Declaration &n) {}
	virtual void visit(ModuleStatement &n) {}
	virtual void visit(Scope &n) {}
	virtual void visit(Module &n) {}
	virtual void visit(TypeExpr &n) {}
	virtual void visit(PrimitiveType &n) {}
	virtual void visit(TypeIdentifier &n) {}
	virtual void visit(TupleType &n) {}
	virtual void visit(Struct &n) {}
	virtual void visit(FunctionType &n) {}
	virtual void visit(Expr &n) {}
	virtual void visit(Generic &n) {}
	virtual void visit(PrimitiveLiteralExpr &n) {}
	virtual void visit(ArrayLiteralExpr &n) {}
	virtual void visit(FunctionLiteralExpr &n) {}
	virtual void visit(VariableExprAST &n) {}
	virtual void visit(UnaryExprAST &n) {}
	virtual void visit(BinaryExprAST &n) {}
	virtual void visit(IndexExprAST &n) {}
	virtual void visit(CallExprAST &n) {}
	virtual void visit(IfExprAST &n) {}
	virtual void visit(ForExprAST &n) {}
	virtual void visit(TypeDecl &n) {}
	virtual void visit(ValDecl &n) {}
	virtual void visit(VarDecl &n) {}
	virtual void visit(PrototypeDecl &n) {}
	virtual void visit(FunctionDecl &n) {}
};
