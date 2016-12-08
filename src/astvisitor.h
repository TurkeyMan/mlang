#pragma once

#include "ast.h"

class ASTVisitor : public gc_cleanup
{
public:
	virtual void visit(Node &n) {}
	virtual void visit(Statement &n) {}
	virtual void visit(Declaration &n) {}
	virtual void visit(::Module &n) {}
	virtual void visit(ModuleStatement &n) {}
	virtual void visit(ExpressionStatement &n) {}
	virtual void visit(ReturnStatement &n) {}
	virtual void visit(IfStatement &n) {}
	virtual void visit(LoopStatement &n) {}
	virtual void visit(TypeExpr &n) {}
	virtual void visit(AsType &n) {}
	virtual void visit(PrimitiveType &n) {}
	virtual void visit(::PointerType &n) {}
	virtual void visit(TupleType &n) {}
	virtual void visit(Struct &n) {}
	virtual void visit(::FunctionType &n) {}
	virtual void visit(Expr &n) {}
	virtual void visit(AsExpr &n) {}
	virtual void visit(Generic &n) {}
	virtual void visit(PrimitiveLiteralExpr &n) {}
	virtual void visit(AggregateLiteralExpr &n) {}
	virtual void visit(ArrayLiteralExpr &n) {}
	virtual void visit(FunctionLiteralExpr &n) {}
	virtual void visit(RefExpr &n) {}
	virtual void visit(DerefExpr &n) {}
	virtual void visit(TypeConvertExpr &n) {}
	virtual void visit(UnaryExpr &n) {}
	virtual void visit(BinaryExpr &n) {}
	virtual void visit(IndexExpr &n) {}
	virtual void visit(CallExpr &n) {}
	virtual void visit(AssignExpr &n) {}
	virtual void visit(BindExpr &n) {}
	virtual void visit(Identifier &n) {}
	virtual void visit(MemberLookup &n) {}
	virtual void visit(TypeDecl &n) {}
	virtual void visit(ValDecl &n) {}
	virtual void visit(VarDecl &n) {}
	virtual void visit(PrototypeDecl &n) {}
	virtual void visit(FunctionDecl &n) {}
};
