#pragma once

#include "astvisitor.h"

class Semantic : public ASTVisitor
{
private:
	Module *module = nullptr;
	Scope *scope = nullptr;
	FunctionLiteralExpr *function = nullptr;

public:
	Semantic();

	void run(const std::string &srcFile, StatementList module);
	Module* getModule() const { return module; }

	TypeExpr* typeForUnaryExpression(UnaryOp op, Expr *expr);
	TypeExpr* typeForBinaryExpression(BinOp op, Expr *left, Expr *right);

	Expr* makeConversion(Expr *expr, TypeExpr *newType, bool implicit);

	void visit(Node &n) override;
	void visit(Statement &n) override;
	void visit(Declaration &n) override;
	void visit(Module &n) override;
	void visit(ModuleStatement &n) override;
	void visit(ExpressionStatement &n) override;
	void visit(ReturnStatement &n) override;
	void visit(IfStatement &n) override;
	void visit(LoopStatement &n) override;
	void visit(TypeExpr &n) override;
	void visit(PrimitiveType &n) override;
	void visit(TypeIdentifier &n) override;
	void visit(PointerType &v) override;
	void visit(TupleType &n) override;
	void visit(Struct &n) override;
	void visit(FunctionType &n) override;
	void visit(Expr &n) override;
	void visit(Generic &n) override;
	void visit(PrimitiveLiteralExpr &n) override;
	void visit(ArrayLiteralExpr &n) override;
	void visit(FunctionLiteralExpr &n) override;
	void visit(IdentifierExpr &n) override;
	void visit(TypeConvertExpr &n) override;
	void visit(UnaryExpr &n) override;
	void visit(BinaryExpr &n) override;
	void visit(IndexExpr &n) override;
	void visit(CallExpr &n) override;
	void visit(TypeDecl &n) override;
	void visit(ValDecl &n) override;
	void visit(VarDecl &n) override;
	void visit(PrototypeDecl &n) override;
	void visit(FunctionDecl &n) override;
};
