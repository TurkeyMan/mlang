#pragma once

#include "astvisitor.h"

class Semantic : public ASTVisitor
{
private:
	Module *module = nullptr;
	Scope *scope = nullptr;

public:
	Semantic();

	void run(const char *pFilename, StatementList module);
	Module* getModule() const { return module; }

	void visit(Node &n) override;
	void visit(Statement &n) override;
	void visit(Declaration &n) override;
	void visit(Scope &n) override;
	void visit(Module &n) override;
	void visit(TypeExpr &n) override;
	void visit(PrimitiveType &n) override;
	void visit(TypeIdentifier &n) override;
	void visit(TupleType &n) override;
	void visit(Struct &n) override;
	void visit(FunctionType &n) override;
	void visit(Expr &n) override;
	void visit(Generic &n) override;
	void visit(PrimitiveLiteralExpr &n) override;
	void visit(ArrayLiteralExpr &n) override;
	void visit(FunctionLiteralExpr &n) override;
	void visit(VariableExprAST &n) override;
	void visit(UnaryExprAST &n) override;
	void visit(BinaryExprAST &n) override;
	void visit(IndexExprAST &n) override;
	void visit(CallExprAST &n) override;
	void visit(IfExprAST &n) override;
	void visit(ForExprAST &n) override;
	void visit(TypeDecl &n) override;
	void visit(ValDecl &n) override;
	void visit(VarDecl &n) override;
	void visit(PrototypeDecl &n) override;
	void visit(FunctionDecl &n) override;
};
