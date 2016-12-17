#pragma once

#include "astvisitor.h"

class Semantic : public ASTVisitor
{
private:
	Module *module = nullptr;
	std::vector<Scope*> _scope;
	std::vector<FunctionLiteralExpr*> _function;

public:
	Semantic();

	void run(const std::string &srcFile, StatementList module);
	Module* getModule() const { return module; }

	TypeExpr* typeForUnaryExpression(UnaryOp op, TypeExpr *expr);
	TypeExpr* typeForBinaryExpression(BinOp op, TypeExpr *left, TypeExpr *right);

	Scope* scope() const { return _scope.size() ? _scope.back() : nullptr; }
	void pushScope(Scope *s) { _scope.push_back(s); }
	void popScope() { _scope.pop_back(); }

	FunctionLiteralExpr* function() const { return _function.size() ? _function.back() : nullptr; }
	void pushFunction(FunctionLiteralExpr *f) { _function.push_back(f); }
	void popFunction() { _function.pop_back(); }

	void visit(Declaration &n) override;
	void visit(Module &n) override;
	void visit(ModuleStatement &n) override;
	void visit(ExpressionStatement &n) override;
	void visit(ReturnStatement &n) override;
	void visit(ScopeStatement &n) override;
	void visit(IfStatement &n) override;
	void visit(LoopStatement &n) override;
	void visit(PrimitiveType &n) override;
	void visit(PointerType &v) override;
	void visit(Struct &n) override;
	void visit(FunctionType &n) override;
	void visit(PrimitiveLiteralExpr &n) override;
	void visit(AggregateLiteralExpr &n) override;
	void visit(ArrayLiteralExpr &n) override;
	void visit(FunctionLiteralExpr &n) override;
	void visit(RefExpr &n) override;
	void visit(DerefExpr &n) override;
	void visit(TypeConvertExpr &n) override;
	void visit(UnaryExpr &n) override;
	void visit(BinaryExpr &n) override;
	void visit(CallExpr &n) override;
	void visit(AssignExpr &n) override;
	void visit(BindExpr &n) override;
	void visit(Identifier &n) override;
	void visit(MemberLookup &n) override;
	void visit(Tuple &n) override;
	void visit(UnknownIndex &n) override;
	void visit(TypeDecl &n) override;
	void visit(ValDecl &n) override;
	void visit(VarDecl &n) override;
	void visit(PrototypeDecl &n) override;
	void visit(FunctionDecl &n) override;
};
