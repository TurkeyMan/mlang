#pragma once
#if !defined(_SEMANTIC_H)
#define _SEMANTIC_H


#include "mlang.h"
#include "astvisitor.h"

namespace m {

class Semantic : public ASTVisitor
{
private:
	Compiler &compiler;
	Module *currentModule;

	Array<Scope*> _scope;
	Array<FunctionLiteralExpr*> _function;

public:
	Semantic(Compiler &compiler)
		: compiler(compiler)
	{}

	void run();

	TypeExpr* typeForUnaryExpression(UnaryOp op, TypeExpr *expr);
	TypeExpr* typeForBinaryExpression(BinOp op, TypeExpr *left, TypeExpr *right);

	Scope* scope() const { return _scope.size() ? _scope.back() : nullptr; }
	void pushScope(Scope *s) { assert(s); _scope.push_back(s); }
	void popScope() { _scope.pop_back(); }
	const SharedString &scopeFilename() const { return scope()->module()->filename(); }

	FunctionLiteralExpr* function() const { return _function.size() ? _function.back() : nullptr; }
	void pushFunction(FunctionLiteralExpr *f) { _function.push_back(f); }
	void popFunction() { _function.pop_back(); }

private:
	void visit(Declaration &n) override;
	void visit(Namespace &n) override;
	void visit(Module &n) override;
	void visit(ExpressionStatement &n) override;
	void visit(ReturnStatement &n) override;
	void visit(ScopeStatement &n) override;
	void visit(IfStatement &n) override;
	void visit(LoopStatement &n) override;
	void visit(PrimitiveType &n) override;
	void visit(ModifiedType &v) override;
	void visit(PointerType &v) override;
	void visit(SliceType &v) override;
	void visit(Struct &n) override;
	void visit(FunctionType &n) override;
	void visit(CVarArgType &n) override;
	void visit(PrimitiveLiteralExpr &n) override;
	void visit(AggregateLiteralExpr &n) override;
	void visit(FunctionLiteralExpr &n) override;
	void visit(RefExpr &n) override;
	void visit(DerefExpr &n) override;
	void visit(TypeConvertExpr &n) override;
	void visit(UnaryExpr &n) override;
	void visit(BinaryExpr &n) override;
	void visit(CallExpr &n) override;
	void visit(AssignExpr &n) override;
	void visit(BindExpr &n) override;
	void visit(SliceExpr &n) override;
	void visit(UnknownExpr &n) override;
	void visit(Identifier &n) override;
	void visit(MemberLookup &n) override;
	void visit(Tuple &n) override;
	void visit(Index &n) override;
	void visit(NamespaceDecl &n) override;
	void visit(ModuleDecl &n) override;
	void visit(ImportDecl &n) override;
	void visit(TypeDecl &n) override;
	void visit(ValDecl &n) override;
	void visit(VarDecl &n) override;
};

}

#endif // _SEMANTIC_H
