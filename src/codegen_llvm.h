#pragma once

#include "common_llvm.h"

#include "llvm/ADT/STLExtras.h"
#include "llvm/Analysis/BasicAliasAnalysis.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"

#include "../KaleidoscopeJIT.h"

#include "astvisitor.h"

using namespace llvm;


void Codegen(const char *pFilename, Node *pAST);

class LLVMGenerator;

class DebugInfo
{
public:
	DICompileUnit *TheCU;
	DIType *DblTy;
	std::vector<DIScope *> LexicalBlocks;

	void emitLocation(LLVMGenerator &g, Expr *AST);
	DIType *getDoubleTy(LLVMGenerator &g);
};

class LLVMGenerator : public ASTVisitor
{
	friend class DebugInfo; // HACK
private:
	LLVMContext &ctx;

	std::unique_ptr<orc::KaleidoscopeJIT> TheJIT;
	std::unique_ptr<llvm::Module> TheModule;
	std::map<std::string, AllocaInst *> NamedValues;
	std::map<std::string, std::unique_ptr<PrototypeDecl>> FunctionProtos;

	DebugInfo KSDbgInfo;
	IRBuilder<> Builder;
	std::unique_ptr<DIBuilder> DBuilder;

	Scope *scope = nullptr;

	Function *getFunction(std::string Name);

	DISubroutineType *createFunctionType(unsigned NumArgs, DIFile *Unit);

	Type *t_type;
	Value *t_value;

	template <typename T>
	Type* visitType(T *n)
	{
		n->accept(*this);
		return t_type;
	}
	template <typename T>
	Value* visitValue(T *n)
	{
		n->accept(*this);
		return t_value;
	}

public:
	LLVMGenerator(const std::string& fileName, const std::string& moduleName);

	void codegen();

	void visit(Node &n) override;
	void visit(Statement &n) override;
	void visit(Declaration &n) override;
	void visit(Scope &n) override;
	void visit(::Module &n) override;
	void visit(TypeExpr &n) override;
	void visit(PrimitiveType &n) override;
	void visit(Struct &n) override;
	void visit(Expr &n) override;
	void visit(Generic &n) override;
	void visit(PrimitiveLiteralExpr &n) override;
	void visit(ArrayLiteralExprAST &n) override;
	void visit(VariableExprAST &n) override;
	void visit(UnaryExprAST &n) override;
	void visit(BinaryExprAST &n) override;
	void visit(IndexExprAST &n) override;
	void visit(CallExprAST &n) override;
	void visit(IfExprAST &n) override;
	void visit(ForExprAST &n) override;
	void visit(TypeDecl &n) override;
	void visit(VarDecl &n) override;
	void visit(PrototypeDecl &n) override;
	void visit(FunctionDecl &n) override;
};
