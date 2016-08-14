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


class LLVMGenerator;

struct LLVMData
{
	llvm::Value* value = nullptr;
	llvm::Type* type = nullptr;

	~LLVMData()
	{
		if (value)
			delete value;
//		if (type)
//			delete type; // TODO: dunno how to clean up llvm::Types? O_o
	}
};

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
//	std::map<std::string, AllocaInst *> NamedValues;
//	std::map<std::string, std::unique_ptr<PrototypeDecl>> FunctionProtos;

	DebugInfo KSDbgInfo;
	IRBuilder<> Builder;
	std::unique_ptr<DIBuilder> DBuilder;

	::Module *module;
	Scope *scope = nullptr;

//	Function *getFunction(std::string Name);

	DISubroutineType *createFunctionType(unsigned NumArgs, DIFile *Unit);

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
	LLVMGenerator(::Module *module);

	std::string codegen(Mode mode, std::string outFile, std::string irFile);

	void visit(Node &n) override;
	void visit(Statement &n) override;
	void visit(Declaration &n) override;
	void visit(::Module &n) override;
	void visit(ModuleStatement &n) override;
	void visit(ExpressionStatement &n) override;
	void visit(ReturnStatement &n) override;
	void visit(IfStatement &n) override;
	void visit(LoopStatement &n) override;
	void visit(TypeExpr &n) override;
	void visit(PrimitiveType &n) override;
	void visit(TypeIdentifier &v) override;
	void visit(::PointerType &v) override;
	void visit(TupleType &v) override;
	void visit(Struct &n) override;
	void visit(::FunctionType &v) override;
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
