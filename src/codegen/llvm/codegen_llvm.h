#pragma once

#include "common_llvm.h"

#pragma warning(disable: 4146) // error C4146: unary minus operator applied to unsigned type, result still unsigned

#include "llvm/ADT/STLExtras.h"
#include "llvm/Analysis/BasicAliasAnalysis.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/CodeGen/CommandFlags.h"
#include "llvm/Support/Casting.h"

#include "KaleidoscopeJIT.h"

#include "astvisitor.h"

using namespace llvm;

namespace m {

class LLVMGenerator;

struct LLVMData
{
	union {
		llvm::Value *value = nullptr;
		llvm::Type *type;
	};
	union {
		llvm::Metadata *divalue = nullptr;
		llvm::DIType *ditype;
	};
	llvm::DIScope *discope = nullptr;

	DIFile* file() const { return discope->getFile(); }

	~LLVMData()
	{
//		if (value)
//			delete value;
//		if (type)
//			delete type; // TODO: dunno how to clean up llvm::Types? O_o
//		if (scope)
//			delete scope;
	}
};

struct FunctionState
{
	FunctionLiteralExpr *func;
	bool isVoid;
	bool hasEarlyReturn;
	size_t scopeDepth;
	AllocaInst *retval;
	BasicBlock *returnBlock;
};

class LLVMGenerator : public ASTVisitor
{
private:
	Compiler &compiler;

	Module *module;

	Array<Scope*> _scope;
	Array<FunctionState> _functionStack;

	LLVMContext &ctx;

	std::unique_ptr<orc::KaleidoscopeJIT> TheJIT;
	std::unique_ptr<llvm::Module> TheModule;
//	std::map<std::string, AllocaInst *> NamedValues;
//	std::map<std::string, std::unique_ptr<PrototypeDecl>> FunctionProtos;

	IRBuilder<> Builder;

	std::unique_ptr<DIBuilder> DBuilder;
	DICompileUnit* DCU;
//	std::vector<DIScope *> LexicalBlocks;

	void emitLocation(Node *node);

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
	LLVMGenerator(Compiler &compiler);

	void codegen();

	Scope* scope() const { return _scope.size() ? _scope.back() : nullptr; }
	LLVMData* scopeCg() const { return scope()->cgData<LLVMData>(); }

	void pushScope(Scope *s) { _scope.push_back(s); }
	void popScope() { _scope.pop_back(); }

	void pushFunction(FunctionLiteralExpr *f);
	void popFunction() { _functionStack.pop_back(); }
	void earlyReturn(Expr *expr);

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
	void visit(Struct &n) override;
	void visit(FunctionType &v) override;
	void visit(CVarArgType &v) override;
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
	void visit(UnknownExpr &n) override;
	void visit(Identifier &n) override;
	void visit(MemberLookup &n) override;
	void visit(Tuple &v) override;
	void visit(Index &n) override;
	void visit(NamespaceDecl &n) override;
	void visit(ModuleDecl &n) override;
	void visit(ImportDecl &n) override;
	void visit(TypeDecl &n) override;
	void visit(ValDecl &n) override;
	void visit(VarDecl &n) override;
};

}
