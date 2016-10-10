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
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/CodeGen/CommandFlags.h"

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

	std::vector<Scope*> _scope;
	std::vector<FunctionState> _functionStack;

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

	void codegen(Mode mode, int opt, std::string outFile, std::string irFile);

	Scope* scope() const { return _scope.size() ? _scope.back() : nullptr; }
	void pushScope(Scope *s) { _scope.push_back(s); }
	void popScope() { _scope.pop_back(); }

	void pushFunction(FunctionLiteralExpr *f)
	{
		TypeExpr *rt = f->type()->returnType();
		_functionStack.push_back(FunctionState{ f, rt->isVoid(), false, _scope.size(), nullptr });
		FunctionState &cur = _functionStack.back();
		if (!cur.isVoid)
			cur.retval = Builder.CreateAlloca(rt->cgData<LLVMData>()->type, nullptr, "retval");
	}
	void popFunction() { _functionStack.pop_back(); }
	void earlyReturn(Expr *expr)
	{
		FunctionState &cur = _functionStack.back();

		if (!cur.hasEarlyReturn)
		{
			// make a return block
			cur.returnBlock = BasicBlock::Create(ctx, "return", nullptr);

			BasicBlock *oldBlock = Builder.GetInsertBlock();
			Builder.SetInsertPoint(cur.returnBlock);

			if (expr)
			{
				LoadInst *load = Builder.CreateLoad(cur.retval);
				Builder.CreateRet(load);
			}
			else
				Builder.CreateRetVoid();

			Builder.SetInsertPoint(oldBlock);

			cur.hasEarlyReturn = true;
		}

		if (expr)
			Builder.CreateStore(expr->cgData<LLVMData>()->value, cur.retval);

		Builder.CreateBr(cur.returnBlock);
	}

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
	void visit(AsType &n) override;
	void visit(PrimitiveType &n) override;
	void visit(::PointerType &v) override;
	void visit(TupleType &v) override;
	void visit(Struct &n) override;
	void visit(::FunctionType &v) override;
	void visit(Expr &n) override;
	void visit(AsExpr &n) override;
	void visit(PrimitiveLiteralExpr &n) override;
	void visit(AggregateLiteralExpr &n) override;
	void visit(ArrayLiteralExpr &n) override;
	void visit(FunctionLiteralExpr &n) override;
	void visit(RefExpr &n) override;
	void visit(DerefExpr &n) override;
	void visit(TypeConvertExpr &n) override;
	void visit(UnaryExpr &n) override;
	void visit(BinaryExpr &n) override;
	void visit(IndexExpr &n) override;
	void visit(CallExpr &n) override;
	void visit(AssignExpr &n) override;
	void visit(BindExpr &n) override;
	void visit(Identifier &n) override;
	void visit(MemberLookup &n) override;
	void visit(TypeDecl &n) override;
	void visit(ValDecl &n) override;
	void visit(VarDecl &n) override;
	void visit(PrototypeDecl &n) override;
	void visit(FunctionDecl &n) override;

	void visit(Generic &n) override;
};
