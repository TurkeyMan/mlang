#include "codegen_llvm.h"

using namespace llvm;
using namespace llvm::orc;


LLVMGenerator::LLVMGenerator(const std::string& fileName, const std::string& moduleName)
	: ctx(getGlobalContext())
	, Builder(ctx)
	, TheJIT(llvm::make_unique<KaleidoscopeJIT>())
	, TheModule(llvm::make_unique<llvm::Module>(moduleName, ctx))
{
	// Open a new module.
	TheModule->setDataLayout(TheJIT->getTargetMachine().createDataLayout());

	// Add the current debug info version into the module.
	TheModule->addModuleFlag(llvm::Module::Warning, "Debug Info Version", DEBUG_METADATA_VERSION);

	// Darwin only supports dwarf2.
	if (Triple(sys::getProcessTriple()).isOSDarwin())
		TheModule->addModuleFlag(llvm::Module::Warning, "Dwarf Version", 2);

	// Construct the DIBuilder, we do this here because we need the module.
	DBuilder = llvm::make_unique<DIBuilder>(*TheModule);

	// Create the compile unit for the module.
	KSDbgInfo.TheCU = DBuilder->createCompileUnit(dwarf::DW_LANG_C, fileName, ".", "M-Lang Compiler", 0, "", 0);
}


void Codegen(::Module *pAST)
{
	InitializeNativeTarget();
	InitializeNativeTargetAsmPrinter();
	InitializeNativeTargetAsmParser();

	LLVMGenerator generator(pAST->filename(), pAST->name());

	pAST->accept(generator);

	generator.codegen();
}

void LLVMGenerator::codegen()
{
	// Finalize the debug info.
	DBuilder->finalize();

	// Print out all of the generated code.
	TheModule->dump();
}


void LLVMGenerator::visit(Node &n)
{
}

void LLVMGenerator::visit(Statement &n)
{
}

void LLVMGenerator::visit(Declaration &n)
{
}

void LLVMGenerator::visit(Scope &n)
{
}

void LLVMGenerator::visit(::Module &n)
{
	scope = &n;

	for (auto &s : n.symbols())
		s.second->accept(*this);
}

void LLVMGenerator::visit(TypeExpr &n)
{
}

void LLVMGenerator::visit(PrimitiveType &n)
{
	switch (n.type())
	{
	case PrimType::v:
		t_type = Type::getVoidTy(ctx); break;
	case PrimType::u1:
		t_type = Type::getInt1Ty(ctx); break;
	case PrimType::i8:
	case PrimType::u8:
	case PrimType::c8:
		t_type = Type::getInt8Ty(ctx); break;
	case PrimType::i16:
	case PrimType::u16:
	case PrimType::c16:
		t_type = Type::getInt16Ty(ctx); break;
	case PrimType::i32:
	case PrimType::u32:
	case PrimType::c32:
		t_type = Type::getInt32Ty(ctx); break;
	case PrimType::i64:
	case PrimType::u64:
		t_type = Type::getInt64Ty(ctx); break;
	case PrimType::f32:
		t_type = Type::getFloatTy(ctx); break;
	case PrimType::f64:
		t_type = Type::getDoubleTy(ctx); break;
	default:
		assert(false);
	}
}

void LLVMGenerator::visit(Struct &n)
{
}

void LLVMGenerator::visit(Expr &n)
{
}

void LLVMGenerator::visit(Generic &n)
{
	switch (n.type)
	{
	case Generic::Type::List:
		n.l->accept(*this);
		if (n.r)
			n.r->accept(*this);
		break;
	case Generic::Type::Module:
		TheModule->setModuleIdentifier(((Generic*)n.l)->s);
		break;
	default:
		break;
	}
}

void LLVMGenerator::visit(PrimitiveLiteralExpr &n)
{
	switch (n.primType())
	{
	case PrimType::u1:
		t_value = ConstantInt::get(ctx, APInt(1, n.getUint(), false)); break;
	case PrimType::i8:
		t_value = ConstantInt::get(ctx, APInt(8, n.getUint(), true)); break;
	case PrimType::u8:
		t_value = ConstantInt::get(ctx, APInt(16, n.getUint(), false)); break;
	case PrimType::c8:
		t_value = ConstantInt::get(ctx, APInt(8, (uint64_t)n.getChar(), false)); break;
	case PrimType::i16:
		t_value = ConstantInt::get(ctx, APInt(16, n.getUint(), true)); break;
	case PrimType::u16:
		t_value = ConstantInt::get(ctx, APInt(16, n.getUint(), false)); break;
	case PrimType::c16:
		t_value = ConstantInt::get(ctx, APInt(16, (uint64_t)n.getChar(), false)); break;
	case PrimType::i32:
		t_value = ConstantInt::get(ctx, APInt(32, n.getUint(), true)); break;
	case PrimType::u32:
		t_value = ConstantInt::get(ctx, APInt(16, n.getUint(), false)); break;
	case PrimType::c32:
		t_value = ConstantInt::get(ctx, APInt(32, (uint64_t)n.getChar(), false)); break;
	case PrimType::i64:
		t_value = ConstantInt::get(ctx, APInt(64, n.getUint(), true)); break;
	case PrimType::u64:
		t_value = ConstantInt::get(ctx, APInt(64, n.getUint(), false)); break;
	case PrimType::f32:
	case PrimType::f64:
		t_value = ConstantFP::get(ctx, APFloat(n.getFloat())); break;
	case PrimType::v:
		t_value = nullptr; break;
	default:
		assert(0);
	}
}

void LLVMGenerator::visit(ArrayLiteralExprAST &n)
{
}

void LLVMGenerator::visit(VariableExprAST &n)
{
/*
	// Look this variable up in the function.
	Value *V = NamedValues[Name];
	if (!V)
		return ErrorV("Unknown variable name");

	KSDbgInfo.emitLocation(this);
	// Load the value.
	return Builder.CreateLoad(V, Name.c_str());
*/
}

void LLVMGenerator::visit(UnaryExprAST &n)
{
//	Value *OperandV = Operand->codegen();
//	if (!OperandV)
//		return nullptr;
//
//	Function *F = getFunction(std::string("unary") + Opcode);
//	if (!F)
//		return ErrorV("Unknown unary operator");
//
//	KSDbgInfo.emitLocation(this);
//	return Builder.CreateCall(F, OperandV, "unop");
}

void LLVMGenerator::visit(BinaryExprAST &n)
{
//	KSDbgInfo.emitLocation(this);
//
//	// Special case '=' because we don't want to emit the LHS as an expression.
//	if (Op == '=')
//	{
//		// Assignment requires the LHS to be an identifier.
//		// This assume we're building without RTTI because LLVM builds that way by
//		// default.  If you build LLVM with RTTI this can be changed to a
//		// dynamic_cast for automatic error checking.
//		VariableExprAST *LHSE = static_cast<VariableExprAST *>(LHS.get());
//		if (!LHSE)
//			return ErrorV("destination of '=' must be a variable");
//		// Codegen the RHS.
//		Value *Val = RHS->codegen();
//		if (!Val)
//			return nullptr;
//
//		// Look up the name.
//		Value *Variable = NamedValues[LHSE->getName()];
//		if (!Variable)
//			return ErrorV("Unknown variable name");
//
//		Builder.CreateStore(Val, Variable);
//		return Val;
//	}
//
//	Value *L = LHS->codegen();
//	Value *R = RHS->codegen();
//	if (!L || !R)
//		return nullptr;
//
//	switch (Op)
//	{
//	case '+':
//		return Builder.CreateFAdd(L, R, "addtmp");
//	case '-':
//		return Builder.CreateFSub(L, R, "subtmp");
//	case '*':
//		return Builder.CreateFMul(L, R, "multmp");
//	case '<':
//		L = Builder.CreateFCmpULT(L, R, "cmptmp");
//		// Convert bool 0/1 to double 0.0 or 1.0
//		return Builder.CreateUIToFP(L, llvm::Type::getDoubleTy(ctx),
//			"booltmp");
//	default:
//		break;
//	}
//
//	// If it wasn't a builtin binary operator, it must be a user defined one. Emit
//	// a call to it.
//	Function *F = getFunction(std::string("binary") + Op);
//	assert(F && "binary operator not found!");
//
//	Value *Ops[] = { L, R };
//	return Builder.CreateCall(F, Ops, "binop");
}

void LLVMGenerator::visit(IndexExprAST &n)
{
}

void LLVMGenerator::visit(CallExprAST &n)
{
//	KSDbgInfo.emitLocation(this);
//
//	// Look up the name in the global module table.
//	Function *CalleeF = getFunction(Callee);
//	if (!CalleeF)
//		return ErrorV("Unknown function referenced");
//
//	// If argument mismatch error.
//	if (CalleeF->arg_size() != Args.size())
//		return ErrorV("Incorrect # arguments passed");
//
//	std::vector<Value *> ArgsV;
//	for (unsigned i = 0, e = Args.size(); i != e; ++i)
//	{
//		ArgsV.push_back(Args[i]->codegen());
//		if (!ArgsV.back())
//			return nullptr;
//	}
//
//	return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}

void LLVMGenerator::visit(IfExprAST &n)
{
//	KSDbgInfo.emitLocation(this);
//
//	Value *CondV = Cond->codegen();
//	if (!CondV)
//		return nullptr;
//
//	// Convert condition to a bool by comparing equal to 0.0.
//	CondV = Builder.CreateFCmpONE(
//		CondV, ConstantFP::get(ctx, APFloat(0.0)), "ifcond");
//
//	Function *TheFunction = Builder.GetInsertBlock()->getParent();
//
//	// Create blocks for the then and else cases.  Insert the 'then' block at the
//	// end of the function.
//	BasicBlock *ThenBB =
//		BasicBlock::Create(ctx, "then", TheFunction);
//	BasicBlock *ElseBB = BasicBlock::Create(ctx, "else");
//	BasicBlock *MergeBB = BasicBlock::Create(ctx, "ifcont");
//
//	Builder.CreateCondBr(CondV, ThenBB, ElseBB);
//
//	// Emit then value.
//	Builder.SetInsertPoint(ThenBB);
//
//	Value *ThenV = Then->codegen();
//	if (!ThenV)
//		return nullptr;
//
//	Builder.CreateBr(MergeBB);
//	// Codegen of 'Then' can change the current block, update ThenBB for the PHI.
//	ThenBB = Builder.GetInsertBlock();
//
//	// Emit else block.
//	TheFunction->getBasicBlockList().push_back(ElseBB);
//	Builder.SetInsertPoint(ElseBB);
//
//	Value *ElseV = Else->codegen();
//	if (!ElseV)
//		return nullptr;
//
//	Builder.CreateBr(MergeBB);
//	// Codegen of 'Else' can change the current block, update ElseBB for the PHI.
//	ElseBB = Builder.GetInsertBlock();
//
//	// Emit merge block.
//	TheFunction->getBasicBlockList().push_back(MergeBB);
//	Builder.SetInsertPoint(MergeBB);
//	PHINode *PN =
//		Builder.CreatePHI(llvm::Type::getDoubleTy(ctx), 2, "iftmp");
//
//	PN->addIncoming(ThenV, ThenBB);
//	PN->addIncoming(ElseV, ElseBB);
//	return PN;
}

void LLVMGenerator::visit(ForExprAST &n)
{
//	// Output for-loop as:
//	//   var = alloca double
//	//   ...
//	//   start = startexpr
//	//   store start -> var
//	//   goto loop
//	// loop:
//	//   ...
//	//   bodyexpr
//	//   ...
//	// loopend:
//	//   step = stepexpr
//	//   endcond = endexpr
//	//
//	//   curvar = load var
//	//   nextvar = curvar + step
//	//   store nextvar -> var
//	//   br endcond, loop, endloop
//	// outloop:
//	Function *TheFunction = Builder.GetInsertBlock()->getParent();
//
//	// Create an alloca for the variable in the entry block.
//	AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
//
//	KSDbgInfo.emitLocation(this);
//
//	// Emit the start code first, without 'variable' in scope.
//	Value *StartVal = Start->codegen();
//	if (!StartVal)
//		return nullptr;
//
//	// Store the value into the alloca.
//	Builder.CreateStore(StartVal, Alloca);
//
//	// Make the new basic block for the loop header, inserting after current
//	// block.
//	BasicBlock *LoopBB =
//		BasicBlock::Create(ctx, "loop", TheFunction);
//
//	// Insert an explicit fall through from the current block to the LoopBB.
//	Builder.CreateBr(LoopBB);
//
//	// Start insertion in LoopBB.
//	Builder.SetInsertPoint(LoopBB);
//
//	// Within the loop, the variable is defined equal to the PHI node.  If it
//	// shadows an existing variable, we have to restore it, so save it now.
//	AllocaInst *OldVal = NamedValues[VarName];
//	NamedValues[VarName] = Alloca;
//
//	// Emit the body of the loop.  This, like any other expr, can change the
//	// current BB.  Note that we ignore the value computed by the body, but don't
//	// allow an error.
//	if (!Body->codegen())
//		return nullptr;
//
//	// Emit the step value.
//	Value *StepVal = nullptr;
//	if (Step)
//	{
//		StepVal = Step->codegen();
//		if (!StepVal)
//			return nullptr;
//	}
//	else
//	{
//		// If not specified, use 1.0.
//		StepVal = ConstantFP::get(ctx, APFloat(1.0));
//	}
//
//	// Compute the end condition.
//	Value *EndCond = End->codegen();
//	if (!EndCond)
//		return nullptr;
//
//	// Reload, increment, and restore the alloca.  This handles the case where
//	// the body of the loop mutates the variable.
//	Value *CurVar = Builder.CreateLoad(Alloca, VarName.c_str());
//	Value *NextVar = Builder.CreateFAdd(CurVar, StepVal, "nextvar");
//	Builder.CreateStore(NextVar, Alloca);
//
//	// Convert condition to a bool by comparing equal to 0.0.
//	EndCond = Builder.CreateFCmpONE(
//		EndCond, ConstantFP::get(ctx, APFloat(0.0)), "loopcond");
//
//	// Create the "after loop" block and insert it.
//	BasicBlock *AfterBB =
//		BasicBlock::Create(ctx, "afterloop", TheFunction);
//
//	// Insert the conditional branch into the end of LoopEndBB.
//	Builder.CreateCondBr(EndCond, LoopBB, AfterBB);
//
//	// Any new code will be inserted in AfterBB.
//	Builder.SetInsertPoint(AfterBB);
//
//	// Restore the unshadowed variable.
//	if (OldVal)
//		NamedValues[VarName] = OldVal;
//	else
//		NamedValues.erase(VarName);
//
//	// for expr always returns 0.0.
//	return Constant::getNullValue(llvm::Type::getDoubleTy(ctx));
}

void LLVMGenerator::visit(TypeDecl &n)
{
}

void LLVMGenerator::visit(VarDecl &n)
{
	llvm::Type *pType = nullptr;
	if (n.type())
	{
		n.type()->accept(*this);
		pType = t_type;
	}
	else if (n.init())
	{
		n.init()->type()->accept(*this);
		pType = t_type;
	}
	else
		assert(0);

	if (dynamic_cast<::Module*>(scope))
	{
		Constant *pGlobal = TheModule->getOrInsertGlobal(n.name(), pType);
	}
	else
	{
		// other scopes?
	}
}

void LLVMGenerator::visit(PrototypeDecl &n)
{
//	// Make the function type:  double(double,double) etc.
//	std::vector<llvm::Type *> Doubles(Args.size(),
//		llvm::Type::getDoubleTy(ctx));
//	FunctionType *FT =
//		FunctionType::get(llvm::Type::getDoubleTy(ctx), Doubles, false);
//
//	Function *F =
//		Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());
//
//	// Set names for all arguments.
//	unsigned Idx = 0;
//	for (auto &Arg : F->args())
//		Arg.setName(Args[Idx++]);
//
//	return F;
}

static std::map<char, int> BinopPrecedence;
void LLVMGenerator::visit(FunctionDecl &n)
{
//	// Transfer ownership of the prototype to the FunctionProtos map, but keep a
//	// reference to it for use below.
//	auto &P = *Proto;
//	FunctionProtos[Proto->getName()] = std::move(Proto);
//	Function *TheFunction = getFunction(P.getName());
//	if (!TheFunction)
//		return nullptr;
//
//	// If this is an operator, install it.
//	if (P.isBinaryOp())
//		BinopPrecedence[P.getOperatorName()] = P.getBinaryPrecedence();
//
//	// Create a new basic block to start insertion into.
//	BasicBlock *BB = BasicBlock::Create(ctx, "entry", TheFunction);
//	Builder.SetInsertPoint(BB);
//
//	// Create a subprogram DIE for this function.
//	DIFile *Unit = DBuilder->createFile(KSDbgInfo.TheCU->getFilename(),
//		KSDbgInfo.TheCU->getDirectory());
//	DIScope *FContext = Unit;
//	unsigned LineNo = P.getLine();
//	unsigned ScopeLine = LineNo;
//	DISubprogram *SP = DBuilder->createFunction(
//		FContext, P.getName(), StringRef(), Unit, LineNo,
//		createFunctionType(TheFunction->arg_size(), Unit),
//		false /* internal linkage */, true /* definition */, ScopeLine,
//		DINode::FlagPrototyped, false);
//	TheFunction->setSubprogram(SP);
//
//	// Push the current scope.
//	KSDbgInfo.LexicalBlocks.push_back(SP);
//
//	// Unset the location for the prologue emission (leading instructions with no
//	// location in a function are considered part of the prologue and the debugger
//	// will run past them when breaking on a function)
//	KSDbgInfo.emitLocation(nullptr);
//
//	// Record the function arguments in the NamedValues map.
//	NamedValues.clear();
//	unsigned ArgIdx = 0;
//	for (auto &Arg : TheFunction->args())
//	{
//		// Create an alloca for this variable.
//		AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, Arg.getName());
//
//		// Create a debug descriptor for the variable.
//		DILocalVariable *D = DBuilder->createParameterVariable(
//			SP, Arg.getName(), ++ArgIdx, Unit, LineNo, KSDbgInfo.getDoubleTy(),
//			true);
//
//		DBuilder->insertDeclare(Alloca, D, DBuilder->createExpression(),
//			DebugLoc::get(LineNo, 0, SP),
//			Builder.GetInsertBlock());
//
//		// Store the initial value into the alloca.
//		Builder.CreateStore(&Arg, Alloca);
//
//		// Add arguments to variable symbol table.
//		NamedValues[Arg.getName()] = Alloca;
//	}
//
//	KSDbgInfo.emitLocation(Body.get());
//
//	if (Value *RetVal = Body->codegen())
//	{
//		// Finish off the function.
//		Builder.CreateRet(RetVal);
//
//		// Pop off the lexical block for the function.
//		KSDbgInfo.LexicalBlocks.pop_back();
//
//		// Validate the generated code, checking for consistency.
//		verifyFunction(*TheFunction);
//
//		return TheFunction;
//	}
//
//	// Error reading body, remove function.
//	TheFunction->eraseFromParent();
//
//	if (P.isBinaryOp())
//		BinopPrecedence.erase(Proto->getOperatorName());
//
//	// Pop off the lexical block for the function since we added it
//	// unconditionally.
//	KSDbgInfo.LexicalBlocks.pop_back();
//
//	return nullptr;
}



//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

Value *ErrorV(const char *Str)
{
	Error(Str);
	return nullptr;
}

Function *LLVMGenerator::getFunction(std::string Name)
{
	// First, see if the function has already been added to the current module.
	if (auto *F = TheModule->getFunction(Name))
		return F;

	// If not, check whether we can codegen the declaration from some existing
	// prototype.
	auto FI = FunctionProtos.find(Name);
	if (FI != FunctionProtos.end())
		return (Function*)visitValue(FI->second.get());

	// If no existing prototype exists, return null.
	return nullptr;
}

/// CreateEntryBlockAlloca - Create an alloca instruction in the entry block of
/// the function.  This is used for mutable variables etc.
static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction,
	const std::string &VarName)
{
	IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
		TheFunction->getEntryBlock().begin());
	return TmpB.CreateAlloca(Type::getDoubleTy(getGlobalContext()), nullptr,
		VarName.c_str());
}


//===----------------------------------------------------------------------===//
// Debug Info Support
//===----------------------------------------------------------------------===//

DIType *DebugInfo::getDoubleTy(LLVMGenerator &g)
{
	if (DblTy)
		return DblTy;

	DblTy = g.DBuilder->createBasicType("double", 64, 64, dwarf::DW_ATE_float);
	return DblTy;
}

void DebugInfo::emitLocation(LLVMGenerator &g, Expr *AST)
{
	if (!AST)
		return g.Builder.SetCurrentDebugLocation(DebugLoc());
	DIScope *Scope;
	if (LexicalBlocks.empty())
		Scope = TheCU;
	else
		Scope = LexicalBlocks.back();
	g.Builder.SetCurrentDebugLocation(
		DebugLoc::get(AST->getLine(), AST->getCol(), Scope));
}

DISubroutineType *LLVMGenerator::createFunctionType(unsigned NumArgs, DIFile *Unit)
{
	SmallVector<Metadata *, 8> EltTys;
	DIType *DblTy = KSDbgInfo.getDoubleTy(*this);

	// Add the result type.
	EltTys.push_back(DblTy);

	for (unsigned i = 0, e = NumArgs; i != e; ++i)
		EltTys.push_back(DblTy);

	return DBuilder->createSubroutineType(DBuilder->getOrCreateTypeArray(EltTys));
}
