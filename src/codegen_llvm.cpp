#pragma warning(disable: 4996)

#include "mlang.h"
#undef error
#undef min
#undef max

#include "codegen_llvm.h"

using namespace llvm;
using namespace llvm::orc;


// TODO: READ THIS AND DO! http://llvm.org/docs/Frontend/PerformanceTips.html#id7

namespace m {

class MemStream : public llvm::raw_ostream
{
public:
	std::string text;
	void write_impl(const char *Ptr, size_t Size) override
	{
		text.append(Ptr, Size);
	}
	uint64_t current_pos() const override { return text.size(); }
	std::string take() { return std::move(text); }
};

class ModuleInfo
{
public:
	ModuleInfo(DICompileUnit *cu)
		: compileUnit(cu)
	{}

	DICompileUnit *compileUnit;
};

LLVMGenerator::LLVMGenerator(Compiler &compiler)
	: compiler(compiler)
	, ctx(getGlobalContext())
	, Builder(ctx)
	, TheJIT(llvm::make_unique<KaleidoscopeJIT>())
	, TheModule(llvm::make_unique<llvm::Module>(compiler.outFile, ctx))
{
	// Open a new module.
	TheModule->setDataLayout(TheJIT->getTargetMachine().createDataLayout());
	TheModule->setTargetTriple(sys::getProcessTriple()); // x86_64-pc-windows-msvc18.0.0 ???

	// Add the current debug info version into the module.
	TheModule->addModuleFlag(llvm::Module::Warning, "Debug Info Version", DEBUG_METADATA_VERSION);

	// Darwin only supports dwarf2.
	if (Triple(sys::getProcessTriple()).isOSDarwin())
		TheModule->addModuleFlag(llvm::Module::Warning, "Dwarf Version", 2);

	// Construct the DIBuilder, we do this here because we need the module.
	DBuilder = llvm::make_unique<DIBuilder>(*TheModule);

	for (auto module : compiler.modules)
		module.second->accept(*this);
}

void LLVMGenerator::pushFunction(FunctionLiteralExpr *f)
{
	TypeExpr *rt = f->type()->returnType();
	_functionStack.push_back(FunctionState{ f, rt->isVoid(), false, _scope.size(), nullptr });
	FunctionState &cur = _functionStack.back();
	if (!cur.isVoid)
	{
		cur.retval = Builder.CreateAlloca(rt->cgData<LLVMData>()->type, nullptr, "retval");
		cur.retval->setAlignment(rt->alignment());
	}
}

void LLVMGenerator::earlyReturn(Expr *expr)
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
			load->setAlignment(cur.retval->getAlignment());
			Builder.CreateRet(load);
		}
		else
			Builder.CreateRetVoid();

		Builder.SetInsertPoint(oldBlock);

		cur.hasEarlyReturn = true;
	}

	if (expr)
	{
		StoreInst *store = Builder.CreateStore(expr->cgData<LLVMData>()->value, cur.retval);
		store->setAlignment(cur.retval->getAlignment());
	}

	Builder.CreateBr(cur.returnBlock);
}

void InitCodegen()
{
//	InitializeNativeTarget();
//	InitializeNativeTargetAsmPrinter();
//	InitializeNativeTargetAsmParser();

	// Initialize targets first, so that --version shows registered targets.
	InitializeAllTargets();
	InitializeAllTargetMCs();
	InitializeAllAsmPrinters();
	InitializeAllAsmParsers();

	// Initialize codegen and IR passes used by llc so that the -print-after,
	// -print-before, and -stop-after options work.
	PassRegistry *Registry = PassRegistry::getPassRegistry();
	initializeCore(*Registry);
	initializeCodeGen(*Registry);
	initializeLoopStrengthReducePass(*Registry);
	initializeLowerIntrinsicsPass(*Registry);
	initializeUnreachableBlockElimPass(*Registry);

	// Register the target printer for --version.
	cl::AddExtraVersionPrinter(TargetRegistry::printRegisteredTargetsForVersion);
}

void Codegen(Compiler &compiler)
{
	LLVMGenerator *generator = new LLVMGenerator(compiler);

	generator->codegen();
}

void LLVMGenerator::codegen()
{
	// Finalize the debug info.
	DBuilder->finalize();

	std::vector<std::string> linkOptions;

	if (compiler.runtime.compare("MT") == 0)
	{
		linkOptions.push_back("/FAILIFMISMATCH:RuntimeLibrary=MT_StaticRelease");
		linkOptions.push_back("/DEFAULTLIB:LIBCMT");
		linkOptions.push_back("/DEFAULTLIB:OLDNAMES");
	}
	else if (compiler.runtime.compare("MTd") == 0)
	{
		linkOptions.push_back("/FAILIFMISMATCH:RuntimeLibrary=MTd_StaticDebug");
		linkOptions.push_back("/DEFAULTLIB:LIBCMTD");
		linkOptions.push_back("/DEFAULTLIB:OLDNAMES");
	}
	else if (compiler.runtime.compare("MD") == 0)
	{
		linkOptions.push_back("/FAILIFMISMATCH:RuntimeLibrary=MD_DynamicRelease");
		linkOptions.push_back("/DEFAULTLIB:MSVCRT");
		linkOptions.push_back("/DEFAULTLIB:OLDNAMES");
	}
	else if (compiler.runtime.compare("MDd") == 0)
	{
		linkOptions.push_back("/FAILIFMISMATCH:RuntimeLibrary=MDd_DynamicDebug");
		linkOptions.push_back("/DEFAULTLIB:MSVCRTD");
		linkOptions.push_back("/DEFAULTLIB:OLDNAMES");
	}
	else if (compiler.runtime.compare("none") != 0)
	{
		// bad runtime!
		assert(false);
	}

	std::vector<Metadata*> linkOpts;
	for (auto &opt : linkOptions)
	{
		// TODO: if option has spaces, it should be split into multiple MDString's..

		std::vector<Metadata*> linkFlag;
		linkFlag.push_back(MDString::get(ctx, opt));
		linkOpts.push_back(MDNode::get(ctx, linkFlag));
	}

	TheModule->addModuleFlag(llvm::Module::ModFlagBehavior::AppendUnique, "Linker Options", MDNode::get(ctx, linkOpts));

	if (!compiler.irFile.empty())
	{
		// Print out all of the generated code.
		MemStream output;
		TheModule->print(output, nullptr);

		std::string ir = output.take();

		if (!ir.empty())
		{
			FILE *file;
			fopen_s(&file, compiler.irFile.c_str(), "w");
			if (!file)
			{
				printf("Can't open file for output: %s\n", compiler.irFile.c_str());
				return;
			}
			fwrite(ir.c_str(), 1, ir.size(), file);
			fclose(file);
		}
	}

	if (!compiler.outFile.empty())
	{
		if (compiler.mode == Mode::OutputBC)
		{
			// TODO:...
			assert(false);
			return;
		}

		TargetMachine::CodeGenFileType fileType = FileType;

		if (compiler.mode == Mode::OutputAsm)
			fileType = TargetMachine::CGFT_AssemblyFile;
		else if (compiler.mode == Mode::Compile || compiler.mode == Mode::CompileAndLink)
			fileType = TargetMachine::CGFT_ObjectFile;

		Triple targetTriple = Triple(TheModule->getTargetTriple());

		// Get the target specific parser.
		std::string Error;
		const Target *TheTarget = TargetRegistry::lookupTarget(MArch, targetTriple, Error);
		if (!TheTarget)
		{
			printf("%s\n", Error.c_str());
			return;
		}

		std::string CPUStr = getCPUStr(), FeaturesStr = getFeaturesStr();

		CodeGenOpt::Level OLvl = CodeGenOpt::None;
		switch (compiler.opt)
		{
			case 0: OLvl = CodeGenOpt::None; break;
			case 1: OLvl = CodeGenOpt::Less; break;
			case 2: OLvl = CodeGenOpt::Default; break;
			case 3: OLvl = CodeGenOpt::Aggressive; break;
			default: break;
		}
//		CodeGenOpt::Level OLvl = CodeGenOpt::Default;
//		switch (OptLevel) {
//		default:
//			errs() << argv[0] << ": invalid optimization level.\n";
//			return 1;
//		case ' ': break;
//		case '0': OLvl = CodeGenOpt::None; break;
//		case '1': OLvl = CodeGenOpt::Less; break;
//		case '2': OLvl = CodeGenOpt::Default; break;
//		case '3': OLvl = CodeGenOpt::Aggressive; break;
//		}

		TargetOptions Options = InitTargetOptionsFromCodeGenFlags();
//		Options.DisableIntegratedAS = NoIntegratedAssembler;
//		Options.MCOptions.ShowMCEncoding = ShowMCEncoding;
//		Options.MCOptions.MCUseDwarfDirectory = EnableDwarfDirectory;
//		Options.MCOptions.AsmVerbose = AsmVerbose;

		std::unique_ptr<TargetMachine> Target(
			TheTarget->createTargetMachine(targetTriple.getTriple(), CPUStr, FeaturesStr,
				Options, RelocModel, CMModel, OLvl));

		assert(Target && "Could not allocate target machine!");

		if (FloatABIForCalls != FloatABI::Default)
			Options.FloatABIType = FloatABIForCalls;

		// Figure out where we are going to send the output.
		std::error_code EC;
		sys::fs::OpenFlags OpenFlags = sys::fs::F_None;
		if (fileType == TargetMachine::CGFT_AssemblyFile)
			OpenFlags |= sys::fs::F_Text;
		auto Out = std::make_unique<tool_output_file>(compiler.mode == Mode::CompileAndLink ? compiler.objFile : compiler.outFile, EC, OpenFlags);
//		std::unique_ptr<tool_output_file> Out =
//			GetOutputStream(TheTarget->getName(), targetTriple.getOS(), outFile);
		assert(Out);

		// Build up all of the passes that we want to do to the module.
		legacy::PassManager PM;

		// Add an appropriate TargetLibraryInfo pass for the module's triple.
		TargetLibraryInfoImpl TLII(Triple(TheModule->getTargetTriple()));

		// The -disable-simplify-libcalls flag actually disables all builtin optzns.
//		if (DisableSimplifyLibCalls)
//			TLII.disableAllFunctions();
		PM.add(new TargetLibraryInfoWrapperPass(TLII));

		// Add the target data from the target machine, if it exists, or the module.
		TheModule->setDataLayout(Target->createDataLayout());

		// Override function attributes based on CPUStr, FeaturesStr, and command line
		// flags.
		setFunctionAttributes(CPUStr, FeaturesStr, *TheModule);

		if (RelaxAll.getNumOccurrences() > 0 && fileType != TargetMachine::CGFT_ObjectFile)
			errs() << compiler.outFile << ": warning: ignoring -mc-relax-all because filetype != obj";

		{
			raw_pwrite_stream *OS = &Out->os();

			// Manually do the buffering rather than using buffer_ostream,
			// so we can memcmp the contents in CompileTwice mode
			SmallVector<char, 0> Buffer;
			std::unique_ptr<raw_svector_ostream> BOS;
			if ((fileType != TargetMachine::CGFT_AssemblyFile &&
				!Out->os().supportsSeeking()))
			{
				BOS = make_unique<raw_svector_ostream>(Buffer);
				OS = BOS.get();
			}

			AnalysisID StartBeforeID = nullptr;
			AnalysisID StartAfterID = nullptr;
			AnalysisID StopAfterID = nullptr;
			const PassRegistry *PR = PassRegistry::getPassRegistry();
			if (!RunPass.empty()) {
				if (!StartAfter.empty() || !StopAfter.empty()) {
					errs() << compiler.outFile << ": start-after and/or stop-after passes are "
						"redundant when run-pass is specified.\n";
					assert(false);
				}
				const PassInfo *PI = PR->getPassInfo(RunPass);
				if (!PI) {
					errs() << compiler.outFile << ": run-pass pass is not registered.\n";
					assert(false);
				}
				StopAfterID = StartBeforeID = PI->getTypeInfo();
			}
			else {
				if (!StartAfter.empty()) {
					const PassInfo *PI = PR->getPassInfo(StartAfter);
					if (!PI) {
						errs() << compiler.outFile << ": start-after pass is not registered.\n";
						assert(false);
					}
					StartAfterID = PI->getTypeInfo();
				}
				if (!StopAfter.empty()) {
					const PassInfo *PI = PR->getPassInfo(StopAfter);
					if (!PI) {
						errs() << compiler.outFile << ": stop-after pass is not registered.\n";
						assert(false);
					}
					StopAfterID = PI->getTypeInfo();
				}
			}

			// Ask the target to add backend passes as necessary.
//			if (Target->addPassesToEmitFile(PM, *OS, fileType, NoVerify, StartBeforeID,
//				StartAfterID, StopAfterID, MIR.get())) {
			if (Target->addPassesToEmitFile(PM, *OS, fileType, true, StartBeforeID,
				StartAfterID, StopAfterID, nullptr)) {
				errs() << compiler.outFile << ": target does not support generation of this file type!\n";
				assert(false);
			}

			// Before executing passes, print the final values of the LLVM options.
			cl::PrintOptionValues();

			PM.run(*TheModule);

			if (BOS) {
				Out->os() << Buffer;
			}
		}

		// Declare success.
		Out->keep();
	}
}


void LLVMGenerator::visit(Declaration &n)
{
	if (n.doneCodegen()) return;

}

void LLVMGenerator::visit(Module &n)
{
	if (n.doneCodegen()) return;

	LLVMData *cg = n.cgData<LLVMData>();

	DICompileUnit *cu = DBuilder->createCompileUnit(dwarf::DW_LANG_C, n.filename(), n.directory(), "M-Lang Compiler", compiler.opt > 0, "", 0);
	cg->divalue = cg->discope = cu;

	pushScope(&n);

	for (auto &s : n.symbols())
		s.second->accept(*this);

	popScope();
}

void LLVMGenerator::visit(ModuleStatement &n)
{
	if (n.doneCodegen()) return;

}

void LLVMGenerator::visit(ExpressionStatement &n)
{
	if (n.doneCodegen()) return;

	n.expression()->accept(*this);
}

void LLVMGenerator::visit(ReturnStatement &n)
{
	if (n.doneCodegen()) return;

	FunctionState &cur = _functionStack.back();

	Expr *expr = n.expression();

	// if the function returns void, we expect no return args
	assert(cur.isVoid == (expr == nullptr));

	if (expr)
		expr->accept(*this);

	if (cur.scopeDepth == _scope.size() && !cur.hasEarlyReturn)
	{
		if (expr)
			Builder.CreateRet(expr->cgData<LLVMData>()->value);
		else
			Builder.CreateRetVoid();
	}
	else
		earlyReturn(expr);
}

void LLVMGenerator::visit(ScopeStatement &n)
{
	if (n.doneCodegen()) return;

	pushScope(&n);
	for (auto &s : n.statements())
	{
		emitLocation(s);

		s->accept(*this);
		if (s->didReturn())
			break; // can't reach any statements after a return!
	}
	popScope();
}

void LLVMGenerator::visit(IfStatement &n)
{
	if (n.doneCodegen()) return;

	Function *TheFunction = Builder.GetInsertBlock()->getParent();

	if (n.initStatements().length > 0)
	{
		pushScope(&n);

		for (auto &s : n.initStatements())
			s->accept(*this);
	}

	BasicBlock *thenBlock = BasicBlock::Create(ctx, "if.then", TheFunction);
	BasicBlock *elseBlock = n.elseStatements() ? BasicBlock::Create(ctx, "if.else") : nullptr;
	BasicBlock *afterBlock = !(n.thenReturned() && n.elseReturned()) ? BasicBlock::Create(ctx, "if.end") : nullptr;

	n.cond()->accept(*this);
	LLVMData *cg = n.cond()->cgData<LLVMData>();
	Builder.CreateCondBr(cg->value, thenBlock, elseBlock ? elseBlock : afterBlock);

	// emit 'then' block
	Builder.SetInsertPoint(thenBlock);
	n.thenStatements()->accept(*this);

//	Value *thenV = block result expression; for if statement *expressions*

	if (!n.thenReturned())
		Builder.CreateBr(afterBlock);
	thenBlock = Builder.GetInsertBlock();

	if (elseBlock)
	{
		// emit 'else' block
		TheFunction->getBasicBlockList().push_back(elseBlock);
		Builder.SetInsertPoint(elseBlock);
		n.elseStatements()->accept(*this);

//		Value *elseV = block result expression; for if statement *expressions*

		if (!n.elseReturned())
			Builder.CreateBr(afterBlock);
		elseBlock = Builder.GetInsertBlock();
	}

	// continue
	if (n.initStatements().length > 0)
	{
		// TODO: destruct init statements!

		popScope();
	}

	if (afterBlock)
	{
		TheFunction->getBasicBlockList().push_back(afterBlock);
		Builder.SetInsertPoint(afterBlock);
	}

	// saw we want the if statement to be an expression... then PHI node will select the 'result' from each block...
//	PHINode *PN = Builder.CreatePHI(llvm::Type::getDoubleTy(ctx), 2, "iftmp");
//	PN->addIncoming(thenV, thenBlock);
//	PN->addIncoming(elseV, elseBlock);
//	cg->value = PN;
}

void LLVMGenerator::visit(LoopStatement &n)
{
	if (n.doneCodegen()) return;

	Function *TheFunction = Builder.GetInsertBlock()->getParent();

	// emit loop block
	pushScope(&n);

	// emit iterators
	for (auto i : n.iterators())
		i->accept(*this);

	Expr *cond = n.cond();

	BasicBlock *condBlock = cond ? BasicBlock::Create(ctx, "loop.cond", TheFunction) : nullptr;
	BasicBlock *loopBlock = BasicBlock::Create(ctx, "loop.body", cond ? nullptr : TheFunction);
	BasicBlock *afterBlock = BasicBlock::Create(ctx, "loop.end");

	if (cond)
	{
		Builder.CreateBr(condBlock);
		Builder.SetInsertPoint(condBlock);

		n.cond()->accept(*this);
		LLVMData *condCg = n.cond()->cgData<LLVMData>();
		Builder.CreateCondBr(condCg->value, loopBlock, afterBlock);

		TheFunction->getBasicBlockList().push_back(loopBlock);
	}
	else
		Builder.CreateBr(loopBlock);

	Builder.SetInsertPoint(loopBlock);

	// emit body
	ScopeStatement *body = n.body();
	body->accept(*this);

	// can't iterate the loop if there was a return in the body!
	bool didReturn = body->didReturn();
	if (!didReturn)
	{
		// emit increments
		for (auto s : n.incrementExpressions())
			s->accept(*this);

		Builder.CreateBr(cond ? condBlock : loopBlock);
	}

	// we can only reach the loop.end if there was an entry condition that failed
	if (!didReturn || cond)
	{
		// emit post-loop
		TheFunction->getBasicBlockList().push_back(afterBlock);
		Builder.SetInsertPoint(afterBlock);
	}

	// TODO: destruct iterators!

	popScope();
}

void LLVMGenerator::visit(PrimitiveType &n)
{
	if (n.doneCodegen()) return;

	LLVMData *cg = n.cgData<LLVMData>();

	switch (n.type())
	{
	case PrimType::v:
		cg->type = Type::getVoidTy(ctx);
//		cg->type = DBuilder->create
		break;
	case PrimType::u1:
		cg->type = Type::getInt1Ty(ctx);
		cg->ditype = DBuilder->createBasicType("bool", 1, 1, dwarf::DW_ATE_boolean);
		break;
	case PrimType::i8:
		cg->type = Type::getInt8Ty(ctx);
		cg->ditype = DBuilder->createBasicType("byte", 8, 8, dwarf::DW_ATE_signed);
		break;
	case PrimType::u8:
		cg->type = Type::getInt8Ty(ctx);
		cg->ditype = DBuilder->createBasicType("ubyte", 8, 8, dwarf::DW_ATE_unsigned);
		break;
	case PrimType::c8:
		cg->type = Type::getInt8Ty(ctx);
		cg->ditype = DBuilder->createBasicType("char", 8, 8, dwarf::DW_ATE_UTF);
		break;
	case PrimType::i16:
		cg->type = Type::getInt16Ty(ctx);
		cg->ditype = DBuilder->createBasicType("short", 16, 16, dwarf::DW_ATE_signed);
		break;
	case PrimType::u16:
		cg->type = Type::getInt16Ty(ctx);
		cg->ditype = DBuilder->createBasicType("ushort", 16, 16, dwarf::DW_ATE_unsigned);
		break;
	case PrimType::c16:
		cg->type = Type::getInt16Ty(ctx);
		cg->ditype = DBuilder->createBasicType("wchar", 16, 16, dwarf::DW_ATE_UTF);
		break;
	case PrimType::i32:
		cg->type = Type::getInt32Ty(ctx);
		cg->ditype = DBuilder->createBasicType("int", 32, 32, dwarf::DW_ATE_signed);
		break;
	case PrimType::u32:
		cg->type = Type::getInt32Ty(ctx);
		cg->ditype = DBuilder->createBasicType("uint", 32, 32, dwarf::DW_ATE_unsigned);
		break;
	case PrimType::c32:
		cg->type = Type::getInt32Ty(ctx);
		cg->ditype = DBuilder->createBasicType("dchar", 32, 32, dwarf::DW_ATE_UTF);
		break;
	case PrimType::i64:
		cg->type = Type::getInt64Ty(ctx);
		cg->ditype = DBuilder->createBasicType("long", 64, 64, dwarf::DW_ATE_signed);
		break;
	case PrimType::u64:
		cg->type = Type::getInt64Ty(ctx);
		cg->ditype = DBuilder->createBasicType("ulong", 64, 64, dwarf::DW_ATE_unsigned);
		break;
	case PrimType::i128:
		cg->type = Type::getInt128Ty(ctx);
		cg->ditype = DBuilder->createBasicType("cent", 128, 128, dwarf::DW_ATE_signed);
		break;
	case PrimType::u128:
		cg->type = Type::getInt128Ty(ctx);
		cg->ditype = DBuilder->createBasicType("ucent", 128, 128, dwarf::DW_ATE_unsigned);
		break;
	case PrimType::f16:
		cg->type = Type::getHalfTy(ctx);
		cg->ditype = DBuilder->createBasicType("half", 16, 16, dwarf::DW_ATE_float);
		break;
	case PrimType::f32:
		cg->type = Type::getFloatTy(ctx);
		cg->ditype = DBuilder->createBasicType("float", 632, 32, dwarf::DW_ATE_float);
		break;
	case PrimType::f64:
		cg->type = Type::getDoubleTy(ctx);
		cg->ditype = DBuilder->createBasicType("double", 64, 64, dwarf::DW_ATE_float);
		break;
	case PrimType::f128:
		cg->type = Type::getFP128Ty(ctx);
		cg->ditype = DBuilder->createBasicType("extended", 128, 128, dwarf::DW_ATE_float);
		break;
//		cg->type = Type::getX86_FP80Ty(ctx); break;
//		cg->type = Type::getPPC_FP128Ty(ctx); break;
	default:
		assert(false);
	}
}

void LLVMGenerator::visit(PointerType &n)
{
	if (n.doneCodegen()) return;

	LLVMData *cg = n.cgData<LLVMData>();

	n.targetType()->accept(*this);
	LLVMData *targetCg = n.targetType()->cgData<LLVMData>();

	cg->type = llvm::PointerType::getUnqual(targetCg->type);
	cg->ditype = DBuilder->createPointerType(targetCg->ditype, 64); // TODO: machine pointer size? 32bit?
}

void LLVMGenerator::visit(Struct &n)
{
	if (n.doneCodegen()) return;

	LLVMData *cg = n.cgData<LLVMData>();

	pushScope(&n);

	// gather member types
	SmallVector<Type*, 8> elements;
	for (auto &m : n.dataMembers())
	{
		m.decl->accept(*this);
		Type *t = m.decl->cgData<LLVMData>()->type;
		elements.push_back(t);
	}

	// make LLVM struct
	cg->type = StructType::get(ctx, elements);

	popScope();
}

void LLVMGenerator::visit(FunctionType &n)
{
	if (n.doneCodegen()) return;

	LLVMData *cg = n.cgData<LLVMData>();

	n.returnType()->accept(*this);
	llvm::Type *r = n.returnType()->cgData<LLVMData>()->type;

	TypeExprList argList = n.argTypes();
	SmallVector<llvm::Type*, 8> args;
	SmallVector<Metadata*, 8> types;

	if (!n.returnType()->isVoid())
		types.push_back(n.returnType()->cgData<LLVMData>()->ditype);

	for (auto a : argList)
	{
		a->accept(*this);
		args.push_back(a->cgData<LLVMData>()->type);
		types.push_back(a->cgData<LLVMData>()->ditype);
	}

	cg->type = llvm::FunctionType::get(r, args, false);
	cg->ditype = DBuilder->createSubroutineType(DBuilder->getOrCreateTypeArray(types));
}

void LLVMGenerator::visit(PrimitiveLiteralExpr &n)
{
	if (n.doneCodegen()) return;

	LLVMData *cg = n.cgData<LLVMData>();

	n.type()->accept(*this);

	switch (n.primType())
	{
	case PrimType::u1:
		cg->value = ConstantInt::get(ctx, APInt(1, n.getUint(), false)); break;
	case PrimType::i8:
		cg->value = ConstantInt::get(ctx, APInt(8, n.getInt(), true)); break;
	case PrimType::u8:
		cg->value = ConstantInt::get(ctx, APInt(16, n.getUint(), false)); break;
	case PrimType::c8:
		cg->value = ConstantInt::get(ctx, APInt(8, (uint64_t)n.getChar(), false)); break;
	case PrimType::i16:
		cg->value = ConstantInt::get(ctx, APInt(16, n.getInt(), true)); break;
	case PrimType::u16:
		cg->value = ConstantInt::get(ctx, APInt(16, n.getUint(), false)); break;
	case PrimType::c16:
		cg->value = ConstantInt::get(ctx, APInt(16, (uint64_t)n.getChar(), false)); break;
	case PrimType::i32:
		cg->value = ConstantInt::get(ctx, APInt(32, n.getInt(), true)); break;
	case PrimType::u32:
		cg->value = ConstantInt::get(ctx, APInt(16, n.getUint(), false)); break;
	case PrimType::c32:
		cg->value = ConstantInt::get(ctx, APInt(32, (uint64_t)n.getChar(), false)); break;
	case PrimType::i64:
		cg->value = ConstantInt::get(ctx, APInt(64, n.getInt(), true)); break;
	case PrimType::u64:
		cg->value = ConstantInt::get(ctx, APInt(64, n.getUint(), false)); break;
	case PrimType::i128:
		// TODO: literal is not big enough...
		cg->value = ConstantInt::get(ctx, APInt(128, n.getInt(), true)); break;
	case PrimType::u128:
		// TODO: literal is not big enough...
		cg->value = ConstantInt::get(ctx, APInt(128, n.getUint(), false)); break;
	case PrimType::f32:
		cg->value = ConstantFP::get(ctx, APFloat((float)n.getFloat())); break;
	case PrimType::f64:
		cg->value = ConstantFP::get(ctx, APFloat(n.getFloat())); break;
	case PrimType::f16:
	case PrimType::f128:
	{
		cg->value = CastInst::CreateFPCast(ConstantFP::get(ctx, APFloat(n.getFloat())), n.type()->cgData<LLVMData>()->type);
		break;
	}
	case PrimType::v:
		cg->value = nullptr; break;
	default:
		assert(0);
	}
}

void LLVMGenerator::visit(AggregateLiteralExpr &n)
{
	if (n.doneCodegen()) return;

	LLVMData *cg = n.cgData<LLVMData>();

	n.type()->accept(*this);

	Struct *s = n.type()->asStruct();
	llvm::StructType *ty = dyn_cast<StructType>(s->cgData<LLVMData>()->type);

	cg->value = UndefValue::get(ty);
	unsigned int i = 0;
	for (auto &e : n.items())
	{
		e->accept(*this);

		unsigned int idx[1] = { i++ };
		cg->value = Builder.CreateInsertValue(cg->value, e->cgData<LLVMData>()->value, idx);
	}
}

void LLVMGenerator::visit(FunctionLiteralExpr &n)
{
	if (n.doneCodegen()) return;

	LLVMData *cg = n.cgData<LLVMData>();

	n.type()->accept(*this);

	FunctionType *fn = n.type()->asFunction();
	TypeExpr *rt = fn->returnType();

	llvm::FunctionType *sig = (llvm::FunctionType*)fn->cgData<LLVMData>()->type;

	Function *function = Function::Create(sig, Function::InternalLinkage, "", TheModule.get());
//	function->setCallingConv(CallingConv::X86_VectorCall)
	function->addFnAttr(Attribute::AttrKind::NoUnwind);
//	function->addFnAttr(Attribute::AttrKind::UWTable);
	cg->value = function;

	DISubprogram *di = DBuilder->createFunction(scopeCg()->discope, n.givenName(), n.givenName(), scopeCg()->file(), n.defLoc().line, dyn_cast<DISubroutineType>(n.type()->cgData<LLVMData>()->ditype), true, true, n.getLoc().line, 0, compiler.opt > 0);
	function->setSubprogram(di);
	cg->divalue = cg->discope = di;

	// Create a new basic block to start insertion into.
	BasicBlock *block = BasicBlock::Create(ctx, "entry", function);
	Builder.SetInsertPoint(block);

	pushScope(&n);
	pushFunction(&n);
	FunctionState &funcState = _functionStack.back();

	// unset current location (begin as prologue)
	emitLocation(nullptr);

	// create stack storage and store all the args
	DeclList args = n.args();
	size_t i = 0;
	for (auto &a : function->args())
	{
		VarDecl *arg = (VarDecl*)args[i];
		arg->accept(*this);

		a.setName(arg->name());

		AllocaInst *alloc = (AllocaInst*)arg->cgData<LLVMData>()->value;

		// TODO: i + i assumed i == 0 is return value?? void return should not +1 ??
		DILocalVariable *diarg = DBuilder->createParameterVariable(cg->discope, arg->name(), i + 1, scopeCg()->file(), n.getLoc().line, arg->type()->cgData<LLVMData>()->ditype, true, 0);
		DBuilder->insertDeclare(alloc, diarg, DBuilder->createExpression(), DebugLoc::get(n.getLoc().line, 0, cg->discope), Builder.GetInsertBlock());

		StoreInst *store = Builder.CreateStore(&a, alloc);
		store->setAlignment(arg->targetType()->alignment());

		++i;
	}

//	KSDbgInfo.emitLocation(Body.get());

	bool bDidReturn = false;
	for (auto &s : n.statements())
	{
		emitLocation(s);

		s->accept(*this);

		bDidReturn = s->didReturn();
		if (bDidReturn)
			break; // can't reach any statements after a return!
	}

	if (!bDidReturn)
	{
		if (fn->returnType()->isVoid())
		{
			if (funcState.hasEarlyReturn)
				Builder.CreateBr(funcState.returnBlock);
			else
				Builder.CreateRetVoid();
		}
	}

	if (funcState.hasEarlyReturn)
		function->getBasicBlockList().push_back(funcState.returnBlock);

	popFunction();
	popScope();

//	// Pop off the lexical block for the function.
//	KSDbgInfo.LexicalBlocks.pop_back();
//
//	return TheFunction;

	// Validate the generated code, checking for consistency.
	MemStream os;
	if (verifyFunction(*function, &os))
	{
		os.flush();
		std::string err = os.take();
		printf("%s\n", err.c_str());
//		assert(false);
	}

//	// Error reading body, remove function.
//	TheFunction->eraseFromParent();
//
//	if (P.isBinaryOp())
//		BinopPrecedence.erase(Proto->getOperatorName());
//
//	// Pop off the lexical block for the function since we added it
//	// unconditionally.
//	KSDbgInfo.LexicalBlocks.pop_back();
}

// none=0, trunc=1, se=2, ze=3, fup=4, fdown=5, f2u=6, f2i=7, u2f=8, i2f=9, inez=10, fnez=11, invalid=-1
static char cast_flags[(size_t)PrimType::__NumTypes * (size_t)PrimType::__NumTypes] =
{	/* from     v  u1 b  ub c  s  us wc i  ui dc l  ul ct uc h  f  d  x */
	/* v    */  0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
	/* u1   */ -1, 0, 3, 3,-1, 3, 3,-1, 3, 3,-1, 3, 3, 3, 3, 8, 8, 8, 8,
	/* i8   */ -1,10, 0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 9, 9, 9, 9,
	/* u8   */ -1,10, 0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 8, 8, 8, 8,
	/* c8   */ -1,10, 0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 8, 8, 8, 8,
	/* i16  */ -1,10, 1, 1, 1, 0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 9, 9, 9, 9,
	/* u16  */ -1,10, 1, 1, 1, 0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 8, 8, 8, 8,
	/* c16  */ -1,10, 1, 1, 1, 0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 8, 8, 8, 8,
	/* i32  */ -1,10, 1, 1, 1, 1, 1, 1, 0, 0, 0, 2, 2, 2, 2, 9, 9, 9, 9,
	/* u32  */ -1,10, 1, 1, 1, 1, 1, 1, 0, 0, 0, 3, 3, 3, 3, 8, 8, 8, 8,
	/* c32  */ -1,10, 1, 1, 1, 1, 1, 1, 0, 0, 0, 3, 3, 3, 3, 8, 8, 8, 8,
	/* i64  */ -1,10, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 2, 2, 9, 9, 9, 9,
	/* u64  */ -1,10, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 3, 3, 8, 8, 8, 8,
	/* i128 */ -1,10, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 9, 9, 9, 9,
	/* u128 */ -1,10, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 8, 8, 8, 8,
	/* f16  */ -1,11, 7, 6, 6, 7, 6, 6, 7, 6, 6, 7, 6, 7, 6, 0, 4, 4, 4,
	/* f32  */ -1,11, 7, 6, 6, 7, 6, 6, 7, 6, 6, 7, 6, 7, 6, 5, 0, 4, 4,
	/* f64  */ -1,11, 7, 6, 6, 7, 6, 6, 7, 6, 6, 7, 6, 7, 6, 5, 5, 0, 4,
	/* f128 */ -1,11, 7, 6, 6, 7, 6, 6, 7, 6, 6, 7, 6, 7, 6, 5, 5, 5, 0
};

static llvm::Instruction::CastOps cast_types[] =
{
	(llvm::Instruction::CastOps)0,
	llvm::Instruction::CastOps::Trunc,
	llvm::Instruction::CastOps::SExt,
	llvm::Instruction::CastOps::ZExt,
	llvm::Instruction::CastOps::FPExt,
	llvm::Instruction::CastOps::FPTrunc,
	llvm::Instruction::CastOps::FPToUI,
	llvm::Instruction::CastOps::FPToSI,
	llvm::Instruction::CastOps::UIToFP,
	llvm::Instruction::CastOps::SIToFP
};

void LLVMGenerator::visit(RefExpr &n)
{
	if (n.doneCodegen()) return;

	LLVMData *cg = n.cgData<LLVMData>();

	PointerType *type = n.type();
	type->accept(*this);
	llvm::PointerType *typeCg = (llvm::PointerType*)type->cgData<LLVMData>()->type;

	Expr *owner = n.owner();
	if (owner)
		owner->accept(*this);

	// if the ref holds a relative target
	VarDecl *target = n.target();
	if (target)
	{
		target->accept(*this);

		// if the target has an 'owner' (that is, it is a member)
		if (n.refType() == RefExpr::Type::Member)
		{
			// assert that 'owner' is a struct (for now?)
			Struct *targetType = dynamic_cast<Struct*>(owner->type()->asPointer()->targetType()->resolveType());
			assert(targetType);

			Type *t = targetType->cgData<LLVMData>()->type;
			Value *v = owner->cgData<LLVMData>()->value;
			size_t i = targetType->memberIndex(target->name());

			cg->value = Builder.CreateStructGEP(t, v, i);
		}
		else
		{
			LLVMData *targetCg = target->cgData<LLVMData>();
			assert(targetCg->value);
			cg->value = targetCg->value;
		}
	}
	else
	{
		if (n.refType() == RefExpr::Type::Absolute)
		{
			size_t address = n.address();

			if (address == 0)
				cg->value = ConstantPointerNull::get(typeCg);
			else
			{
				TypeDecl *szt = dynamic_cast<TypeDecl*>(module->getDecl("size_t", true));
				szt->accept(*this);
				PrimitiveType *prim = dynamic_cast<PrimitiveType*>(szt->type());
				prim->accept(*this);
				IntegerType *sztCg = (IntegerType*)prim->cgData<LLVMData>()->type;

				// TODO: typeCg is a pointer type, does this function expect the poitner TARGET type? docs unclear...
				cg->value = Builder.CreateIntToPtr(ConstantInt::get(sztCg, (uint64_t)address), typeCg);
			}
		}
		else if (n.refType() == RefExpr::Type::Index)
		{
			// assert that 'owner' is a tuple (for now?)
			Tuple *targetType = dynamic_cast<Tuple*>(owner->type()->asPointer()->targetType()->resolveType());
			assert(targetType);

			if (targetType->isSequence())
			{
				Expr *index = n.index();
				index->accept(*this);

				Type *t = targetType->seqElement()->asType()->cgData<LLVMData>()->type;
				Value *v = owner->cgData<LLVMData>()->value;
				Value *i = index->cgData<LLVMData>()->value;

				// gotta cast pointers to arrays to pointers to elements to index elements
				v = Builder.CreatePointerCast(v, llvm::PointerType::getUnqual(t));
				cg->value = Builder.CreateGEP(t, v, i);
			}
			else
			{
				Type *t = targetType->cgData<LLVMData>()->type;
				Value *v = owner->cgData<LLVMData>()->value;

				cg->value = Builder.CreateStructGEP(t, v, n.element());
			}
		}
	}
}

void LLVMGenerator::visit(DerefExpr &n)
{
	if (n.doneCodegen()) return;

	LLVMData *cg = n.cgData<LLVMData>();

	Expr *expr = n.expr();
	expr->accept(*this);
	LLVMData *exprCg = expr->cgData<LLVMData>();

	LoadInst *load = Builder.CreateLoad(exprCg->value);
	load->setAlignment(expr->type()->asPointer()->targetType()->alignment());
	cg->value = load;
}

void LLVMGenerator::visit(TypeConvertExpr &n)
{
	if (n.doneCodegen()) return;

	LLVMData *cg = n.cgData<LLVMData>();

	Expr *expr = n.expr();
	expr->accept(*this);
	TypeExpr *type = n.type();
	type->accept(*this);

	TypeExpr *exprType = expr->type();

	LLVMData *exprCg = expr->cgData<LLVMData>();
	LLVMData *typeCg = type->cgData<LLVMData>();

	PrimitiveType *primt = type->asPrimitive();
	if (primt)
	{
		PrimType src;
		PrimitiveType *from_pt = exprType->asPrimitive();
		if (!from_pt)
		{
			// TODO: can 'from_type' be wrangled into a primitive type??
			assert(false);
		}
		else
			src = from_pt->type();

		int castType = cast_flags[(int)src*(int)PrimType::__NumTypes + (int)primt->type()];
		assert(castType >= 0);

		if (castType == 0)
			cg->value = exprCg->value;
		else if (castType == 10)
			cg->value = Builder.CreateICmpNE(exprCg->value, ConstantInt::get(exprType->cgData<LLVMData>()->type, 0), "tobool");
		else if (castType == 11)
			cg->value = Builder.CreateFCmpUNE(exprCg->value, ConstantFP::get(ctx, APFloat(0.0)), "tobool");
		else
			cg->value = Builder.CreateCast(cast_types[castType], exprCg->value, typeCg->type, "cast");
		return;
	}
	PointerType *ptrt = type->asPointer();
	if (ptrt)
	{
		if (exprType->asPrimitive())
		{
			assert(false); // test!
			cg->value = Builder.CreateBitOrPointerCast(exprCg->value, typeCg->type, "cast");
			return;
		}
		else
		{
			assert(exprType->ptrDepth() == ptrt->ptrDepth());
			cg->value = Builder.CreatePointerCast(exprCg->value, typeCg->type, "cast");
			return;
		}
	}

	assert(false);
	// can 'type' be constructed from a 'from_type'
}

void LLVMGenerator::visit(UnaryExpr &n)
{
	if (n.doneCodegen()) return;

	LLVMData *cg = n.cgData<LLVMData>();

	Expr *operand = n.operand();
	operand->accept(*this);
	TypeExpr *type = n.type();
	type->accept(*this);

	PrimitiveType *pt = dynamic_cast<PrimitiveType*>(type);
	if (pt)
	{
		LLVMData *operandCg = operand->cgData<LLVMData>();

		PrimType primType = pt->type();

		switch (n.op())
		{
		case UnaryOp::Neg:
			assert(isInt(primType) || isFloat(primType));
			if(isFloat(primType))
				cg->value = Builder.CreateFNeg(operandCg->value, "fneg");
			else
				cg->value = Builder.CreateNeg(operandCg->value, "neg");
			break;
		case UnaryOp::Pos:
			assert(isInt(primType) || isFloat(primType));
			cg->value = operandCg->value;
			break;
		case UnaryOp::LogicNot:
			assert(primType > PrimType::v);
			cg->value = Builder.CreateNot(operandCg->value, "not");
			break;
		case UnaryOp::BitNot:
			assert(isNotFloat(primType));
			cg->value = Builder.CreateXor(operandCg->value, (uint64_t)-1, "comp");
			break;
		case UnaryOp::PreDec:
//				n.codegenData = Builder.CreateXor(operandCg->value, (uint64_t)-1);
			assert(false);
			break;
		case UnaryOp::PreInc:
//				n.codegenData = Builder.CreateXor(operandCg->value, (uint64_t)-1);
			assert(false);
			break;
		default:
			assert(false);
			break;
		}
	}
	else
		assert(false);


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

void LLVMGenerator::visit(BinaryExpr &n)
{
	if (n.doneCodegen()) return;

	LLVMData *cg = n.cgData<LLVMData>();

	Expr *lhs = n.lhs();
	n.lhs()->accept(*this);
	Expr *rhs = n.rhs();
	n.rhs()->accept(*this);
	TypeExpr *type = n.type();
	type->accept(*this);

	LLVMData *lhCg = lhs->cgData<LLVMData>();
	LLVMData *rhCg = rhs->cgData<LLVMData>();

	PrimitiveType *pt = dynamic_cast<PrimitiveType*>(type);
	if (pt)
	{
		PrimType primType = pt->type();

		switch (n.op())
		{
		case BinOp::Add:
			assert(primType > PrimType::u1);
			if (isFloat(primType))
				cg->value = Builder.CreateFAdd(lhCg->value, rhCg->value, "fadd");
			else
				cg->value = Builder.CreateAdd(lhCg->value, rhCg->value, "add");
			break;
		case BinOp::Sub:
			assert(primType > PrimType::u1);
			if (isFloat(primType))
				cg->value = Builder.CreateFSub(lhCg->value, rhCg->value, "fsub");
			else
				cg->value = Builder.CreateSub(lhCg->value, rhCg->value, "sub");
			break;
		case BinOp::Mul:
			assert(primType > PrimType::u1);
			if (isFloat(primType))
				cg->value = Builder.CreateFMul(lhCg->value, rhCg->value, "fmul");
			else
				cg->value = Builder.CreateMul(lhCg->value, rhCg->value, "mul");
			break;
		case BinOp::Div:
			assert(primType > PrimType::u1);
			if (isFloat(primType))
				cg->value = Builder.CreateFDiv(lhCg->value, rhCg->value, "fdiv");
			else if (isUnsignedInt(primType))
				cg->value = Builder.CreateUDiv(lhCg->value, rhCg->value, "udiv");
			else
				cg->value = Builder.CreateSDiv(lhCg->value, rhCg->value, "sdiv");
			break;
		case BinOp::Mod:
			assert(primType > PrimType::u1);
			if (isFloat(primType))
				cg->value = Builder.CreateFRem(lhCg->value, rhCg->value, "fmod");
			else if (isUnsignedInt(primType))
				cg->value = Builder.CreateURem(lhCg->value, rhCg->value, "umod");
			else
				cg->value = Builder.CreateSRem(lhCg->value, rhCg->value, "smod");
			break;
		case BinOp::Pow:
		{
			// TODO: why doesn't this work?!! >_<
			assert(primType > PrimType::u1);
			PrimitiveType *lhpt = dynamic_cast<PrimitiveType*>(lhs->type());
			PrimitiveType *rhpt = dynamic_cast<PrimitiveType*>(rhs->type());
			assert(lhpt && rhpt);
			PrimType lh = lhpt->type();
			PrimType rh = rhpt->type();
			if (isFloat(rh))
			{
				llvm::Function *pow = TheModule->getFunction(std::string("llvm.pow.").append(primTypeNames[(int)lh]));
				llvm::Value* args[2] = { lhCg->value, rhCg->value };
				cg->value = Builder.CreateCall(pow, args, "pow");
			}
			else if (isInt(rh))
			{
				llvm::Function *pow = TheModule->getFunction(std::string("llvm.powi.").append(primTypeNames[(int)lh]));
				llvm::Value* args[2] = { lhCg->value, rhCg->value };
				cg->value = Builder.CreateCall(pow, args, "powi");
			}
			break;
		}
		case BinOp::Cat:
			assert(false);
			break;
		case BinOp::SHL:
			assert(isNotFloat(primType));
			cg->value = Builder.CreateShl(lhCg->value, rhCg->value, "shl");
			break;
		case BinOp::ASR:
			assert(isNotFloat(primType));
			cg->value = Builder.CreateAShr(lhCg->value, rhCg->value, "asr");
			break;
		case BinOp::LSR:
			assert(isNotFloat(primType));
			cg->value = Builder.CreateLShr(lhCg->value, rhCg->value, "lsr");
			break;
		case BinOp::BitAnd:
			assert(isNotFloat(primType));
			cg->value = Builder.CreateAnd(lhCg->value, rhCg->value, "and");
			break;
		case BinOp::BitOr:
			assert(isNotFloat(primType));
			cg->value = Builder.CreateOr(lhCg->value, rhCg->value, "or");
			break;
		case BinOp::BitXor:
			assert(isNotFloat(primType));
			cg->value = Builder.CreateXor(lhCg->value, rhCg->value, "xor");
			break;
		case BinOp::LogicAnd:
			assert(isBool(primType));
			cg->value = Builder.CreateAnd(lhCg->value, rhCg->value, "and");
			break;
		case BinOp::LogicOr:
			assert(isBool(primType));
			cg->value = Builder.CreateOr(lhCg->value, rhCg->value, "or");
			break;
		case BinOp::LogicXor:
			assert(isBool(primType));
			cg->value = Builder.CreateXor(lhCg->value, rhCg->value, "xor");
			break;
		case BinOp::Eq:
		{
			PrimitiveType *lhpt = dynamic_cast<PrimitiveType*>(lhs->type());
			if (lhpt)
			{
				PrimType lhPrimType = lhpt->type();
				if (isFloat(lhPrimType))
					cg->value = Builder.CreateFCmpOEQ(lhCg->value, rhCg->value, "feq");
				else if (isNotFloat(lhPrimType))
					cg->value = Builder.CreateICmpEQ(lhCg->value, rhCg->value, "eq");
				else
					assert(false);
			}
			break;
		}
		case BinOp::Ne:
		{
			PrimitiveType *lhpt = dynamic_cast<PrimitiveType*>(lhs->type());
			if (lhpt)
			{
				PrimType lhPrimType = lhpt->type();
				if (isFloat(lhPrimType))
					cg->value = Builder.CreateFCmpONE(lhCg->value, rhCg->value, "fne");
				else if (isNotFloat(lhPrimType))
					cg->value = Builder.CreateICmpNE(lhCg->value, rhCg->value, "ne");
				else
					assert(false);
			}
			break;
		}
		case BinOp::Gt:
		{
			PrimitiveType *lhpt = dynamic_cast<PrimitiveType*>(lhs->type());
			if (lhpt)
			{
				PrimType lhPrimType = lhpt->type();
				if (isFloat(lhPrimType))
					cg->value = Builder.CreateFCmpOGT(lhCg->value, rhCg->value, "fgt");
				else if (isUnsignedInt(lhPrimType))
					cg->value = Builder.CreateICmpUGT(lhCg->value, rhCg->value, "ugt");
				else if (isSignedInt(lhPrimType))
					cg->value = Builder.CreateICmpSGT(lhCg->value, rhCg->value, "sgt");
				else
					assert(false);
			}
			break;
		}
		case BinOp::Ge:
		{
			PrimitiveType *lhpt = dynamic_cast<PrimitiveType*>(lhs->type());
			if (lhpt)
			{
				PrimType lhPrimType = lhpt->type();
				if (isFloat(lhPrimType))
					cg->value = Builder.CreateFCmpOGE(lhCg->value, rhCg->value, "fge");
				else if (isUnsignedInt(lhPrimType))
					cg->value = Builder.CreateICmpUGE(lhCg->value, rhCg->value, "uge");
				else if (isSignedInt(lhPrimType))
					cg->value = Builder.CreateICmpSGE(lhCg->value, rhCg->value, "sge");
				else
					assert(false);
			}
			break;
		}
		case BinOp::Lt:
		{
			PrimitiveType *lhpt = dynamic_cast<PrimitiveType*>(lhs->type());
			if (lhpt)
			{
				PrimType lhPrimType = lhpt->type();
				if (isFloat(lhPrimType))
					cg->value = Builder.CreateFCmpOLT(lhCg->value, rhCg->value, "flt");
				else if (isUnsignedInt(lhPrimType))
					cg->value = Builder.CreateICmpULT(lhCg->value, rhCg->value, "ult");
				else if (isSignedInt(lhPrimType))
					cg->value = Builder.CreateICmpSLT(lhCg->value, rhCg->value, "slt");
				else
					assert(false);
			}
			break;
		}
		case BinOp::Le:
		{
			PrimitiveType *lhpt = dynamic_cast<PrimitiveType*>(lhs->type());
			if (lhpt)
			{
				PrimType lhPrimType = lhpt->type();
				if (isFloat(lhPrimType))
					cg->value = Builder.CreateFCmpOLE(lhCg->value, rhCg->value, "fle");
				else if (isUnsignedInt(lhPrimType))
					cg->value = Builder.CreateICmpULE(lhCg->value, rhCg->value, "ule");
				else if (isSignedInt(lhPrimType))
					cg->value = Builder.CreateICmpSLE(lhCg->value, rhCg->value, "sle");
				else
					assert(false);
			}
			break;
		}
		}
	}

//	KSDbgInfo.emitLocation(this);
//
//	// Special case '=' because we don't want to emit the LHS as an expression.
//	if (Op == '=')
//	{
//		// Assignment requires the LHS to be an identifier.
//		// This assume we're building without RTTI because LLVM builds that way by
//		// default.  If you build LLVM with RTTI this can be changed to a
//		// dynamic_cast for automatic error checking.
//		IdentifierExpr *LHSE = static_cast<IdentifierExpr *>(LHS.get());
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

void LLVMGenerator::visit(CallExpr &n)
{
	if (n.doneCodegen()) return;

	LLVMData *cg = n.cgData<LLVMData>();

	Expr *func = n.function();

	func->accept(*this);

	// TODO: handle calling on function pointers?

	FunctionType *fn = func->type()->asFunction();
	if (fn)
	{
		LLVMData *cgFn = func->cgData<LLVMData>();
		std::vector<llvm::Value*> args;
		for (auto a : n.callArgs())
		{
			a->accept(*this);
			LLVMData *cgArg = a->cgData<LLVMData>();
			args.push_back(cgArg->value);
		}
		if (fn->returnType()->isVoid())
			Builder.CreateCall((llvm::Function*)cgFn->value, args);
		else
			cg->value = Builder.CreateCall((llvm::Function*)cgFn->value, args, "result");// fl->name());
		return;
	}

	assert(false);

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

void LLVMGenerator::visit(AssignExpr &n)
{
	if (n.doneCodegen()) return;

	Expr *target = n.target();
	target->accept(*this);
	Expr *expr = n.expr();
	expr->accept(*this);

	LLVMData *targetCg = target->cgData<LLVMData>();
	LLVMData *exprCg = expr->cgData<LLVMData>();

	StoreInst *store = Builder.CreateStore(exprCg->value, targetCg->value, false);
	store->setAlignment(target->type()->asPointer()->targetType()->alignment());
}

void LLVMGenerator::visit(BindExpr &n)
{
	if (n.doneCodegen()) return;

	int x = 0;
}

void LLVMGenerator::visit(Identifier &n)
{
	if (n.doneCodegen()) return;

	LLVMData *cg = n.cgData<LLVMData>();

	Declaration *decl = n.target();
	decl->accept(*this);

	LLVMData *declCg = n.resolve()->cgData<LLVMData>();
	if (n.isExpr())
		cg->value = declCg->value;
	else
		cg->type = declCg->type;
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

void LLVMGenerator::visit(MemberLookup &n)
{
	if (n.doneCodegen()) return;

	LLVMData *cg = n.cgData<LLVMData>();

	Node *expr = n.expr();
	expr->accept(*this);

	LLVMData *exprCg = expr->cgData<LLVMData>();
	if (n.isExpr())
		cg->value = exprCg->value;
	else
		cg->type = exprCg->type;
}

void LLVMGenerator::visit(Tuple &n)
{
	if (n.doneCodegen()) return;

	LLVMData *cg = n.cgData<LLVMData>();

	if (n.isType())
	{
		if (n.isSequence())
		{
			TypeExpr *t = dynamic_cast<TypeExpr*>(n.seqElement());
			t->accept(*this);

			for (auto e : n.shape())
				e->accept(*this);

			Type *ty = t->cgData<LLVMData>()->type;
			if (!n.isDynamicSize())
				cg->type = ArrayType::get(ty, n.numElements());
			else
				cg->type = ty;
		}
		else
		{
			// gather member types
			std::vector<Type*> elements;
			for (auto e : n.elements())
			{
				TypeExpr *t = dynamic_cast<TypeExpr*>(e);
				t->accept(*this);
				t = t->resolveType();

				Type *ty = t->cgData<LLVMData>()->type;
				elements.push_back(ty);
			}

			// make LLVM struct
			cg->type = StructType::get(ctx, elements);
		}
	}
	else if (n.isExpr())
	{
		TypeExpr *type = n.type();
		type->accept(*this);

		if (n.isSequence())
		{
			Expr *expr = dynamic_cast<Expr*>(n.seqElement());
			expr->accept(*this);

			Value *val = expr->cgData<LLVMData>()->value;

			if (expr->isConstant())
			{
				std::vector<Constant*> elements;
				elements.reserve(n.numElements());
				for (ptrdiff_t i = 0; i < n.numElements(); ++i)
					elements.push_back(dyn_cast<Constant>(val));
				cg->value = ConstantArray::get(dyn_cast<ArrayType>(type->cgData<LLVMData>()->type), elements);
			}
			else
			{
				cg->value = UndefValue::get(type->cgData<LLVMData>()->type);
				for (ptrdiff_t i = 0; i < n.numElements(); ++i)
					cg->value = Builder.CreateInsertValue(cg->value, val, { (unsigned int)i });
			}
		}
		else
		{
			cg->value = UndefValue::get(type->cgData<LLVMData>()->type);

			// TODO: ConstantStruct version when tuple elements are all const?

			auto elements = n.elements();
			for (size_t i = 0; i < elements.length; ++i)
			{
				Expr *expr = dynamic_cast<Expr*>(elements[i]);
				expr->accept(*this);

				Value *val = expr->cgData<LLVMData>()->value;

				unsigned int idx[1] = { (unsigned int)i };
				cg->value = Builder.CreateInsertValue(cg->value, val, idx);
			}
		}
	}
}

void LLVMGenerator::visit(Index &n)
{
	if (n.doneCodegen()) return;

	LLVMData *cg = n.cgData<LLVMData>();

	n.source()->accept(*this);

	for (auto i : n.indices())
		i->accept(*this);

	n.expr()->accept(*this);

	cg->value = n.expr()->cgData<LLVMData>()->value;
	cg->type = n.expr()->cgData<LLVMData>()->type;
}

void LLVMGenerator::visit(TypeDecl &n)
{
	if (n.doneCodegen()) return;

	TypeExpr *ty = n.type();

	ty->accept(*this);

	Struct *s = ty->asStruct();
	if (s)
	{
		LLVMData *cg = n.cgData<LLVMData>();

		SmallVector<llvm::Metadata*, 8> elements;
		for (auto &member : s->dataMembers())
			elements.push_back(member.decl->cgData<LLVMData>()->divalue);

		cg->discope = cg->ditype = DBuilder->createStructType(scopeCg()->discope, n.name(), scopeCg()->file(), n.getLoc().line, ty->size(), ty->alignment(), 0, nullptr, DBuilder->getOrCreateArray(elements), 0, nullptr, n.mangledName());

		ty->cgData<LLVMData>()->ditype = cg->ditype;
	}
}

void LLVMGenerator::visit(ValDecl &n)
{
	if (n.doneCodegen()) return;

	LLVMData *cg = n.cgData<LLVMData>();

	n.type()->accept(*this);
	n.value()->accept(*this);

	TypeExpr *valueType = n.value()->type();
	assert(valueType == n.type() || valueType->isVoid()); // is this true?

	if (valueType->asFunction())
	{
		llvm::Function *func = (llvm::Function*)n.value()->cgData<LLVMData>()->value;
		func->setName(n.name());
//		func->setName(n.mangledName());
		func->setLinkage(Function::ExternalLinkage);
		cg->value = func;

		// update function DI with proper names
		DISubprogram *di = dyn_cast<DISubprogram>(n.value()->cgData<LLVMData>()->divalue);
		cg->divalue = cg->discope = di;
	}
	else if (valueType->isVoid() && n.type()->asFunction())
	{
		llvm::FunctionType *sig = (llvm::FunctionType*)n.type()->cgData<LLVMData>()->type;

		// TODO: use n.mangledName()
		Function *function = Function::Create(sig, Function::AvailableExternallyLinkage, n.name(), TheModule.get());
//		function->setCallingConv(CallingConv::X86_VectorCall)
		function->addFnAttr(Attribute::AttrKind::NoUnwind);
//		function->addFnAttr(Attribute::AttrKind::UWTable);
		cg->value = function;

		// HACK: WOAH MADHAX!!!! probably need a prototype node, or a generally better way of dealing with prototypes...
		// since the expression evaluates the declarations value (void), let's just stuff the function proto there...
		n.value()->cgData<LLVMData>()->value = function;

		DISubprogram *di = DBuilder->createFunction(scopeCg()->discope, n.name(), n.mangledName(), scopeCg()->file(), n.getLoc().line, dyn_cast<DISubroutineType>(n.type()->cgData<LLVMData>()->ditype), false, true, 0, 0, false);
		function->setSubprogram(di);
		cg->divalue = di;
	}
	else
	{
		// TODO: data variables
	}
}

void LLVMGenerator::visit(VarDecl &n)
{
	if (n.doneCodegen()) return;

	LLVMData *cg = n.cgData<LLVMData>();

	n.type()->accept(*this);

	llvm::Type *typeCg = n.targetType()->cgData<LLVMData>()->type;

	FunctionType *pFunc = n.type()->asFunction();
	if (pFunc)
	{
		// function pointer
		assert(false);
	}
	else
	{
		Scope *s = scope();

		// TODO: support static; insert in global scope with local mangling...

		if (dynamic_cast<Module*>(s))
		{
			GlobalVariable *global = TheModule->getGlobalVariable(n.name());
			if (global != nullptr)
			{
				// already exists!!
				assert(false);
			}

			TypeExpr *targetTy = n.targetType();

			cg->value = TheModule->getOrInsertGlobal(n.name(), typeCg);
			global = TheModule->getGlobalVariable(n.name());
			global->setAlignment(targetTy->alignment());
			global->setLinkage(GlobalValue::LinkageTypes::InternalLinkage);

			llvm::Constant *constant = nullptr;
			if (!n.init()->type()->isVoid())
			{
				n.init()->accept(*this);
				llvm::Value *val = n.init()->cgData<LLVMData>()->value;
				constant = llvm::dyn_cast<llvm::Constant>(val);
			}
			else
			{
				Type *ty = targetTy->cgData<LLVMData>()->type;
				if (ty->isPointerTy())
					constant = ConstantPointerNull::get(dyn_cast<llvm::PointerType>(ty));
				else if (ty->isAggregateType())
					constant = ConstantAggregateZero::get(ty);
				else if (ty->isFloatingPointTy())
					constant = ConstantFP::get(ty, 0.0);
				else if (ty->isIntegerTy())
					constant = ConstantInt::get(ty, 0);
				else
					assert(false);
			}
			global->setInitializer(constant);

			Expr *expr = n.value();
			expr->accept(*this);

			cg->divalue = DBuilder->createGlobalVariable(s->cgData<LLVMData>()->discope, n.name(), n.mangledName(), s->cgData<LLVMData>()->file(), n.getLoc().line, targetTy->cgData<LLVMData>()->ditype, true, constant);
		}
		else if (dynamic_cast<Struct*>(scope()))
		{
			cg->type = typeCg;
		}
		else
		{
			Expr *arrayLen = nullptr;
			Tuple *tup = dynamic_cast<Tuple*>(n.targetType());
			if (tup && tup->isSequence() && tup->isDynamicSize())
			{
				arrayLen = tup->dynamicSize();
				arrayLen->accept(*this);
			}

			AllocaInst *alloc = Builder.CreateAlloca(typeCg, arrayLen ? arrayLen->cgData<LLVMData>()->value : nullptr, n.name() + ".addr");
			alloc->setAlignment(n.targetType()->alignment());

			cg->value = alloc;
			if (!n.init()->type()->isVoid())
			{
				n.init()->accept(*this);
				llvm::Value *val = n.init()->cgData<LLVMData>()->value;

				// HAX: if assigning SizeT_Type to Ptr
				if (n.targetType()->asPointer() && n.init()->type()->asPrimitive() && n.init()->type()->asPrimitive()->type() == SizeT_Type)
					val = Builder.CreateIntToPtr(val, typeCg);

				StoreInst *store = Builder.CreateStore(val, cg->value, false);
				store->setAlignment(alloc->getAlignment());
			}

			Expr *expr = n.value();
			expr->accept(*this);
		}
	}
}

/*
void LLVMGenerator::visit(PrototypeDecl &n)
{
	if (n.doneCodegen()) return;

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
	if (n.doneCodegen()) return;

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
//		false, // internal linkage
//		true, // definition
//		ScopeLine,
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
*/


//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

//Function *LLVMGenerator::getFunction(std::string Name)
//{
//	// First, see if the function has already been added to the current module.
//	if (auto *F = TheModule->getFunction(Name))
//		return F;
//
//	// If not, check whether we can codegen the declaration from some existing
//	// prototype.
//	auto FI = FunctionProtos.find(Name);
//	if (FI != FunctionProtos.end())
//		return (Function*)visitValue(FI->second.get());
//
//	// If no existing prototype exists, return null.
//	return nullptr;
//}

void LLVMGenerator::emitLocation(Node *node)
{
	if (!node)
		return Builder.SetCurrentDebugLocation(DebugLoc());

	DIScope *s = scopeCg()->discope;
	Builder.SetCurrentDebugLocation(DebugLoc::get(node->getLine(), node->getCol(), s));
}

}
