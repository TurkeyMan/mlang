#include "codegen_llvm.h"

using namespace llvm;
using namespace llvm::orc;


LLVMGenerator::LLVMGenerator(::Module *_module)
	: ctx(getGlobalContext())
	, Builder(ctx)
	, module(_module)
	, TheJIT(llvm::make_unique<KaleidoscopeJIT>())
	, TheModule(llvm::make_unique<llvm::Module>(_module->name(), ctx))
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

	// Create the compile unit for the module.
	KSDbgInfo.TheCU = DBuilder->createCompileUnit(dwarf::DW_LANG_C, _module->filename(), ".", "M-Lang Compiler", 0, "", 0);
}


std::string Codegen(::Module *module)
{
	InitializeNativeTarget();
	InitializeNativeTargetAsmPrinter();
	InitializeNativeTargetAsmParser();

	LLVMGenerator *generator = new LLVMGenerator(module);

	module->accept(*generator);

	return generator->codegen();
}

std::string LLVMGenerator::codegen()
{
	// Finalize the debug info.
	DBuilder->finalize();

	// Print out all of the generated code.
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

	MemStream output;
	TheModule->print(output, nullptr);
	return output.take();
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

void LLVMGenerator::visit(::Module &n)
{
	Scope *old = scope;
	scope = n.scope();

	for (auto &s : scope->symbols())
		s.second->accept(*this);

	scope = old;
}

void LLVMGenerator::visit(ModuleStatement &n)
{
}

void LLVMGenerator::visit(ReturnStatement &n)
{
	if (n.expression())
	{
		n.expression()->accept(*this);
		Builder.CreateRet(n.expression()->cgData<LLVMData>()->value);
	}
	else
		Builder.CreateRetVoid();
}

void LLVMGenerator::visit(TypeExpr &n)
{
}

void LLVMGenerator::visit(PrimitiveType &n)
{
	LLVMData *cg = n.cgData<LLVMData>();
	if (!cg) return;

	switch (n.type())
	{
	case PrimType::v:
		cg->type = Type::getVoidTy(ctx); break;
	case PrimType::u1:
		cg->type = Type::getInt1Ty(ctx); break;
	case PrimType::i8:
	case PrimType::u8:
	case PrimType::c8:
		cg->type = Type::getInt8Ty(ctx); break;
	case PrimType::i16:
	case PrimType::u16:
	case PrimType::c16:
		cg->type = Type::getInt16Ty(ctx); break;
	case PrimType::i32:
	case PrimType::u32:
	case PrimType::c32:
		cg->type = Type::getInt32Ty(ctx); break;
	case PrimType::i64:
	case PrimType::u64:
		cg->type = Type::getInt64Ty(ctx); break;
	case PrimType::i128:
	case PrimType::u128:
		cg->type = Type::getInt128Ty(ctx); break;
	case PrimType::iz:
	case PrimType::uz:
		cg->type = Type::getInt64Ty(ctx); break; // TODO: depends on target arch!
	case PrimType::f16:
		cg->type = Type::getHalfTy(ctx); break;
	case PrimType::f32:
		cg->type = Type::getFloatTy(ctx); break;
	case PrimType::f64:
		cg->type = Type::getDoubleTy(ctx); break;
	case PrimType::f128:
		cg->type = Type::getFP128Ty(ctx); break;
//		cg->type = Type::getX86_FP80Ty(ctx); break;
//		cg->type = Type::getPPC_FP128Ty(ctx); break;
	default:
		assert(false);
	}
}

void LLVMGenerator::visit(TypeIdentifier &n)
{
}

void LLVMGenerator::visit(TupleType &n)
{
}

void LLVMGenerator::visit(Struct &n)
{
}

void LLVMGenerator::visit(::FunctionType &n)
{
	LLVMData *cg = n.cgData<LLVMData>();
	if (!cg) return;

	n.returnType()->accept(*this);
	llvm::Type *r = n.returnType()->cgData<LLVMData>()->type;

	TypeExprList argList = n.argTypes();
	std::vector<llvm::Type *> args;
	for (auto a : argList)
	{
		a->accept(*this);
		args.push_back(a->cgData<LLVMData>()->type);
	}

	cg->type = llvm::FunctionType::get(r, args, false);
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
	LLVMData *cg = n.cgData<LLVMData>();
	if (!cg) return;

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
	case PrimType::iz: // TODO: may not be 64 bits!
	case PrimType::i64:
		cg->value = ConstantInt::get(ctx, APInt(64, n.getInt(), true)); break;
	case PrimType::uz: // TODO: may not be 64 bits!
	case PrimType::u64:
		cg->value = ConstantInt::get(ctx, APInt(64, n.getUint(), false)); break;
	case PrimType::i128:
		// TODO: literal is not big enough...
		cg->value = ConstantInt::get(ctx, APInt(128, n.getInt(), true)); break;
	case PrimType::u128:
		// TODO: literal is not big enough...
		cg->value = ConstantInt::get(ctx, APInt(128, n.getUint(), false)); break;
	case PrimType::f16:
	case PrimType::f32:
	case PrimType::f64:
	case PrimType::f128:
		cg->value = ConstantFP::get(ctx, APFloat(n.getFloat())); break;
	case PrimType::v:
		cg->value = nullptr; break;
	default:
		assert(0);
	}
}

void LLVMGenerator::visit(ArrayLiteralExpr &n)
{
}

void LLVMGenerator::visit(FunctionLiteralExpr &n)
{
}

void LLVMGenerator::visit(IdentifierExpr &n)
{
	LLVMData *cg = n.cgData<LLVMData>();
	if (!cg) return;

	Declaration *decl = n.target();
	decl->accept(*this);

	cg->value = Builder.CreateLoad(decl->cgData<LLVMData>()->value, n.getName());
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

// none=0, trunc=1, se=2, ze=3, fup=4, fdown=5, f2u=6, f2i=7, u2f=8, i2f=9, inez=10, fnez=11, invalid=-1
static char cast_flags[] =
{	/* from     v  u1 b  ub c  s  us wc i  ui dc z  uz l  ul ct uc h  f  d  x */
	/* v    */  0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
	/* u1   */ -1, 0, 3, 3,-1, 3, 3,-1, 3, 3,-1, 3, 3, 3, 3, 3, 3, 8, 8, 8, 8,
	/* i8   */ -1,10, 0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 9, 9, 9, 9,
	/* u8   */ -1,10, 0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 8, 8, 8, 8,
	/* c8   */ -1,10, 0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 8, 8, 8, 8,
	/* i16  */ -1,10, 1, 1, 1, 0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 9, 9, 9, 9,
	/* u16  */ -1,10, 1, 1, 1, 0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 8, 8, 8, 8,
	/* c16  */ -1,10, 1, 1, 1, 0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 8, 8, 8, 8,
	/* i32  */ -1,10, 1, 1, 1, 1, 1, 1, 0, 0, 0, 2, 2, 2, 2, 2, 2, 9, 9, 9, 9,
	/* u32  */ -1,10, 1, 1, 1, 1, 1, 1, 0, 0, 0, 3, 3, 3, 3, 3, 3, 8, 8, 8, 8,
	/* c32  */ -1,10, 1, 1, 1, 1, 1, 1, 0, 0, 0, 3, 3, 3, 3, 3, 3, 8, 8, 8, 8,
	/* iz   */ -1,10, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 2, 2, 9, 9, 9, 9,
	/* uz   */ -1,10, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 3, 3, 8, 8, 8, 8,
	/* i64  */ -1,10, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 2, 2, 9, 9, 9, 9,
	/* u64  */ -1,10, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 3, 3, 8, 8, 8, 8,
	/* i128 */ -1,10, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 9, 9, 9, 9,
	/* u128 */ -1,10, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 8, 8, 8, 8,
	/* f16  */ -1,11, 7, 6, 6, 7, 6, 6, 7, 6, 6, 7, 6, 7, 6, 7, 6, 0, 4, 4, 4,
	/* f32  */ -1,11, 7, 6, 6, 7, 6, 6, 7, 6, 6, 7, 6, 7, 6, 7, 6, 5, 0, 4, 4,
	/* f64  */ -1,11, 7, 6, 6, 7, 6, 6, 7, 6, 6, 7, 6, 7, 6, 7, 6, 5, 5, 0, 4,
	/* f128 */ -1,11, 7, 6, 6, 7, 6, 6, 7, 6, 6, 7, 6, 7, 6, 7, 6, 5, 5, 5, 0,
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

void LLVMGenerator::visit(TypeConvertExpr &n)
{
	LLVMData *cg = n.cgData<LLVMData>();
	if (!cg) return;

	Expr *expr = n.expr();
	expr->accept(*this);
	TypeExpr *type = n.type();
	type->accept(*this);

	LLVMData *exprCg = expr->cgData<LLVMData>();
	LLVMData *typeCg = type->cgData<LLVMData>();

	PrimitiveType *pt = dynamic_cast<PrimitiveType*>(type);
	if (pt)
	{
		TypeExpr *from_type = expr->type();

		PrimType src;
		PrimitiveType *from_pt = dynamic_cast<PrimitiveType*>(from_type);
		if (!from_pt)
		{
			// TODO: can from_type be wrangled into a primitive type??
			assert(false);
		}
		else
			src = from_pt->type();

		int castType = cast_flags[(int)src*21 + (int)pt->type()];
		assert(castType >= 0);

		if (castType == 0)
			cg->value = exprCg->value;
		else if (castType == 10)
			cg->value = Builder.CreateICmpNE(exprCg->value, ConstantInt::get(ctx, APInt(64, 0, false)), "u1cast");
		else if (castType == 11)
			cg->value = Builder.CreateFCmpONE(exprCg->value, ConstantFP::get(ctx, APFloat(0.0)), "u1cast");
		else
			cg->value = Builder.CreateCast(cast_types[castType], exprCg->value, typeCg->type, "cast");
	}
}

void LLVMGenerator::visit(UnaryExpr &n)
{
	LLVMData *cg = n.cgData<LLVMData>();
	if (!cg) return;

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
			assert(isBinary(primType));
			cg->value = Builder.CreateXor(operandCg->value, (uint64_t)-1, "neg");
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
	LLVMData *cg = n.cgData<LLVMData>();
	if (!cg) return;

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
			else if (isUnsigned(primType))
				cg->value = Builder.CreateUDiv(lhCg->value, rhCg->value, "udiv");
			else
				cg->value = Builder.CreateSDiv(lhCg->value, rhCg->value, "sdiv");
			break;
		case BinOp::Mod:
			assert(primType > PrimType::u1);
			if (isFloat(primType))
				cg->value = Builder.CreateFRem(lhCg->value, rhCg->value, "fmod");
			else if (isUnsigned(primType))
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
		case BinOp::ASL:
			assert(isBinary(primType));
			cg->value = Builder.CreateShl(lhCg->value, rhCg->value, "asl");
			break;
		case BinOp::ASR:
			assert(isBinary(primType));
			cg->value = Builder.CreateAShr(lhCg->value, rhCg->value, "asr");
			break;
		case BinOp::LSR:
			assert(isBinary(primType));
			cg->value = Builder.CreateLShr(lhCg->value, rhCg->value, "lsr");
			break;
		case BinOp::BitAnd:
			assert(isBinary(primType));
			cg->value = Builder.CreateAnd(lhCg->value, rhCg->value, "and");
			break;
		case BinOp::BitOr:
			assert(isBinary(primType));
			cg->value = Builder.CreateOr(lhCg->value, rhCg->value, "or");
			break;
		case BinOp::BitXor:
			assert(isBinary(primType));
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
			assert(isBool(primType));
			PrimitiveType *lhpt = dynamic_cast<PrimitiveType*>(lhs->type());
			if (lhpt)
			{
				PrimType lhPrimType = lhpt->type();
				if (isFloat(lhPrimType))
					cg->value = Builder.CreateFCmpOEQ(lhCg->value, rhCg->value, "feq");
				else if (isBinary(lhPrimType))
					cg->value = Builder.CreateICmpEQ(lhCg->value, rhCg->value, "eq");
				else
					assert(false);
			}
			break;
		}
		case BinOp::Ne:
		{
			assert(isBool(primType));
			PrimitiveType *lhpt = dynamic_cast<PrimitiveType*>(lhs->type());
			if (lhpt)
			{
				PrimType lhPrimType = lhpt->type();
				if (isFloat(lhPrimType))
					cg->value = Builder.CreateFCmpONE(lhCg->value, rhCg->value, "fne");
				else if (isBinary(lhPrimType))
					cg->value = Builder.CreateICmpNE(lhCg->value, rhCg->value, "ne");
				else
					assert(false);
			}
			break;
		}
		case BinOp::Gt:
		{
			assert(isBool(primType));
			PrimitiveType *lhpt = dynamic_cast<PrimitiveType*>(lhs->type());
			if (lhpt)
			{
				PrimType lhPrimType = lhpt->type();
				if (isFloat(lhPrimType))
					cg->value = Builder.CreateFCmpOGT(lhCg->value, rhCg->value, "fgt");
				else if (isUnsigned(lhPrimType))
					cg->value = Builder.CreateICmpUGT(lhCg->value, rhCg->value, "ugt");
				else if (isSigned(lhPrimType))
					cg->value = Builder.CreateICmpSGT(lhCg->value, rhCg->value, "sgt");
				else
					assert(false);
			}
			break;
		}
		case BinOp::Ge:
		{
			assert(isBool(primType));
			PrimitiveType *lhpt = dynamic_cast<PrimitiveType*>(lhs->type());
			if (lhpt)
			{
				PrimType lhPrimType = lhpt->type();
				if (isFloat(lhPrimType))
					cg->value = Builder.CreateFCmpOGE(lhCg->value, rhCg->value, "fge");
				else if (isUnsigned(lhPrimType))
					cg->value = Builder.CreateICmpUGE(lhCg->value, rhCg->value, "uge");
				else if (isSigned(lhPrimType))
					cg->value = Builder.CreateICmpSGE(lhCg->value, rhCg->value, "sge");
				else
					assert(false);
			}
			break;
		}
		case BinOp::Lt:
		{
			assert(isBool(primType));
			PrimitiveType *lhpt = dynamic_cast<PrimitiveType*>(lhs->type());
			if (lhpt)
			{
				PrimType lhPrimType = lhpt->type();
				if (isFloat(lhPrimType))
					cg->value = Builder.CreateFCmpOLT(lhCg->value, rhCg->value, "flt");
				else if (isUnsigned(lhPrimType))
					cg->value = Builder.CreateICmpULT(lhCg->value, rhCg->value, "ult");
				else if (isSigned(lhPrimType))
					cg->value = Builder.CreateICmpSLT(lhCg->value, rhCg->value, "slt");
				else
					assert(false);
			}
			break;
		}
		case BinOp::Le:
		{
			assert(isBool(primType));
			PrimitiveType *lhpt = dynamic_cast<PrimitiveType*>(lhs->type());
			if (lhpt)
			{
				PrimType lhPrimType = lhpt->type();
				if (isFloat(lhPrimType))
					cg->value = Builder.CreateFCmpOLE(lhCg->value, rhCg->value, "fle");
				else if (isUnsigned(lhPrimType))
					cg->value = Builder.CreateICmpULE(lhCg->value, rhCg->value, "ule");
				else if (isSigned(lhPrimType))
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

void LLVMGenerator::visit(IndexExpr &n)
{
}

void LLVMGenerator::visit(CallExpr &n)
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

void LLVMGenerator::visit(IfStatement &n)
{
	n.cond()->accept(*this);
	LLVMData *cg = n.cond()->cgData<LLVMData>();

	Function *TheFunction = Builder.GetInsertBlock()->getParent();

	BasicBlock *thenBlock = BasicBlock::Create(ctx, "then", TheFunction);
	BasicBlock *elseBlock = n.elseStatements().length > 0 ? BasicBlock::Create(ctx, "else") : nullptr;
	BasicBlock *afterBlock = BasicBlock::Create(ctx, "continue");

	Builder.CreateCondBr(cg->value, thenBlock, elseBlock ? elseBlock : afterBlock);

	// emit 'then' block
	Scope *old = scope;
	scope = n.thenScope();
	Builder.SetInsertPoint(thenBlock);

	// emit statements...
	for (auto &s : n.thenStatements())
		s->accept(*this);

//	Value *thenV = block result expression; for if statement *expressions*

	Builder.CreateBr(afterBlock);
	thenBlock = Builder.GetInsertBlock();

	if (elseBlock)
	{
		// emit 'else' block
		scope = n.elseScope();
		TheFunction->getBasicBlockList().push_back(elseBlock);
		Builder.SetInsertPoint(elseBlock);

		// emit statements...
		for (auto &s : n.elseStatements())
			s->accept(*this);

//		Value *elseV = block result expression; for if statement *expressions*

		Builder.CreateBr(afterBlock);
		elseBlock = Builder.GetInsertBlock();
	}

	// continue
	scope = old;
	TheFunction->getBasicBlockList().push_back(afterBlock);
	Builder.SetInsertPoint(afterBlock);

	// saw we want the if statement to be an expression... then PHI node will select the 'result' from each block...
//	PHINode *PN = Builder.CreatePHI(llvm::Type::getDoubleTy(ctx), 2, "iftmp");
//	PN->addIncoming(thenV, thenBlock);
//	PN->addIncoming(elseV, elseBlock);
//	cg->value = PN;


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

void LLVMGenerator::visit(ValDecl &n)
{
	LLVMData *cg = n.cgData<LLVMData>();
	if (!cg) return;

	::FunctionType *funcType = dynamic_cast<::FunctionType*>(n.type());
	if (funcType)
	{
		// function declaration
		funcType->accept(*this);
		llvm::FunctionType *sig = (llvm::FunctionType*)funcType->cgData<LLVMData>()->type;

		Function *proto = Function::Create(sig, Function::ExternalLinkage, n.name(), TheModule.get());
		cg->value = proto;

		FunctionLiteralExpr *func = (FunctionLiteralExpr*)n.init();
		if (func)
		{
			Scope *old = scope;
			scope = func->scope();

			// Create a new basic block to start insertion into.
			BasicBlock *block = BasicBlock::Create(ctx, "entry", proto);
			Builder.SetInsertPoint(block);

			// Set names for all arguments.
			StatementList args = func->args();
			size_t i = 0;
			for (auto &a : proto->args())
			{
				VarDecl *arg = (VarDecl*)args[i++];

				a.setName(arg->name());

				AllocaInst *alloc = Builder.CreateAlloca(a.getType(), nullptr, a.getName() + ".addr");
				arg->cgData<LLVMData>()->value = alloc;

				// Create an alloca for this variable.
//				IRBuilder<> builder(&proto->getEntryBlock(), proto->getEntryBlock().begin());
//				AllocaInst *Alloca = builder.CreateAlloca(a.getType(), nullptr, a.getName());

//
//				// Create a debug descriptor for the variable.
//				DILocalVariable *D = DBuilder->createParameterVariable(
//					SP, Arg.getName(), ++ArgIdx, Unit, LineNo, KSDbgInfo.getDoubleTy(),
//					true);
//
//				DBuilder->insertDeclare(Alloca, D, DBuilder->createExpression(),
//					DebugLoc::get(LineNo, 0, SP),
//					Builder.GetInsertBlock());
//
				// Store the initial value into the alloca.
				Builder.CreateStore(&a, alloc);
//
//				// Add arguments to variable symbol table.
//				NamedValues[Arg.getName()] = Alloca;
			}
//
//			KSDbgInfo.emitLocation(Body.get());

			for (auto &s : func->statements())
			{
				s->accept(*this);
			}

//			if (Value *RetVal = Body->codegen())
//			{
//				// Finish off the function.
//				Builder.CreateRet(RetVal);
//
//				// Pop off the lexical block for the function.
//				KSDbgInfo.LexicalBlocks.pop_back();
//
//				// Validate the generated code, checking for consistency.
				verifyFunction(*proto);
//
//				return TheFunction;
//			}
//
//			// Error reading body, remove function.
//			TheFunction->eraseFromParent();
//
//			if (P.isBinaryOp())
//				BinopPrecedence.erase(Proto->getOperatorName());
//
//			// Pop off the lexical block for the function since we added it
//			// unconditionally.
//			KSDbgInfo.LexicalBlocks.pop_back();

			scope = old;
		}
	}
	else
	{
		// constant value
		assert(false);
	}
}

void LLVMGenerator::visit(VarDecl &n)
{
	LLVMData *cg = n.cgData<LLVMData>();
	if (!cg) return;

	::FunctionType *pFunc = dynamic_cast<::FunctionType*>(n.type());
	if (pFunc)
	{
		// function pointer
		assert(false);
	}
	else
	{
		n.type()->accept(*this);
		llvm::Type *pType = n.type()->cgData<LLVMData>()->type;

		if (dynamic_cast<::Module*>(scope->owner()))
		{
			cg->value = TheModule->getOrInsertGlobal(n.name(), pType);
		}
		else
		{
			// other scopes?
		}
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

/// CreateEntryBlockAlloca - Create an alloca instruction in the entry block of
/// the function.  This is used for mutable variables etc.
static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction, const std::string &VarName)
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
