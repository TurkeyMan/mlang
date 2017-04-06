//**********************
//** Expression nodes **
//**********************

class PrimitiveLiteralExpr : public Expr
{
	friend class Semantic;

	PrimType _type;
	PrimitiveType *_typeExpr = nullptr;

	union
	{
		double f;
		int64_t i;
		uint64_t u;
		char32_t c;
	};

public:
	PrimitiveLiteralExpr(PrimType type, uint64_t v, SourceLocation loc) : Node(loc), Expr(true, loc), _type(type), _typeExpr(PrimitiveType::get(type, loc)), u(v) {}
	PrimitiveLiteralExpr(PrimType type, int64_t v, SourceLocation loc) : Node(loc), Expr(true, loc), _type(type), _typeExpr(PrimitiveType::get(type, loc)), i(v) {}
	PrimitiveLiteralExpr(PrimType type, double v, SourceLocation loc) : Node(loc), Expr(true, loc), _type(type), _typeExpr(PrimitiveType::get(type, loc)), f(v) {}
	PrimitiveLiteralExpr(PrimType type, char32_t v, SourceLocation loc) : Node(loc), Expr(true, loc), _type(type), _typeExpr(PrimitiveType::get(type, loc)), c(v) {}
	PrimitiveLiteralExpr(uint64_t v, SourceLocation loc) : PrimitiveLiteralExpr(PrimType::u64, v, loc) {}
	PrimitiveLiteralExpr(int64_t v, SourceLocation loc) : PrimitiveLiteralExpr(PrimType::i64, v, loc) {}
	PrimitiveLiteralExpr(double v, SourceLocation loc) : PrimitiveLiteralExpr(PrimType::f64, v, loc) {}
	PrimitiveLiteralExpr(char32_t v, SourceLocation loc) : PrimitiveLiteralExpr(PrimType::c32, v, loc) {}
	PrimitiveLiteralExpr(bool b, SourceLocation loc) : PrimitiveLiteralExpr(PrimType::u1, b ? 1ull : 0ull, loc) {}

	PrimType primType() const { return _type; }
	double getFloat() const { return f; }
	int64_t getInt() const { return i; }
	uint64_t getUint() const { return u; }
	char32_t getChar() const { return c; }

	PrimitiveType* type() override { return _typeExpr; }
	PrimitiveLiteralExpr* constEval() override { return this; };

	MutableString64 stringof() const override;
	MutableString64 mangleof() const override { assert(false); return nullptr; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class AggregateLiteralExpr : public Expr
{
	friend class Semantic;

	TypeExpr *_type;
	Array<Expr*> _items;

public:
	AggregateLiteralExpr(Array<Expr*> items, TypeExpr *type, SourceLocation loc)
		: Node(loc), Expr(false, loc), _type(type), _items(std::move(items)) {}

	TypeExpr* type() override { return _type; }

	const Array<Expr*>& items() { return _items; }

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class FunctionLiteralExpr : public Expr, public Scope
{
	friend class Semantic;

	Array<Statement*> bodyStatements;

	Array<ValDecl*> _args;
	TypeExpr *returnType;
	bool inferReturnType;

	Array<TypeExpr*> argTypes;
	FunctionType *_type = nullptr;

	SharedString _givenName;
	SourceLocation _defLoc = SourceLocation(0);

public:
	FunctionLiteralExpr(Array<Statement*> bodyStatements, Array<ValDecl*> args, TypeExpr *returnType, SourceLocation loc)
		: Node(loc), Expr(true, loc), Scope(nullptr, loc), bodyStatements(std::move(bodyStatements)), _args(std::move(args)), returnType(returnType), inferReturnType(returnType == nullptr)
	{
		static int literalCount = 0;
		_givenName = SharedString(Concat, "fn_literal_", std::to_string(literalCount++));
	}

	FunctionType* type() override { return _type; }

	const Array<ValDecl*>& args() const { return _args; }
	const Array<Statement*>& statements() { return bodyStatements; }

	Node *getMember(String name) override { return Expr::getMember(name); }

	SharedString& givenName() { return _givenName; }
	SourceLocation& defLoc() { return _defLoc; }

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind)
	{
		out << "fn\n";

		ind++;
		indent(out, ind) << "return: ";
		if (returnType)
			returnType->dump(out, ind + 1);
		else
			out << "???\n";
		indent(out, ind) << "args: (\n";
		for (auto a : _args)
			a->dump(indent(out, ind + 1), ind + 1);
		indent(out, ind) << ")\n";
		indent(out, ind) << "body: {\n";
		for (auto s : bodyStatements)
			s->dump(indent(out, ind + 1), ind + 1);
		return indent(out, ind) << "}\n";
	}
};

class RefExpr : public Expr
{
	friend class Semantic;
public:
	enum Type
	{
		Direct,
		Member,
		Index,
		Absolute
	};

private:
	PointerType *_type = nullptr;

	Expr *_owner = nullptr;
	VarDecl *_target = nullptr;
	size_t _absolute = 0;
	size_t _element = 0;
	Expr *_index = nullptr;

	Type _refType;

public:

	RefExpr(VarDecl *target, RefExpr *owner, SourceLocation loc)
		: Node(loc), Expr(false, loc), _owner(owner), _target(target), _refType(owner ? Type::Member : Type::Direct) {}
	RefExpr(Expr *owner, size_t element, SourceLocation loc)
		: Node(loc), Expr(false, loc), _owner(owner), _element(element), _refType(Type::Index) {}
	RefExpr(Expr *owner, Expr *index, SourceLocation loc)
		: Node(loc), Expr(false, loc), _owner(owner), _index(index), _refType(Type::Index) {}
	RefExpr(PtrType ptrType, size_t target, TypeExpr *targetType, SourceLocation loc)
		: Node(loc), Expr(false, loc), _type(new PointerType(ptrType, targetType, loc)), _absolute(target), _refType(Type::Absolute) {}

	PointerType* type() override { return _type; }
	TypeExpr* targetType() const { return _type->targetType(); }
	Type refType() const { return _refType; }

	Expr* owner() { return _owner; }
	VarDecl* target() { return _target; }
	size_t address() { return _absolute; }
	size_t element() { return _element; }
	Expr* index() { return _index; }

	Node *getMember(String name) override;

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class DerefExpr : public Expr
{
	friend class Semantic;

	Expr *_expr;

public:
	DerefExpr(Expr *expr, SourceLocation loc)
		: Node(loc), Expr(false, loc), _expr(expr) {}

	Expr* expr() { return _expr; }
	TypeExpr* type() override { return _expr->type()->asPointer()->targetType(); }

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class TypeConvertExpr : public Expr
{
	friend class Semantic;

	Expr *_expr;
	TypeExpr *_newType;
	bool _implicit;

public:
	TypeConvertExpr(Expr *expr, TypeExpr *newType, bool implicit, SourceLocation loc)
		: Node(loc), Expr(expr->isConstant(), loc), _expr(expr), _newType(newType), _implicit(implicit) {}

	Expr* expr() { return _expr; }
	TypeExpr* type() override { return _newType; }

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

enum class UnaryOp
{
	Pos, Neg, BitNot, LogicNot, PreInc, PreDec
};
class UnaryExpr : public Expr
{
	friend class Semantic;

	UnaryOp _op;
	Expr *_operand;
	TypeExpr *_type = nullptr;

public:
	UnaryExpr(UnaryOp op, Expr *operand, SourceLocation loc)
		: Node(loc), Expr(operand->isConstant(), loc), _op(op), _operand(operand) {}

	UnaryOp op() { return _op; }
	Expr* operand() { return _operand; }

	TypeExpr* type() override { return _type; }

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

enum class BinOp
{
	None,
	Add, Sub, Mul, Div, Mod, Pow, Cat,
	SHL, ASR, LSR,
	BitAnd, BitOr, BitXor, LogicAnd, LogicOr, LogicXor,
	Eq, Ne, Gt, Ge, Lt, Le,
	Is, IsNot, __NumOps
};
class BinaryExpr : public Expr
{
	friend class Semantic;

	BinOp _op;
	Expr *_lhs, *_rhs;
	TypeExpr *_type = nullptr;

public:
	BinaryExpr(BinOp op, Expr *lhs, Expr *rhs, SourceLocation loc)
		: Node(loc), Expr(lhs->isConstant() && rhs->isConstant(), loc), _op(op), _lhs(lhs), _rhs(rhs) {}

	BinOp op() { return _op; }
	Expr* lhs() { return _lhs; }
	Expr* rhs() { return _rhs; }

	TypeExpr* type() override { return _type; }

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class CallExpr : public Expr
{
	friend class Semantic;

	Expr *_func;
	Array<Expr*> _callArgs;

public:
	CallExpr(Expr *func, Array<Expr*> callArgs, SourceLocation loc)
		: Node(loc), Expr(false, loc), _func(func), _callArgs(std::move(callArgs))
	{}

	TypeExpr* type() override
	{
		FunctionType *f = dynamic_cast<FunctionType*>(_func->type());
		return f->returnType();
	}

	Expr* function() { return _func; }
	const Array<Expr*>& callArgs() { return _callArgs; }

	Node* getMember(String name) override;

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class AssignExpr : public Expr
{
	friend class Semantic;

	Expr *_target;
	Expr *_expr;
	BinOp _op;

public:
	AssignExpr(Expr *target, Expr *expr, BinOp op, SourceLocation loc)
		: Node(loc), Expr(false, loc), _target(target), _expr(expr), _op(op)
	{}

	TypeExpr* type() override { return _target->type(); }

	Expr *target() { return _target; }
	Expr *expr() { return _expr; }

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class BindExpr : public Expr
{
	friend class Semantic;

	Expr *_target;
	Expr *_expr;

public:
	BindExpr(Expr *target, Expr *expr, SourceLocation loc)
		: Node(loc), Expr(false, loc), _target(target), _expr(expr)
	{}

	TypeExpr* type() override { return _target->type(); }

	Expr *target() { return _target; }
	Expr *expr() { return _expr; }

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};
