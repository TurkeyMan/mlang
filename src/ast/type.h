

enum class PrimType
{
	v, u1, i8, u8, c8, i16, u16, c16, i32, u32, c32, i64, u64, i128, u128, f16, f32, f64, f128, __NumTypes
};

enum TypeMod : uint32_t
{
	Const = 1
};

enum class PtrType
{
	RawPtr,
	UniquePtr,
	BorrowedPtr,
	LValue
};

template <typename TypeNode, typename... Args>
struct MakeTypeHelper
{
	static TypeNode* makeType(Scope *scope, Args&&... args)
	{
		pNew = nullptr; // find

		if (!pNew)
		{
			TypeNode *pNew = new TypeNode(std::forward<Args>(args)...);
			// insert...
		}
		return pNew;
	}
};
template <typename... Args>
struct MakeTypeHelper<PrimitiveType, Args...>
{
	static PrimitiveType* makeType(Scope *scope, PrimType type, SourceLocation loc);
};

// compiler will init these when the target arch is known
extern PrimType SizeT_Type, SSizeT_Type, WCharT_Type;


//****************
//** Type nodes **
//****************

class PrimitiveType : public TypeExpr
{
	friend class Semantic;

	PrimType _type;
	Expr *_init = nullptr;

public:
	static PrimitiveType* get(PrimType type, SourceLocation loc) { return makeType<PrimitiveType>(nullptr, type, loc); }

	PrimitiveType(PrimType type, SourceLocation loc) : Node(loc), TypeExpr(loc), _type(type) {}
	~PrimitiveType() {}

	PrimType type() const { return _type; }
	Expr* init() const override;

	size_t size() const override { return typeBytes[(int)_type]; }
	size_t alignment() const override { return typeBytes[(int)_type]; }

	Node *getMember(String name) override;

	bool isSame(const TypeExpr *other) const override;
	ConvType convertible(const TypeExpr *target) const override;
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override;

	void accept(ASTVisitor &v) override;

	MutableString64 stringof() const override;
	MutableString64 mangleof() const override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class ModifiedType : public TypeExpr
{
	friend class Semantic;

	TypeMod _mod;
	TypeExpr *_type;
	Expr *_init = nullptr;

public:
	ModifiedType(TypeMod mods, TypeExpr *type, SourceLocation loc)
		: Node(loc), TypeExpr(loc), _mod(mods), _type(type) {}
	~ModifiedType() {}

	static ModifiedType* makeModified(TypeMod mods, TypeExpr *type, SourceLocation loc);

	TypeMod mod() const { return _mod; }
	TypeExpr *innerType() const { return _type; }
	Expr* init() const override { return _type->init(); }

	size_t size() const override { return _type->size(); }
	size_t alignment() const override { return _type->alignment(); }

	Node *getMember(String name) override;

	bool isSame(const TypeExpr *other) const override;
	ConvType convertible(const TypeExpr *target) const override;
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override;

	void accept(ASTVisitor &v) override;

	MutableString64 stringof() const override;
	MutableString64 mangleof() const override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class PointerType : public TypeExpr
{
	friend class Semantic;

	PtrType _type;
	TypeExpr *_pointerTarget;
	Expr *_init = nullptr;

public:
	PointerType(PtrType type, TypeExpr *targetType, SourceLocation loc)
		: Node(loc), TypeExpr(loc), _type(type), _pointerTarget(targetType) {}
	~PointerType() {}

	PtrType ptrType() const { return _type; }
	TypeExpr *targetType() const { return _pointerTarget; }
	Expr* init() const override { return _init; }

	size_t size() const override { return typeBytes[(int)SizeT_Type]; }
	size_t alignment() const override { return typeBytes[(int)SizeT_Type]; }

	Node *getMember(String name) override;

	bool isSame(const TypeExpr *other) const override;
	ConvType convertible(const TypeExpr *target) const override;
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override;
	bool canPromote(PtrType to) const;

	void accept(ASTVisitor &v) override;

	MutableString64 stringof() const override;
	MutableString64 mangleof() const override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class SliceType : public TypeExpr
{
	friend class Semantic;

	PtrType _type;
	TypeExpr *_elementType;
	Expr *_init = nullptr;

public:
	SliceType(TypeExpr *elementType, SourceLocation loc)
		: Node(loc), TypeExpr(loc), _elementType(elementType) {}
	~SliceType() {}

	PtrType ptrType() const { return _type; }
	TypeExpr *elementType() const { return _elementType; }
	Expr* init() const override { return _init; }

	size_t size() const override { return typeBytes[(int)SizeT_Type] * 2; }
	size_t alignment() const override { return typeBytes[(int)SizeT_Type]; }

	Node *getMember(String name) override;

	bool isSame(const TypeExpr *other) const override;
	ConvType convertible(const TypeExpr *target) const override;
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override;

	void accept(ASTVisitor &v) override;

	MutableString64 stringof() const override;
	MutableString64 mangleof() const override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class Struct : public TypeExpr, public Scope
{
	friend class Semantic;

	struct DataMember
	{
		size_t offset;
		VarDecl *decl;
	};

	Array<Statement*> _members;
	Array<DataMember> _dataMembers;

	Expr *_init = nullptr;

	size_t _sizeof = 0, _alignment = 0;

	SharedString _givenName;
	SourceLocation _defLoc = SourceLocation(0);

public:
	Struct(Array<Statement*> members, SourceLocation loc)
		: Node(loc), TypeExpr(loc), Scope(nullptr, loc), _members(std::move(members))
	{}

	Array<DataMember>& dataMembers() { return _dataMembers; }
	size_t memberIndex(String name);

	Expr* init() const override { return _init; }

	size_t size() const override { return _sizeof; }
	size_t alignment() const override { return _alignment; }

	SharedString& givenName() { return _givenName; }
	SourceLocation& defLoc() { return _defLoc; }

	Node *getMember(String name) override;

	bool isSame(const TypeExpr *other) const override;
	ConvType convertible(const TypeExpr *target) const override;
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override;

	void accept(ASTVisitor &v) override;

	MutableString64 stringof() const override;
	MutableString64 mangleof() const override { ice("TODO"); return nullptr; }

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class FunctionType : public TypeExpr
{
	friend class Semantic;

	TypeExpr *_returnType = nullptr;
	Array<TypeExpr*> _argTypes;
	Array<VarDecl*> _args;

public:
	FunctionType(TypeExpr *returnType, Array<TypeExpr*> args, SourceLocation loc)
		: Node(loc), TypeExpr(loc), _returnType(returnType), _argTypes(std::move(args)) {}
	FunctionType(TypeExpr *returnType, Array<VarDecl*> args, SourceLocation loc)
		: Node(loc), TypeExpr(loc), _returnType(returnType), _args(std::move(args)) {}
	~FunctionType() {}

	Expr* init() const override { ice("TODO"); return nullptr; }

	size_t size() const override { return 0; }
	size_t alignment() const override { return 0; }

	Node *getMember(String name) override { ice("TODO"); return nullptr; }

	bool isSame(const TypeExpr *other) const override;
	ConvType convertible(const TypeExpr *target) const override;
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override { ice("TODO"); return nullptr; }

	TypeExpr* returnType() const { return _returnType; }
	const Array<TypeExpr*>& argTypes() const { return _argTypes; }

	void accept(ASTVisitor &v) override;

	MutableString64 stringof() const override;
	MutableString64 mangleof() const override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class CVarArgType : public TypeExpr
{
	friend class Semantic;

public:
	CVarArgType(SourceLocation loc)
		: Node(loc), TypeExpr(loc) {}
	~CVarArgType() {}

	Expr* init() const override;

	size_t size() const override { return 0; }
	size_t alignment() const override { return 0; }

	Node *getMember(String name) override { ice("TODO"); return nullptr; }

	bool isSame(const TypeExpr *other) const override { ice("TODO"); return false; }
	ConvType convertible(const TypeExpr *target) const override { ice("TODO"); return ConvType::NoConversion; }
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override { ice("TODO"); return nullptr; }

	void accept(ASTVisitor &v) override;

	MutableString64 stringof() const override;
	MutableString64 mangleof() const override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};
