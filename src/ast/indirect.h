//******************************************************
//** Indirect nodes, may resolve expressions or types **
//******************************************************

class UnknownExpr : public AmbiguousExpr
{
	friend class Semantic;

	Node *_node;

public:
	UnknownExpr(Node *node, SourceLocation loc)
		: Node(loc), AmbiguousExpr(loc), _node(node) {}

	Node *node() const { return _node; }

	bool isType() const override { return dynamic_cast<TypeExpr*>(_node) != nullptr; }
	bool isExpr() const override { return dynamic_cast<Expr*>(_node) != nullptr; }

	MutableString64 stringof() const override { return _node->stringof(); }
	MutableString64 mangleof() const override { return _node->mangleof(); }

	Node *getMember(String name) override { return _node->getMember(name); }

	// expr overrides
	TypeExpr* type() override { return dynamic_cast<Expr*>(_node)->type(); }
	Expr* constEval() override { return dynamic_cast<Expr*>(_node)->constEval(); };

	Expr* resolveExpr() override { return dynamic_cast<Expr*>(_node); }
	TypeExpr* resolveType() override { return dynamic_cast<TypeExpr*>(_node); }

	// type overrides
	Expr* init() const override { return dynamic_cast<TypeExpr*>(_node)->init(); }

	size_t size() const override { return dynamic_cast<TypeExpr*>(_node)->size(); }
	size_t alignment() const override { return dynamic_cast<TypeExpr*>(_node)->alignment(); }

	bool isSame(const TypeExpr *other) const override { return dynamic_cast<TypeExpr*>(_node)->isSame(other); }
	ConvType convertible(const TypeExpr *target) const override { return dynamic_cast<TypeExpr*>(_node)->convertible(target); }
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override { return dynamic_cast<TypeExpr*>(_node)->makeConversion(expr, targetType, implicit); }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class Identifier : public AmbiguousExpr
{
	friend class Semantic;

	SharedString _name;

	ValDecl *_var = nullptr;
	TypeDecl *_type = nullptr;
	ModuleDecl *_module = nullptr;

public:
	Identifier(SharedString name, SourceLocation loc)
		: Node(loc), AmbiguousExpr(loc), _name(std::move(name)) {}

	const SharedString &getName() const { return _name; }

	Declaration* target() const { return _var ? (Declaration*)_var : (Declaration*)_type; }

	bool isExpr() const override { return _var != nullptr; }
	bool isType() const override { return _type != nullptr; }

	// expr overrides
	TypeExpr* type() override { return _var->type(); }
	Expr* constEval() override { return resolveExpr()->constEval(); };

	Expr* resolveExpr() override { return _var ? _var->value() : nullptr; }
	TypeExpr* resolveType() override { return _type ? _type->type() : nullptr; }

	// type overrides
	MutableString64 stringof() const override
	{
		if (isType())
			return _type->stringof();
		else if (_module)
			return _module->stringof();
		return _name;
	}
	MutableString64 mangleof() const override
	{
		if (isType())
			return _type->mangleof();
		assert(false); // TODO: what is the scope of the symbol?
		if (_module)
			return _module->mangleof();
		return _var->mangleof();
	}

	Expr* init() const override { return _type->type()->init(); }

	size_t size() const override { return _type->type()->size(); }
	size_t alignment() const override { return _type->type()->alignment(); }

	bool isSame(const TypeExpr *other) const override { return _type->type()->isSame(other); }
	ConvType convertible(const TypeExpr *target) const override { return _type->type()->convertible(target); }
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override { return _type->type()->makeConversion(expr, targetType, implicit); }

	Node *getMember(String name) override;

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class MemberLookup : public AmbiguousExpr
{
	friend class Semantic;

	SharedString _member;

	Node *_node;
	Node *_result;

public:
	MemberLookup(Node *node, SharedString member, SourceLocation loc)
		: Node(loc), AmbiguousExpr(loc), _member(std::move(member)), _node(node) {}

	Node* expr() const { return _result; }

	bool isType() const override;
	bool isExpr() const override;

	// expr overrides
	TypeExpr* type() override;
	Expr* constEval() override { return resolveExpr()->constEval(); };

	Expr* resolveExpr() override;
	TypeExpr* resolveType() override { return ((const MemberLookup*)this)->resolveType(); }
	TypeExpr* resolveType() const;

	// type overrides
	MutableString64 stringof() const override
	{
		if (isType())
			return _result->stringof();
		return MutableString64(Concat, _node->stringof(), '.', _member);
	}
	MutableString64 mangleof() const override
	{
		if (isType())
			return _result->mangleof();
		assert(false);
		return nullptr;
	}

	Expr* init() const override { return resolveType()->init(); }

	size_t size() const override { return resolveType()->size(); }
	size_t alignment() const override { return resolveType()->alignment(); }

	bool isSame(const TypeExpr *other) const override { return resolveType()->isSame(other); }
	ConvType convertible(const TypeExpr *target) const override { return resolveType()->convertible(target); }
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override { return resolveType()->makeConversion(expr, targetType, implicit); }

	Node *getMember(String name) override;

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class Tuple : public AmbiguousExpr
{
	friend class Semantic;

	Array<Node*> _elements;
	Array<size_t> _offsets;

	Node *_element = nullptr;
	Array<Expr*> _shape;
	Expr *_numElements = nullptr;

	bool allExpr = false, allTypes = false;
	size_t _size = 0, _alignment = 0;

	Tuple *_type = nullptr;
	Tuple *_init = nullptr;

public:
	Tuple(Array<Node*> elements, SourceLocation loc)
		: Node(loc), AmbiguousExpr(loc), _elements(std::move(elements))
	{
		// check if all elements are the same, if so, make it into an array...
		//...
	}
	Tuple(Node *element, Array<Expr*> shape, SourceLocation loc)
		: Node(loc), AmbiguousExpr(loc), _element(element), _shape(std::move(shape))
	{}

	static Tuple* makeStringLiteralQuoted(String str, SourceLocation loc);
	static Tuple* makeStringLiteral(String str, PrimType type, bool unescape, SourceLocation loc);

	const Array<Node*>& elements() const { return _elements; }
	const Array<size_t>& elementOffsets() const { return _offsets; }

	bool isSequence() const { return _element != nullptr; }
	Node* seqElement() { return _element; }
	const Array<Expr*>& shape() { return _shape; }
	int dimensions() const { return _element == nullptr ? 1 : _shape.length; }
	ptrdiff_t numElements(int dimension = -1) const;
	bool isDynamicSize() const { return numElements() == -1; }
	Expr* dynamicSize() { return _numElements; }

	bool isType() const override { return allTypes; }
	bool isExpr() const override { return allExpr; }

	bool isString() const { return isExpr() && _type->isSequence() && _type->_element->asType()->isSomeChar(); }

	// expr overrides
	TypeExpr* type() override;
	Expr* constEval() override { assert(false); return nullptr; };

	Expr* resolveExpr() override { return this; }
	TypeExpr* resolveType() override { return this; }

	// type overrides
	MutableString64 stringof() const override;
	MutableString64 mangleof() const override;

	Expr* init() const override;

	size_t size() const override { return _size; }
	size_t alignment() const override { return _alignment; }

	bool isSame(const TypeExpr *other) const override;
	ConvType convertible(const TypeExpr *target) const override;
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override;

	Node *getMember(String name) override; // length, etc

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;

private:
	void analyse();
};

class Index : public AmbiguousExpr
{
	friend class Semantic;

	Node *_node;
	Node *_result = nullptr;

	Array<Expr*> _indices;

public:
	Index(Node *node, Array<Expr*> indices, SourceLocation loc)
		: Node(loc), AmbiguousExpr(loc), _node(node), _indices(std::move(indices)) {}

	Node* source() { return _node; }
	const Array<Expr*>& indices() { return _indices; }

	Node* expr() const { return _result; }

	bool isType() const override { return _result->asType() != nullptr; }

	// expr overrides
	TypeExpr* type() override { return resolveExpr()->type(); }
	Expr* constEval() override { return resolveExpr()->constEval(); };

	Expr* resolveExpr() override { return _result->asExpr(); }
	TypeExpr* resolveType() override { return _result->asType(); }

	// type overrides
	MutableString64 stringof() const override
	{
		if (isType())
			return _result->stringof();
		MutableString64 t = MutableString64(Concat, _node->stringof(), '[');
		bool first = true;
		for (auto &i : _indices)
		{
			if (first)
				first = false;
			else
				t.append(", ");
			t.append(i->stringof());
		}
		return t.append(']');
	}
	MutableString64 mangleof() const override
	{
		if (isType())
			return _result->mangleof();
		assert(false);
		return nullptr;
	}

	Expr* init() const override { return dynamic_cast<TypeExpr*>(_result)->init(); }

	size_t size() const override { return dynamic_cast<TypeExpr*>(_result)->size(); }
	size_t alignment() const override { return dynamic_cast<TypeExpr*>(_result)->alignment(); }

	bool isSame(const TypeExpr *other) const override { return dynamic_cast<TypeExpr*>(_result)->isSame(other); }
	ConvType convertible(const TypeExpr *target) const override { return dynamic_cast<TypeExpr*>(_result)->convertible(target); }
	Expr* makeConversion(Expr *expr, TypeExpr *targetType, bool implicit = true) const override { return dynamic_cast<TypeExpr*>(_result)->makeConversion(expr, targetType, implicit); }

	Node *getMember(String name) override;

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};
