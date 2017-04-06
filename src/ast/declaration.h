//***********************
//** Declaration nodes **
//***********************

class ModuleDecl : public Declaration
{
	friend class Semantic;

	Module *_module = nullptr;

public:
	ModuleDecl(SharedString name, Array<Node*> attrs, SourceLocation loc)
		: Node(loc), Declaration(name, std::move(attrs), loc)
	{
	}

	bool didReturn() const override { return false; }

	MutableString64 stringof() const override { return _name; assert(false); } // TODO: test me!
	MutableString64 mangleof() const override { return MutableString64(Concat, "_M", _name); assert(false); } // TODO: test me!

	Node *getMember(String name) override;

	Module* module() { return _module; }
	void setModule(Module *module) { _module = module; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override {
		return Statement::dump(out << "module: #" << str_ref(_name) << '\n', ind);
	}
};

class ImportDecl : public Declaration
{
	friend class Semantic;

	Module *_module = nullptr;

public:
	ImportDecl(SharedString name, Array<Node*> attrs, SourceLocation loc)
		: Node(loc), Declaration(name, std::move(attrs), loc)
	{
	}

	bool didReturn() const override { return false; }

	MutableString64 stringof() const override { return _name; assert(false); } // TODO: test me!
	MutableString64 mangleof() const override { return MutableString64(Concat, "_M", _name); assert(false); } // TODO: test me!

	Node *getMember(String name) override;

	Module* module() { return _module; }
	void setModule(Module *module) { _module = module; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override {
		return Statement::dump(out << "import: #" << str_ref(_name) << '\n', ind);
	}
};

class TypeDecl : public Declaration
{
	friend class Semantic;
protected:
	TypeExpr *_type;

public:
	TypeDecl(SharedString name, TypeExpr *type, Array<Node*> attrs, SourceLocation loc) // TODO: template args
		: Node(loc), Declaration(std::move(name), std::move(attrs), loc), _type(type)
	{}

	TypeExpr* type() { return _type; }

	MutableString64 stringof() const override { return _name; }
	MutableString64 mangleof() const override { return MutableString64(Concat, std::to_string(_name.size()), _name); }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class ValDecl : public Declaration
{
	friend class Semantic;
protected:
	TypeExpr *_type;
	Expr *_value;

public:
	ValDecl(SharedString name, TypeExpr *type, Expr *value, Array<Node*> attrs, SourceLocation loc)
		: Node(loc), Declaration(std::move(name), std::move(attrs), loc), _type(type), _value(value)
	{}

	TypeExpr* type() { return _type; }
	Expr* value() { return _value; }

	MutableString64 stringof() const override;
	MutableString64 mangleof() const override;

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};
class VarDecl : public ValDecl
{
	friend class Semantic;
	friend Statement* makeForEach(Array<ValDecl*>, Expr*, ScopeStatement*, SourceLocation);
protected:
	TypeExpr *_valType;
	Expr *_init;

public:
	VarDecl(SharedString name, TypeExpr *type, Expr *init, Array<Node*> attrs, SourceLocation loc)
		: Node(loc), ValDecl(std::move(name), nullptr, nullptr, std::move(attrs), loc), _valType(type), _init(init)
	{}

	TypeExpr* targetType() { return _valType; }
	Expr* init() { return _init; }

	MutableString64 stringof() const override;
	MutableString64 mangleof() const override;

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};
