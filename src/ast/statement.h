//*********************
//** Statement nodes **
//*********************

class Declaration : public Statement
{
	friend class Semantic;
protected:

	NodeList _attributes;

	SharedString _name;
	mutable SharedString _mangledName;

	Scope *_owner = nullptr;

public:
	const SharedString& name() const { return _name; }
	const SharedString& mangledName() const;

	Declaration(SharedString name, NodeList attrs, SourceLocation loc)
		: Statement(loc), _attributes(std::move(attrs)), _name(std::move(name))
	{}

	NodeList attributes() const { return _attributes; }
	void appendAttributes(NodeList attrs) { _attributes.append(attrs); }

	bool didReturn() const override { return false; }

	Node *getMember(String name) override;

	void accept(ASTVisitor &v) override;
};

class ExpressionStatement : public Statement
{
	friend class Semantic;

	Expr *_expression;

public:
	ExpressionStatement(Expr *expression, SourceLocation loc)
		: Node(loc), Statement(loc), _expression(expression)
	{
	}

	Expr* expression() const { return _expression; }

	bool didReturn() const override { return false; }

	MutableString64 stringof() const override { return _expression->stringof(); }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class ReturnStatement : public Statement
{
	friend class Semantic;

	Expr *_expression;

public:
	ReturnStatement(Expr *expression, SourceLocation loc)
		: Node(loc), Statement(loc), _expression(expression)
	{
	}

	Expr* expression() const { return _expression; }

	bool didReturn() const override { return true; }

	MutableString64 stringof() const override { return MutableString64(Concat, "return ", _expression->stringof()); }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class ScopeStatement : public Statement, public Scope
{
	friend class Semantic;
	friend Statement* makeForEach(DeclList, Expr*, ScopeStatement*, SourceLocation);

	StatementList _statements;

	bool _didReturn = false;

	// TODO: scope attributes

public:
	ScopeStatement(StatementList statements, Scope *parentScope, SourceLocation loc)
		: Node(loc), Statement(loc), Scope(parentScope, loc), _statements(statements)
	{}

	StatementList statements() const { return _statements; }
	bool didReturn() const override { return _didReturn; }

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class IfStatement : public Statement, public Scope
{
	friend class Semantic;

	Expr *_cond;
	ScopeStatement *_then = nullptr;
	ScopeStatement *_else = nullptr;

	DeclList _initStatements;

public:
	IfStatement(Expr *cond, ScopeStatement *thenStatements, ScopeStatement *elseStatements, DeclList initStatements, SourceLocation loc)
		: Node(loc), Statement(loc), Scope(nullptr, loc), _cond(cond), _then(thenStatements), _else(elseStatements), _initStatements(initStatements)
	{}

	Expr *cond() { return _cond; }

	DeclList initStatements() { return _initStatements; }

	ScopeStatement* thenStatements() { return _then; }
	ScopeStatement* elseStatements() { return _else; }

	bool thenReturned() const { return _then && _then->didReturn(); }
	bool elseReturned() const { return _else && _else->didReturn(); }
	bool didReturn() const override { return thenReturned() && elseReturned(); }

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};

class LoopStatement : public Statement, public Scope
{
	friend class Semantic;

	DeclList _iterators;
	Expr *_cond;
	ExprList _increments;

	ScopeStatement* _body;

public:
	LoopStatement(DeclList iterators, Expr *cond, ExprList increments, ScopeStatement* body, SourceLocation loc)
		: Node(loc), Statement(loc), Scope(nullptr, loc), _iterators(iterators), _cond(cond), _increments(increments), _body(body)
	{}

	DeclList iterators() { return _iterators; }

	Expr *cond() { return _cond; }

	ScopeStatement* body() { return _body; }
	ExprList incrementExpressions() { return _increments; }

	bool didReturn() const override { return _body->didReturn(); }

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};
