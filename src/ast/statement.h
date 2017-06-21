//*********************
//** Statement nodes **
//*********************

class Statement : public virtual Node
{
	friend class Semantic;
public:

	Statement(SourceLocation loc)
		: Node(loc)
	{}

	virtual bool didReturn() const = 0;
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
	friend Statement* makeForEach(Array<ValDecl*>, Expr*, ScopeStatement*, SourceLocation);

	Array<Statement*> _statements;

	bool _didReturn = false;

	// TODO: scope attributes

public:
	ScopeStatement(Array<Statement*> statements, Scope *parentScope, SourceLocation loc)
		: Node(loc), Statement(loc), Scope(parentScope, loc), _statements(std::move(statements))
	{}

	const Array<Statement*>& statements() const { return _statements; }
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

	Array<ValDecl*> _initStatements;

public:
	IfStatement(Expr *cond, ScopeStatement *thenStatements, ScopeStatement *elseStatements, Array<ValDecl*> initStatements, SourceLocation loc)
		: Node(loc), Statement(loc), Scope(nullptr, loc), _cond(cond), _then(thenStatements), _else(elseStatements), _initStatements(std::move(initStatements))
	{}

	Expr *cond() { return _cond; }

	const Array<ValDecl*>& initStatements() { return _initStatements; }

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

	Array<ValDecl*> _iterators;
	Expr *_cond;
	Array<Expr*> _increments;

	ScopeStatement* _body;

public:
	LoopStatement(Array<ValDecl*> iterators, Expr *cond, Array<Expr*> increments, ScopeStatement* body, SourceLocation loc)
		: Node(loc), Statement(loc), Scope(nullptr, loc), _iterators(std::move(iterators)), _cond(cond), _increments(std::move(increments)), _body(body)
	{}

	const Array<ValDecl*>& iterators() { return _iterators; }

	Expr *cond() { return _cond; }

	ScopeStatement* body() { return _body; }
	const Array<Expr*>& incrementExpressions() { return _increments; }

	bool didReturn() const override { return _body->didReturn(); }

	MutableString64 stringof() const override { assert(false); return nullptr; }
	MutableString64 mangleof() const override { assert(false); return nullptr; }

	void accept(ASTVisitor &v) override;

	raw_ostream &dump(raw_ostream &out, int ind) override;
};
