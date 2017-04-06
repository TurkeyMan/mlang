#include "ast.h"
#include "astvisitor.h"
#include "error.h"

namespace m {

void Declaration::accept(ASTVisitor &v) { v.visit(*this); }
void ExpressionStatement::accept(ASTVisitor &v) { v.visit(*this); }
void ReturnStatement::accept(ASTVisitor &v) { v.visit(*this); }
void ScopeStatement::accept(ASTVisitor &v) { v.visit(*this); }
void IfStatement::accept(ASTVisitor &v) { v.visit(*this); }
void LoopStatement::accept(ASTVisitor &v) { v.visit(*this); }


Statement* makeForEach(Array<ValDecl*> iterators, Expr *range, ScopeStatement *body, SourceLocation loc)
{
	assert(false); // TODO!!

				   // only [value], or [key, value] are allowed!
	assert(iterators.length <= 2);

	// assign void initialisation for counters (initialised at head of loop body)
	for (auto &i : iterators)
		((VarDecl*)i)->_init = new PrimitiveLiteralExpr(PrimType::v, 0ull, loc);

	VarDecl *r = new VarDecl("__loop_range", nullptr, range, Array<Node*>(), loc);
	iterators = Array<ValDecl*>(Concat, r, iterators);

	Expr *cond = nullptr; // cast(bool)__loop_range.empty()

	Array<Statement*> entry;
	if (iterators.length)
	{
		if (iterators.length == 3)
		{
			// iterators[1] = __loop_range.front.key;
			// entry = entry.append(assignment);
		}

		// iterators[iterators.length-1] = __loop_range.front.value;
		// entry = entry.append(assignment);
	}
	body->_statements = entry.append(body->_statements);

	Expr *increment = nullptr; // __loop_range = __loop_range.popFront();

	return new LoopStatement(std::move(iterators), cond, Array<Expr*>(Concat, increment), body, loc);
}


//*********************
//** Statement nodes **
//*********************

const SharedString& Declaration::mangledName() const
{
	if (_mangledName.empty())
	{
		for (auto a : _attributes)
		{
			Identifier *i = dynamic_cast<Identifier*>(a);
			if (i && i->getName().eq("extern_c"))
			{
				_mangledName = name();
				return _mangledName;
			}
		}
		_mangledName = SharedString(Concat, "_M", mangleof()); // TODO: include scope...
	}
	return _mangledName;
}

Node *Declaration::getMember(String name)
{
	if (name.eq("mangleof"))
		return Tuple::makeStringLiteral(mangledName(), PrimType::c8, false, SourceLocation(0));
	return Statement::getMember(name);
}

raw_ostream &ExpressionStatement::dump(raw_ostream &out, int ind)
{
	return _expression->dump(out, ind);
}

raw_ostream &ReturnStatement::dump(raw_ostream &out, int ind)
{
	if (_expression)
	{
		Statement::dump(out << "return: ", ind);
		return _expression->dump(out, ind + 1);
	}
	else
		return Statement::dump(out << "return\n", ind);
}

raw_ostream &ScopeStatement::dump(raw_ostream &out, int ind)
{
	for (auto s : _statements)
		s->dump(out, ind);
	return out;
}

raw_ostream &IfStatement::dump(raw_ostream &out, int ind)
{
	Statement::dump(out << "if\n", ind);
	ind++;
	if (_initStatements.length > 0)
	{
		indent(out, ind) << "init: {\n";
		for (auto s : _initStatements)
			s->dump(indent(out, ind + 1), ind + 1);
		indent(out, ind) << "}\n";
	}
	_cond->dump(indent(out, ind) << "cond: ", ind + 1);
	indent(out, ind) << "then: {\n";
	_then->dump(indent(out, ind + 1), ind + 1);
	indent(out, ind) << "}\n";
	if (_else)
	{
		indent(out, ind) << "else: {\n";
		_else->dump(indent(out, ind + 1), ind + 1);
		indent(out, ind) << "}\n";
	}
	return out;
}

raw_ostream &LoopStatement::dump(raw_ostream &out, int ind)
{
	Statement::dump(out << "loop\n", ind);
	ind++;
	if (_iterators.length)
	{
		indent(out, ind) << "iterators: {\n";
		for (auto i : _iterators)
			i->dump(indent(out, ind + 1), ind + 1);
		indent(out, ind) << "}\n";
	}
	if (_cond)
		_cond->dump(indent(out, ind) << "while: ", ind + 1);
	if (_increments.length)
	{
		indent(out, ind) << "increments: {\n";
		for (auto i : _increments)
			i->dump(indent(out, ind + 1), ind + 1);
		indent(out, ind) << "}\n";
	}
	indent(out, ind) << "body: {\n";
	_body->dump(indent(out, ind + 1), ind + 1);
	indent(out, ind) << "}\n";
	return out;
}

}
