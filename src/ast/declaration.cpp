#include "ast.h"
#include "astvisitor.h"
#include "error.h"

namespace m {

void ModuleDecl::accept(ASTVisitor &v) { v.visit(*this); }
void ImportDecl::accept(ASTVisitor &v) { v.visit(*this); }
void TypeDecl::accept(ASTVisitor &v) { v.visit(*this); }
void ValDecl::accept(ASTVisitor &v) { v.visit(*this); }
void VarDecl::accept(ASTVisitor &v) { v.visit(*this); }


//***********************
//** Declaration nodes **
//***********************

Node *ModuleDecl::getMember(String name)
{
	Declaration *decl = _module->getDecl(name, true);
	if (decl)
		return decl;
	return Declaration::getMember(name);
}

Node *ImportDecl::getMember(String name)
{
	Declaration *decl = _module->getDecl(name, true);
	if (decl)
		return decl;
	return Declaration::getMember(name);
}

raw_ostream &TypeDecl::dump(raw_ostream &out, int ind)
{
	Declaration::dump(out << "type: " << str_ref(_name), ind) << "\n";
	++ind;
	if (_type)
		_type->dump(indent(out, ind) << "as: ", ind + 1);
	return out;
}

MutableString64 ValDecl::stringof() const
{
	return MutableString64(Concat, _owner->module()->stringof(), '.', _name);
}

MutableString64 ValDecl::mangleof() const
{
	MutableString64 m = _owner->module()->mangleof();
	m.append(std::to_string(_name.size()), _name);
	FunctionType *fn = _type->asFunction();
	if (!fn)
		return m;
	for (auto &a : fn->argTypes())
		m.append(a->mangleof());
	return m;
}

raw_ostream &ValDecl::dump(raw_ostream &out, int ind)
{
	Declaration::dump(out << "def: #" << str_ref(_name), ind) << "\n";
	++ind;
	if (_type)
		_type->dump(indent(out, ind) << "type: ", ind + 1);
	if (_value)
		_value->dump(indent(out, ind) << "val: ", ind + 1);
	return out;
}

MutableString64 VarDecl::stringof() const
{
	return MutableString64(Concat, _owner->module()->stringof(), '.', _name);
}

MutableString64 VarDecl::mangleof() const
{
	MutableString64 m = _owner->module()->mangleof();
	m.append(std::to_string(_name.size()), _name);
	FunctionType *fn = _type->asFunction();
	if (!fn)
		return m;
	for (auto &a : fn->argTypes())
		m.append(a->mangleof());
	return m;
}

raw_ostream &VarDecl::dump(raw_ostream &out, int ind)
{
	Declaration::dump(out << "var: #" << str_ref(_name), ind) << "\n";
	++ind;
	if (_valType)
		_valType->dump(indent(out, ind) << "type: ", ind + 1);
	if (_init)
		_init->dump(indent(out, ind) << "init: ", ind + 1);
	return out;
}

}
