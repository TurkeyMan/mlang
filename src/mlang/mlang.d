module mlang;

import core.stdc.string;
import std.conv;
import std.stdio;

enum Type
{
	Int,
	UInt,
	Double,
	String,

	List,

	Id,
	TempalteId,
	Type,
	FunctionType,
	Instantiate,

	Const,
	Pointer,
	Ref,

	TypedId,

	Struct,
	Tuple,

	Array,

	ArrayLiteral,
	FunctionLiteral,

	Module,
	DefType,
	DefConst,
	Var,

	Elipsis,

	MemberLookup,
	Call,
	OpIndex,
	OpPostInc,
	OpPostDec,
	OpPreInc,
	OpPreDec,
	OpUnaryPlus,
	OpUnaryMinus,
	OpUnaryNot,
	OpUnaryComp,
	OpMul,
	OpDiv,
	OpMod,
	OpAdd,
	OpSub,
	OpConcat,
	OpASL,
	OpASR,
	OpLSR,
	OpLt,
	OpGt,
	OpLe,
	OpGe,
	OpEq,
	OpNe,
	OpBitAnd,
	OpBitXor,
	OpBitOr,
	OpAnd,
	OpXor,
	OpOr,
	OpAssign,
	OpBind,
	OpMulEq,
	OpDivEq,
	OpModEq,
	OpAddEq,
	OpSubEq,
	OpConcatEq,
	OpBitAndEq,
	OpBitXorEq,
	OpBitOrEq,
	OpAndEq,
	OpXorEq,
	OpOrEq,
	OpASLEq,
	OpASREq,
	OpLSREq,

	Return,
	Break,
}

struct Node
{
	Type t;
	union
	{
		string s;
		long i;
		ulong u;
		double f;
		struct
		{
			Node* l;
			Node* r;
		}
	}
}


Node*[2048] stack;
size_t depth = 0;

extern (C) Node* Push(Node *n)
{
	stack[depth++] = n;
	return n;
}
extern (C) Node* Pop()
{
	assert(depth > 0);
	return stack[--depth];
}
extern (C) Node* Top()
{
	assert(depth > 0);
	return stack[depth-1];
}

extern (C) Node* String(const char * s)
{
	Node *pN = new Node();
	pN.t = Type.String;
	pN.s = s[1..strlen(s)-1].idup;
	return pN;
}
extern (C) Node* Identifier(const char * i)
{
	Node *pN = new Node();
	pN.t = Type.Id;
	pN.s = i[0..strlen(i)].idup;
	return pN;
}
extern (C) Node* TypeId(const char * i)
{
	Node *pN = new Node();
	pN.t = Type.Type;
	pN.s = i[0..strlen(i)].idup;
	return pN;
}
extern (C) Node* Int(long i)
{
	Node *pN = new Node();
	pN.t = Type.Int;
	pN.i = i;
	return pN;
}
extern (C) Node* UInt(ulong i)
{
	Node *pN = new Node();
	pN.t = Type.UInt;
	pN.i = i;
	return pN;
}
extern (C) Node* Float(double f)
{
	Node *pN = new Node();
	pN.t = Type.Double;
	pN.f = f;
	return pN;
}
extern (C) Node* Add(Type type, Node* l =  null, Node* r = null)
{
	Node *pN = new Node();
	pN.t = type;
	pN.l = l;
	pN.r = r;
	return pN;
}
extern (C) Node* SetChildren(Node* node, Node* l, Node* r)
{
	if(l)
		node.l = l;
	if(r)
		node.r = r;
	return node;
}

string ListAsString(Node* n)
{
	return (n.l && n.l.t == Type.List ? ListAsString(n.l) : AsString(n.l)) ~
		(n.r ? ", " ~ AsString(n.r) : null);
}
string AsString(Node* n)
{
	if(!n)
		return null;

	switch(n.t) with(Type)
	{
		case Int:
			return n.i.to!string;
		case UInt:
			return (cast(ulong)(n.i)).to!string;
		case Double:
			return n.f.to!string;
		case String:
			return '\"' ~ n.s ~ '\"';
		case Id:
			return n.s;
		case List:
			return "[" ~ ListAsString(n) ~ "]";
		default:
			return "(" ~ n.t.to!string ~
				(n.l ? " " ~ AsString(n.l) : null) ~
				(n.r ? " " ~ AsString(n.r) : null) ~ ")";
	}
}
extern (C) void Print(Node *n)
{
	writeln(AsString(n));
}


extern(C) int run(const(char)*);

int main(string args[])
{
	return run(args[0].ptr);
}
