#include "ast/ast.h"

template <typename T>
struct ArrayHolder
{
	static ArrayHolder<T> empty()
	{
		ArrayHolder<T> r;
		r.ptr = nullptr;
		r.length = 0;
		return r;
	}

	template <size_t N, bool S>
	static ArrayHolder<T> create(Array<T, N, S> &&arr)
	{
		if (arr.length == 0)
			return empty();
		if (!arr.is_allocated())
			return create(Array<T, 0>(std::move(arr)));
		ArrayHolder r;
		r.ptr = arr.ptr;
		r.length = arr.length;
		arr.ptr = nullptr;
		arr.length = 0;
		return r;
	}

	Array<T> get() const
	{
		Array<T> sa;
		sa.ptr = ptr;
		sa.length = length;
		ptr = (T*)(size_t)0xFEEEFEEEFEEEFEEE;
		length = (size_t)0xFEEEFEEEFEEEFEEE;
		return sa;
	}

	ArrayHolder<T> append(T item) const
	{
		return create(std::move(get().append(item)));
	}
	ArrayHolder<T> append(ArrayHolder<T> arr) const
	{
		return create(std::move(get().append(arr.get())));
	}

private:
	mutable size_t length;
	mutable T *ptr;
};

using NodeList = ArrayHolder<m::Node*>;
using ExprList = ArrayHolder<m::Expr*>;
using TypeExprList = ArrayHolder<m::TypeExpr*>;
using StatementList = ArrayHolder<m::Statement*>;
using DeclList = ArrayHolder<m::ValDecl*>;
