#include "parse.h"

void yyerror(const char *s);

using namespace m;

const char *my_dup(const char *s)
{
	size_t sz = strlen(s);
	char *buffer = (char*)malloc(sz + 1);
	strcpy_s(buffer, sz + 1, s);
	return buffer;
}

const char *join_strings(const char *s1, const char *s2)
{
	size_t sz1 = strlen(s1);
	size_t sz2 = strlen(s2);
	char *buffer = (char*)malloc(sz1 + sz2 + 2);
	strcpy_s(buffer, sz1 + sz2 + 2, s1);
	strcpy_s(buffer + sz1 + 1, sz2 + 1, s2);
	buffer[sz1] = '.';
	return buffer;
}

char32_t parse_char(const char *s, PrimType &type)
{
	Slice<const char> str(s);

	type = PrimType::v;
	if (str[str.length - 1] == 'w')
	{
		type = PrimType::c16;
		str.pop_back();
	}
	else if (str[str.length - 1] == 'd')
	{
		type = PrimType::c32;
		str.pop_back();
	}

	assert(str[0] == '\'' && str[str.length - 1] == '\'');

	str = str.slice(1, str.length - 1);

	char32_t c = str.pop_front_char();
	if (c == '\\')
	{
		c = str.pop_front_char();
		switch (c)
		{
		case '0':	c = '\0';	break;
		case 'a':	c = '\a';	break;
		case 'b':	c = '\b';	break;
		case 'f':	c = '\f';	break;
		case 'n':	c = '\n';	break;
		case 'r':	c = '\r';	break;
		case 't':	c = '\t';	break;
		case 'v':	c = '\v';	break;
		case 'x':
		{
			if (str.length < 2 || !detail::is_hex(str[0]) || !detail::is_hex(str[1]))
				yyerror("Invalid hexadecimal character.");
			c = (char32_t)str.pop_front(2).parse_int<16>();
			break;
		}
		case 'u':
		{
			if (str.length < 4 || !detail::is_hex(str[0]) || !detail::is_hex(str[1]) || !detail::is_hex(str[2]) || !detail::is_hex(str[3]))
				yyerror("Invalid hexadecimal character.");
			c = (char32_t)str.pop_front(4).parse_int<16>();
			break;
		}
		case 'U':
		{
			if (str.length < 8 ||
				!detail::is_hex(str[0]) || !detail::is_hex(str[1]) || !detail::is_hex(str[2]) || !detail::is_hex(str[3]) ||
				!detail::is_hex(str[4]) || !detail::is_hex(str[5]) || !detail::is_hex(str[6]) || !detail::is_hex(str[7]))
				yyerror("Invalid hexadecimal character.");
			c = (char32_t)str.pop_front(8).parse_int<16>();
			break;
		}
		default:
			break;
		}
	}

	if (!str.empty())
		yyerror("Invalid character literal.");
	if (type == PrimType::c16 && c >= 0x10000)
		yyerror("Character can not be encoded by wchar.");
	if (type == PrimType::v)
	{
		if (c >= 0x10000)
			type = PrimType::c32;
		else if (c >= 0x80)
			type = PrimType::c16;
		else
			type = PrimType::c8;
	}

	return c;
}
