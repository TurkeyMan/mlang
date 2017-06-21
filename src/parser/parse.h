#pragma once
#if !defined(_PARSE_H)
#define _PARSE_H

#include "ast/ast.h"

const char *my_dup(const char *s);
const char *join_strings(const char *s1, const char *s2);
char32_t parse_char(const char *s, m::PrimType &type);

#endif // _PARSE_H
