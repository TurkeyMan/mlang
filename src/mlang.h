#pragma once
#if !defined(_MLANG_H)
#define _MLANG_H

#include "error.h"
#include "ast.h"

namespace m {

class Compiler
{
public:
	Array<SharedString> srcFiles;

	Array<SharedString> impPaths = { "" };
	Array<SharedString> libPaths;
	Array<SharedString> libs;

	SharedString linkCmd;
	SharedString linkArgs;

	SharedString objFile;
	SharedString outFile;
	SharedString irFile;

	SharedString runtime = "MD"; // multi-threaded dll - release

	Mode mode = Mode::CompileAndLink;
	int opt = 0;

	bool debug = false;

	m::Module *root;

	Array<m::Module*> modules;

	std::map<SharedString, m::TypeExpr*, string_less> types;
};

}

#endif // _MLANG_H
