#pragma once
#if !defined(_MLANG_H)
#define _MLANG_H

#include "error.h"
#include "ast.h"

namespace m {

class Compiler
{
public:
	std::vector<std::string> srcFiles;

	std::string outFile;
	std::string irFile;

	std::string objFile;
	std::vector<std::string> libPaths;
	std::vector<std::string> libs;
	std::string runtime = "MD"; // multi-threaded dll - release

	Mode mode = Mode::CompileAndLink;
	int opt = 0;

	std::map<std::string, m::Module*> modules;

	std::map<std::string, m::TypeExpr*> types;
};

}

#endif // _MLANG_H
