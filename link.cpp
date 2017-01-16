#include "src/mlang.h"

#include <stdio.h>
#include <cstdio>
#include <iostream>
#include <memory>
#include <stdexcept>

namespace m {

void Link(Compiler &compiler)
{
	const char *link = "C:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\VC\\bin\\link.exe";
	FILE *f = NULL; fopen_s(&f, link, "r");
	if (f)
	{
		fclose(f);
		compiler.libPaths.push_back("\"C:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\VC\\lib\\amd64\"");
		compiler.libPaths.push_back("\"C:\\Program Files (x86)\\Windows Kits\\8.1\\Lib\\winv6.3\\um\\x64\"");
		compiler.libPaths.push_back("\"C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.10240.0\\ucrt\\x64\"");
	}
	else
		link = "link.exe";

	std::string args = "/OUT:\"" + compiler.outFile + "\" /MACHINE:X64 /ERRORREPORT:PROMPT /NOLOGO /DYNAMICBASE /SUBSYSTEM:CONSOLE";
	for (auto &libPath : compiler.libPaths)
		args += " /LIBPATH:" + libPath;
	for (auto &lib : compiler.libs)
		args += " " + lib;
	if (!compiler.objFile.empty())
		args += " \"" + compiler.objFile + "\"";
	std::string cmd = std::string("\"\"") + link + std::string("\"") + " " + args + "\"";

	int result = system(cmd.c_str());
	if (result)
	{
		// link failed!!
		//...
	}
}

}
