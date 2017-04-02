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

	MutableString<1024> args(Concat, "/OUT:\"", compiler.outFile, "\" /MACHINE:X64 /ERRORREPORT:PROMPT /NOLOGO /DYNAMICBASE /SUBSYSTEM:CONSOLE");

	if (compiler.debug)
	{
		args.append(" /DEBUG");
		if (compiler.outFile.ends_with_ic(".exe"))
			args.append(" /PDB:\"", compiler.outFile.slice(0, compiler.outFile.length - 4), ".pdb\"");
		else
			args.append(" /PDB:\"", compiler.outFile, ".pdb\"");
	}

	for (auto &libPath : compiler.libPaths)
		args.append(" /LIBPATH:", libPath);
	for (auto &lib : compiler.libs)
		args.append(" ", lib);
	if (!compiler.objFile.empty())
		args.append(" \"", compiler.objFile, "\"");
	MutableString<1024> cmd(Concat, "\"\"", link, "\" ", args, "\"");

	int result = system(cmd.c_str());
	if (result)
	{
		error(compiler.outFile.c_str(), 0, "Link failed!");
	}
}

}
