#include "mlang.h"

#include <stdio.h>
#include <cstdio>
#include <iostream>
#include <memory>
#include <stdexcept>

namespace m {

extern Compiler mlang;

void Link(Compiler &compiler)
{
	MutableString<1024> args(Concat, "/OUT:\"", compiler.outFile, "\" /MACHINE:X64");
	if (mlang.linkArgs)
		args.append(' ', mlang.linkArgs);

	if (compiler.debug)
	{
		args.append(" /DEBUG");
		if (compiler.outFile.ends_with_ic(".exe"))
			args.append(" /PDB:\"", compiler.outFile.slice(0, compiler.outFile.length - 4), ".pdb\"");
		else
			args.append(" /PDB:\"", compiler.outFile, ".pdb\"");
	}

	for (auto &libPath : compiler.libPaths)
		args.append(" /LIBPATH:\"", libPath, '"');
	for (auto &lib : compiler.libs)
		args.append(" ", lib);
	if (!compiler.objFile.empty())
		args.append(" \"", compiler.objFile, "\"");
	MutableString<1024> cmd(Concat, "\"\"", mlang.linkCmd, "\" ", args, "\"");

	int result = system(cmd.c_str());
	if (result)
	{
		infoError("%s: link failed", compiler.outFile.c_str());
	}
}

}
