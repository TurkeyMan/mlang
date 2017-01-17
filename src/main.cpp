#include "mlang.h"

#include "semantic.h"

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <stdio.h>

m::StatementList parse(FILE *file, std::string filename);

namespace m {

void InitCodegen();
void Codegen(Compiler &compiler);
void Link(Compiler &compiler);

std::string curSrcFile;
std::string astFile;

Compiler mlang;

inline bool ends_with(std::string const & value, std::string const & ending)
{
	if (ending.size() > value.size()) return false;
	return std::equal(ending.rbegin(), ending.rend(), value.rbegin());
}

extern "C" {
	int main(int argc, const char *argv[])
	{
		GC_INIT();

		InitCodegen();

		// parse command line
		int arg = 1;
		while (arg < argc)
		{
			if (!strcmp(argv[arg], "-o"))
			{
				mlang.outFile = argv[++arg];
			}
			else if (!strcmp(argv[arg], "-emit-ir"))
			{
				mlang.irFile = argv[++arg];
			}
			else if (!strcmp(argv[arg], "-emit-ast"))
			{
				astFile = argv[++arg];
			}
			else if (!strcmp(argv[arg], "-C"))
			{
				mlang.mode = Mode::Compile;
			}
			else if (!strcmp(argv[arg], "-S"))
			{
				mlang.mode = Mode::OutputAsm;
			}
			else if (!strcmp(argv[arg], "-B"))
			{
				mlang.mode = Mode::OutputBC;
			}
			else if (!strcmp(argv[arg], "-g"))
			{
				mlang.debug = true;
			}
			else if (!strncmp(argv[arg], "-O", 2))
			{
				if (argv[arg][2])
					mlang.opt = atoi(argv[arg] + 2);
				else
					mlang.opt = 2;
			}
			else if (!strncmp(argv[arg], "-L", 2))
			{
				if (argv[arg][2])
					mlang.libPaths.push_back(argv[arg] + 2);
			}
			else if (!strncmp(argv[arg], "-l", 2))
			{
				if (argv[arg][2])
					mlang.libs.push_back(argv[arg] + 2);
			}
			else if (!strncmp(argv[arg], "-runtime=", 9))
			{
				if (argv[arg][9])
					mlang.runtime = argv[arg] + 9;
			}
			else
				mlang.srcFiles.push_back(argv[arg]);

			++arg;
		}

		if (mlang.outFile.empty())
		{
			if (mlang.mode == Mode::Compile)
#if defined(_MSC_VER)
				mlang.outFile = "out.obj";
#else
				mlang.outFile = "out.o";
#endif
			else if (mlang.mode == Mode::CompileAndLink)
#if defined(_MSC_VER)
				mlang.outFile = "out.exe";
#else
				mlang.outFile = "a.out";
#endif
			else if (mlang.mode == Mode::OutputAsm)
#if defined(_MSC_VER)
				mlang.outFile = "out.asm";
#else
				mlang.outFile = "out.s";
#endif
			else if (mlang.mode == Mode::OutputBC)
				mlang.outFile = "out.bc";
		}

		if (mlang.mode == Mode::CompileAndLink)
		{
			if (ends_with(mlang.outFile, ".exe") || ends_with(mlang.outFile, ".lib") || ends_with(mlang.outFile, ".dll"))
				mlang.objFile = mlang.outFile.substr(0, mlang.outFile.size() - 4) + ".obj";
			else
				mlang.objFile = mlang.outFile + ".obj";
		}
		else
			mlang.objFile = mlang.outFile;

		for (auto &src : mlang.srcFiles)
		{
			char path[260];
			char *pFilePart;
			DWORD len = GetFullPathNameA(src.c_str(), sizeof(path), path, &pFilePart);
			if (len == 0)
			{
				outputMessage("Source file %s does not exist\n", src.c_str());
				return -1;
			}
			if (!pFilePart)
			{
				outputMessage("Source filename is a directory: %s\n", src.c_str());
				return -1;
			}

			// open source file
			FILE *file;
			fopen_s(&file, src.c_str(), "r");
			if (!file)
			{
				outputMessage("Can't open source file: %s\n", src.c_str());
				return -1;
			}

			// parse the source
			m::StatementList statements = parse(file, src);

			// done with the file
			fclose(file);

			// HACK?: make default module name from filename without extension
			std::string moduleName = src;
			size_t slash = moduleName.find_last_of('/');
			slash = max(moduleName.find_last_of('\\'), slash);
			size_t dot = moduleName.find_last_of('.');
			if (dot > slash)
				moduleName = moduleName.substr(0, dot);
			std::replace(moduleName.begin(), moduleName.end(), '/', '.');
			std::replace(moduleName.begin(), moduleName.end(), '\\', '.');

			m::Module *module = new m::Module(path, pFilePart, std::move(moduleName), statements);

			mlang.modules.insert({ moduleName, module });

			// dump parse tree
			FILE *ast = nullptr;
			if (!astFile.empty())
			{
				fopen_s(&ast, (src + ".ast").c_str(), "w");
				if (!file)
				{
					outputMessage("Can't open ast file: %s", astFile.c_str());
					return -1;
				}
				class OS : public llvm::raw_ostream
				{
					FILE *ast;
					uint64_t offset = 0;
				public:
					OS(FILE *ast) : ast(ast) {}
					void write_impl(const char *Ptr, size_t Size) override
					{
						if (ast)
							fwrite(Ptr, 1, Size, ast);
						offset += Size;
					}
					uint64_t current_pos() const override { return offset; }
				};
				OS os(ast);
				for (auto s : statements)
					s->dump(os, 0);
				os.flush();
				fclose(ast);
			}
		}

		// semantic
		m::Semantic semantic(mlang);
		semantic.run();

		// codegen
		if(mlang.mode != Mode::Parse)
			Codegen(mlang);

		// link
		if (mlang.mode == Mode::CompileAndLink)
			Link(mlang);

		return 0;
	}
}

}
