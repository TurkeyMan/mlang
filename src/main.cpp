#include "semantic.h"

#include <stdio.h>

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>


void Codegen(Module *pAST, Mode mode, std::string outFile, std::string irFile);

std::vector<std::string> srcFiles;
std::string curSrcFile;
std::string outFile;
std::string irFile;
std::string astFile;

std::map<std::string, TypeExpr*> types;

Mode mode = Mode::CompileAndLink;
int opt = 0;

SourceLocation CurLoc;
SourceLocation LexLoc = { 1, 0 };


/// Error* - These are little helper functions for error handling.
std::unique_ptr<Expr> Error(const char *Str)
{
	fprintf(stderr, "Error: %s\n", Str);
	return nullptr;
}

std::unique_ptr<PrototypeDecl> ErrorP(const char *Str)
{
	Error(Str);
	return nullptr;
}


StatementList parse(FILE *file);

extern "C" {
	int main(int argc, const char *argv[])
	{
		GC_INIT();

		// parse command line
		int arg = 1;
		while (arg < argc)
		{
			if (!strcmp(argv[arg], "-o"))
			{
				outFile = argv[++arg];
			}
			else if (!strcmp(argv[arg], "-emit-ir"))
			{
				irFile = argv[++arg];
			}
			else if (!strcmp(argv[arg], "-emit-ast"))
			{
				astFile = argv[++arg];
			}
			else if (!strcmp(argv[arg], "-C"))
			{
				mode = Mode::Compile;
			}
			else if (!strcmp(argv[arg], "-S"))
			{
				mode = Mode::OutputAsm;
			}
			else if (!strcmp(argv[arg], "-B"))
			{
				mode = Mode::OutputBC;
			}
			else if (!strcmp(argv[arg], "-O"))
			{
				if (argv[arg][2])
					opt = atoi(argv[arg] + 2);
				else
					opt = 2;
			}
			else
				srcFiles.push_back(argv[arg]);

			++arg;
		}

		if (outFile.empty())
		{
			if (mode == Mode::Compile)
#if defined(_MSC_VER)
				outFile = "out.obj";
#else
				outFile = "out.o";
#endif
			else if (mode == Mode::CompileAndLink)
#if defined(_MSC_VER)
				outFile = "out.exe";
#else
				outFile = "a.out";
#endif
			else if (mode == Mode::OutputAsm)
#if defined(_MSC_VER)
				outFile = "out.asm";
#else
				outFile = "out.s";
#endif
			else if (mode == Mode::OutputBC)
				outFile = "out.bc";
		}

		bool bFirst = true;
		for (auto &file : srcFiles)
		{
			if(bFirst)
				printf("%s", file.c_str());
			else
				printf("%s", (file + " ").c_str());
		}
		printf("\n");

		curSrcFile = srcFiles[0];

		// open source file
		FILE *file;
		fopen_s(&file, srcFiles[0].c_str(), "r");
		if (!file)
		{
			printf("Can't open file: %s\n", srcFiles[0].c_str());
			return -1;
		}

		// parse the source
		StatementList module = parse(file);

		// done with the file
		fclose(file);

		// semantic
		Semantic semantic;
		semantic.run(srcFiles[0], module);

		// dump parse tree
		FILE *ast = nullptr;
		if (!astFile.empty())
		{
			fopen_s(&ast, astFile.c_str(), "w");
			if (!file)
			{
				printf("Can't open ast file: %s\n", argv[3]);
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
			for (auto s : module)
				s->dump(os, 0);
			os.flush();
			fclose(ast);
		}

		// codegen
		if(mode != Mode::Parse)
			Codegen(semantic.getModule(), mode, outFile, irFile);

		return 0;
	}
}
