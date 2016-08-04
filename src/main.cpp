#include "semantic.h"

#include <stdio.h>

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>


std::string Codegen(Module *pAST);


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

		// open source file
		const char *pFilename = argv[1];
		FILE *file;
		fopen_s(&file, pFilename, "r");
		if (!file)
		{
			printf("Can't open file: %s\n", pFilename);
			return -1;
		}

		// parse the source
		StatementList module = parse(file);

		// done with the file
		fclose(file);

		// semantic
		Semantic semantic;
		semantic.run(pFilename, module);

		// dump parse tree
		FILE *ast = nullptr;
		if (argc >= 4)
		{
			fopen_s(&ast, argv[3], "w");
			if (!file)
			{
				printf("Can't open ast file: %s\n", argv[3]);
				return -1;
			}
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

		// codegen
		std::string code = Codegen(semantic.getModule());

		pFilename = argv[2];
		fopen_s(&file, pFilename, "w");
		if (!file)
		{
			printf("Can't open file for output: %s\n", pFilename);
			return -1;
		}
		fwrite(code.c_str(), 1, code.size(), file);
		fclose(file);

		return 0;
	}
}
