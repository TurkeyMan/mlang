#include "semantic.h"

#include <stdio.h>

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>


void Codegen(Module *pAST);


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


Node* parse(FILE *file);

int main(int argc, char *argv[])
{
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
	Node *pParseTree = parse(file);

	// dump parse tree
	class OS : public llvm::raw_ostream
	{
	public:
		uint64_t offset = 0;
		void write_impl(const char *Ptr, size_t Size) override
		{
			OutputDebugStringA(Ptr);
			printf("%s", Ptr);
			offset += Size;
		}
		uint64_t current_pos() const override { return offset; }
	};
	OS os;
	pParseTree->dump(os, 0);
	os << "\n" << '\0';
	os.flush();

	// semantic
	Semantic semantic;
	semantic.run(pFilename, pParseTree);

	// codegen
	Codegen(semantic.getModule());

	return 0;
}
