
#define SLICE_ALLOC(bytes) malloc(bytes)
#define SLICE_FREE(ptr) free(ptr)

#include "sharedarray.h"
using namespace beautifulcode;

namespace m {

enum class Mode
{
	Compile, CompileAndLink, Parse, OutputAsm, OutputBC
};

struct Loc
{
	int startLine, startCol;
	int endLine, endCol;
};

extern SharedString curSrcFile;

inline void emitCompileError(String message, Loc loc)
{
	printf("%s(%d): Error: %s", (const char*)curSrcFile.c_str(), 0, (const char*)message.c_str());
}

}
