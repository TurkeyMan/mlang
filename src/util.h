#include <string>

enum class Mode
{
	Compile, CompileAndLink, Parse, OutputAsm, OutputBC
};

struct Loc
{
	int startLine, startCol;
	int endLine, endCol;
};

extern std::string curSrcFile;

class Module;
extern ::Module *module;

inline void emitCompileError(const std::string& message, Loc loc)
{
	printf("%s(%d): Error: %s", curSrcFile.c_str(), 0, message.c_str());
}
