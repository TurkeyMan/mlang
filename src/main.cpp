#include "mlang.h"

#include "semantic.h"

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <stdio.h>

m::StatementList parse(FILE *file, String filename);

namespace m {

void InitCodegen();
void Codegen(Compiler &compiler);
void Link(Compiler &compiler);

SharedString curSrcFile;
SharedString astFile;

Compiler mlang;

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
			{
				MutableString256 file = argv[arg];
				file.replace('\\', '/');
				mlang.srcFiles.push_back(file);
			}

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
			if (mlang.outFile.ends_with(".exe") || mlang.outFile.ends_with(".lib") || mlang.outFile.ends_with(".dll"))
				mlang.objFile = SharedString(Concat, mlang.outFile.slice(0, mlang.outFile.length - 4), ".obj");
			else
				mlang.objFile = SharedString(Concat, mlang.outFile, ".obj");
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

			// dump parse tree
			FILE *ast = nullptr;
			if (!astFile.empty())
			{
				fopen_s(&ast, MutableString256(Concat, src, ".ast").c_str(), "w");
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

			// add to module list
			Array<SharedString> moduleIdentifier;

			ModuleStatement *moduleStatement = nullptr;
			for (auto pStatement : statements)
			{
				ModuleStatement *module = dynamic_cast<ModuleStatement*>(pStatement);
				if (module)
				{
					if (moduleStatement)
						error(src.c_str(), module->getLine(), "Invalid; multiple 'module' statements.");

					module->name().tokenise([&](String token, size_t) { moduleIdentifier.push_back(token); }, ".");
					moduleStatement = module;
				}
			}

			if (moduleIdentifier.empty())
			{
				// HACK?: make default module name from filename without extension
				MutableString256 moduleName = src;
				ptrdiff_t slash = moduleName.find_last('/');
				if (slash == moduleName.length)
					slash = -1;
				ptrdiff_t dot = moduleName.find_last('.');
				if (dot == moduleName.length)
					slash = -1;
				if (dot > slash)
					moduleName.pop_back(moduleName.length - dot);
				moduleName.replace('.', '_');

				moduleName.tokenise([&](String token, size_t) { moduleIdentifier.push_back(token); }, "/");
			}

			m::Module *module = new m::Module(path, pFilePart, std::move(moduleIdentifier), statements, moduleStatement);

			mlang.modules.push_back(module);
		}

		// organise modules into tree...
		mlang.root = new Module(nullptr, nullptr, {}, StatementList::empty(), nullptr);
		for (auto m : mlang.modules)
		{
			Module *sub = mlang.root;
			for (size_t i = 0; i < m->fullName().size(); ++i)
			{
				const SharedString &name = m->fullName()[i];
				auto it = sub->submodules().find(name);

				if (i < m->fullName().size() - 1)
				{
					if (it == sub->submodules().end())
					{
						Module *package = new Module(nullptr, nullptr, Array<SharedString>(m->fullName().slice(0, i + 1)), StatementList::empty(), nullptr);
						sub->submodules().insert({ name, package });
						sub = sub->submodules()[name];
					}
				}
				else
				{
					if (it != sub->submodules().end())
					{
						const SharedString &path = sub->path();
						ModuleStatement *m = sub->getModuleStatement();
						error(!path.empty() ? (const char*)path.c_str() : "", m ? m->getLine() : 0, "Module '%s' already exists.", (const char*)sub->stringof().c_str());
					}
					sub->submodules().insert({ name, m });
				}
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
