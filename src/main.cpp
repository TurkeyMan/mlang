#include "mlang.h"

#include "semantic.h"

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <stdio.h>

m::StatementList parse(FILE *file, String filename);

namespace m {

void LoadConfig();
void InitCodegen();
void PopulateIntrinsics(Module *module);
Module* LoadModule(String filename, String path, bool errorOnFail);
void Codegen(Compiler &compiler);
void Link(Compiler &compiler);

SharedString curSrcFile;
SharedString astFile;

Compiler mlang;

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
			else if (!strncmp(argv[arg], "-I", 2))
			{
				if (argv[arg][2])
					mlang.impPaths.push_back(argv[arg] + 2);
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

		LoadConfig();
		InitCodegen();

		// create root module
		mlang.root = new Module(nullptr, nullptr, {}, StatementList::empty(), nullptr);

		// load source modules
		for (auto &src : mlang.srcFiles)
		{
			m::Module *module = LoadModule(src, nullptr, true);
			mlang.modules.push_back(module);
		}

		// populate the root namespace with intrinsics
		PopulateIntrinsics(mlang.root);

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

void LoadConfig()
{
	MutableString<> file;

	FILE *f;
	if (fopen_s(&f, "mlang.conf", "r") == 0)
	{
		fseek(f, 0, SEEK_END);
		long size = ftell(f);
		fseek(f, 0, SEEK_SET);

		file.resize(size);
		fread(file.get_buffer().ptr, 1, size, f);
		fclose(f);
	}

	file.tokenise([](String line, size_t) {
		line = line.get_left_at_first("#").trim();
		if (line.empty())
			return;

		// parse line
		size_t valOffset = min(line.find_first(' '), line.find_first('='));
		String token = line.slice(0, valOffset).trim();

		String arg;
		if (valOffset < line.length)
		{
			arg = line.slice(valOffset + 1, line.length).trim();
			if (arg[0] == '=')
				arg = arg.slice(1, arg.length).trim();
		}

		if (token.eq_ic("IMPPATH"))
			mlang.impPaths.push_back(arg);
		else if (token.eq_ic("LIBPATH"))
			mlang.libPaths.push_back(arg);
		else if (token.eq_ic("LINKCMD"))
			mlang.linkCmd = arg;
		else if (token.eq_ic("LINKARGS"))
			mlang.linkArgs = arg;
	}, "\n");
}

void PopulateIntrinsics(Module *module)
{
	TypeDecl *xtern = new TypeDecl("extern", new Struct(StatementList::empty(), SourceLocation(0)), NodeList::empty(), SourceLocation(0));
	module->addDecl(xtern->name(), xtern);

	TypeDecl *xternc = new TypeDecl("extern_c", new Struct(StatementList::empty(), SourceLocation(0)), NodeList::empty(), SourceLocation(0));
	module->addDecl(xternc->name(), xternc);

	TypeDecl *deprecate = new TypeDecl("deprecate", new Struct(StatementList::empty(), SourceLocation(0)), NodeList::empty(), SourceLocation(0));
	module->addDecl(deprecate->name(), deprecate);
}

Module* LoadModule(String filename, String searchPath, bool errorOnFail)
{
	MutableString256 src(Concat, searchPath, searchPath && searchPath.back() != '/' && searchPath.back() != '\\' ? "/" : "", filename);

	src.replace('\\', '/');
	String filePart = src.get_right_at_last('/', false);
	if (!filePart)
		filePart = src;

	// open source file
	FILE *file;
	fopen_s(&file, src.c_str(), "r");
	if (!file)
	{
		if (errorOnFail)
			infoError("'%s': can't open source file", src.c_str());
		return nullptr;
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
			if (errorOnFail)
				infoError("'%s': can't open ast file", astFile.c_str());
			return nullptr;
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

	ModuleDecl *moduleDecl = nullptr;
	for (auto statement : statements)
	{
		ModuleDecl *decl = dynamic_cast<ModuleDecl*>(statement);
		if (decl)
		{
			if (moduleDecl)
				error(src.c_str(), decl->getLine(), "invalid: multiple 'module' statements");

			decl->name().tokenise([&](String token, size_t) { moduleIdentifier.push_back(token); }, ".");
			moduleDecl = decl;
		}
	}

	if (!moduleDecl)
	{
		// HACK?: make default module name from filename without extension
		MutableString256 moduleName = filename;
		ptrdiff_t slash = moduleName.find_last('/');
		if (slash == moduleName.length)
			slash = -1;
		ptrdiff_t dot = moduleName.find_last('.');
		if (dot == moduleName.length)
			slash = -1;
		if (dot > slash)
			moduleName.pop_back(moduleName.length - dot);
		moduleName.replace('.', '_');
		moduleName.replace('/', '.');

		moduleName.tokenise([&](String token, size_t) { moduleIdentifier.push_back(token); }, ".");

		moduleDecl = new ModuleDecl(moduleName, NodeList::empty(), SourceLocation(0));
	}

	m::Module *module = new m::Module(src, filePart, moduleIdentifier, statements, moduleDecl);
	moduleDecl->setModule(module);

	// insert it into the tree...
	Module *sub = mlang.root;
	for (size_t i = 0; i < moduleIdentifier.length; ++i)
	{
		const SharedString &name = moduleIdentifier[i];
		auto it = sub->submodules().find(name);

		if (i < moduleIdentifier.length - 1)
		{
			if (it == sub->submodules().end())
			{
				ModuleDecl *moduleDecl = new ModuleDecl(nullptr, NodeList::empty(), SourceLocation(0));
				Module *package = new Module(nullptr, nullptr, Array<SharedString>(moduleIdentifier.slice(0, i + 1)), StatementList::empty(), moduleDecl);
				moduleDecl->setModule(package);

				package->setParent(sub);
				sub->submodules().insert({ name, package });
				sub->addDecl(name, moduleDecl);
				sub = sub->submodules()[name];
			}
		}
		else
		{
			if (it != sub->submodules().end())
			{
				const SharedString &path = sub->path();
				ModuleDecl *m = sub->getModuleDecl();
				error(!path.empty() ? path.c_str() : "", m ? m->getLine() : 0, "module '%s' already exists", (const char*)sub->stringof().c_str());
			}
			module->setParent(sub);
			sub->submodules().insert({ name, module });
			sub->addDecl(name, module->getModuleDecl());
		}
	}

	return module;
}

}
