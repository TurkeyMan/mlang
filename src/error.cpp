#include "error.h"
#include <stdio.h>
#include <stdarg.h>

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>

void vaOutputMessage(const char *message, va_list args)
{
	char buffer[2048];
	int len = vsprintf_s(buffer, message, args);
	buffer[len] = '\n'; buffer[len + 1] = 0;

	fputs(buffer, stderr);
	OutputDebugStringA(buffer);
}

void outputMessage(const char *message, ...)
{
	va_list args;
	va_start(args, message);
	vaOutputMessage(message, args);
	va_end(args);
}

void emitWarning(const char *file, int line, const char *message, ...)
{
	char buffer[2048];
	va_list args;
	va_start(args, message);
	vsprintf_s(buffer, message, args);
	outputMessage("%s(%d): warning: %s", file, line, buffer);
	va_end(args);
}

void emitError(const char *file, int line, const char *message, ...)
{
	char buffer[2048];
	va_list args;
	va_start(args, message);
	vsprintf_s(buffer, message, args);
	outputMessage("%s(%d): error: %s", file, line, message);
	va_end(args);
}

void emitICE(const char *file, int line, const char *message, ...)
{
	char buffer[2048];
	va_list args;
	va_start(args, message);
	vsprintf_s(buffer, message, args);
	outputMessage("%s(%d): internal compiler error: %s", file, line, message);
	va_end(args);
}
