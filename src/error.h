#include <stdarg.h>

void outputMessage(const char *message, ...);
void vaOutputMessage(const char *message, va_list args);

void emitWarning(const char *file, int line, const char *message, ...);
void emitError(const char *file, int line, const char *message, ...);
void emitICE(const char *file, int line, const char *message, ...);

#define error(file, line, message, ...) do { emitError((file), (line), (message), __VA_ARGS__); __debugbreak(); exit(-1); } while(false)
#define ice(message, ...) do { emitICE(__FILE__, __LINE__, (message), __VA_ARGS__); __debugbreak(); } while(false)
