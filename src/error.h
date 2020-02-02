#ifndef ERRORS_H
#define ERRORS_H

#include <stdlib.h>
#include <stdint.h>

typedef enum Severity_e {
  SeverityDebug,
  SeverityInfo,
  SeverityWarn,
  SeverityError,
  SeverityFatal,
  SeverityUnknown,
} Severity;

// Errors caused by bad user code
typedef enum Error {
  // no error
  ErrorOk,
  // Initialization
  ErrorBadOption,
  // Generic Lexing Errors
  ErrorEOF,
  ErrorUnrecognizedCharacter,
  // Integer Literals
  ErrorIntLiteralUnrecognizedRadixCode,
  ErrorIntLiteralDigitExceedsRadix,
  ErrorIntLiteralOverflow,
  // Float Literals
  ErrorFloatLiteralDigitExceedsRadix,
  ErrorFloatLiteralExceedsMaxPrecision,
  // Character Literals
  ErrorCharLiteralEmpty,
  ErrorCharLiteralTooLong,
  ErrorCharLiteralUnrecognizedEscapeCode,
  // String Literals
  ErrorStringLiteralTooLong,
  ErrorStringLiteralUnrecognizedEscapeCode,
} Error;

char* toStrSeverity(Severity level);
char* toStrError(Error level);

void logError(Severity severity, Error type, uint64_t ln, uint64_t col);
void logInternalError(uint64_t line, const char* func, const char* fmt, ...);

#define UNUSED(x) (void)(x)
#define PANIC() exit(EXIT_FAILURE)

#endif
