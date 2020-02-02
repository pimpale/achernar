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

/*
int literal: digit value exceeds radix value
int literal: unexpected character
int literal: value exceeds 64 bits
int literal: invalid radix code
float literal: digit value exceeds radix value
float literal: unexpected character
float literal: value exceeds max precision for double
float literal: excess decimal point
char literal: unrecognized escape code
char literal: value too long
char literal: empty char literal
string literal: unrecognized escape code
string literal: value too long
*/

char* toStrSeverity(Severity level);
char* toStrError(Error level);

void logError(Severity severity, Error type, uint64_t ln, uint64_t col);
void logInternalError(uint64_t line, const char* func, const char* fmt, ...);

#define UNUSED(x) (void)(x)
#define PANIC() exit(EXIT_FAILURE)

#endif
