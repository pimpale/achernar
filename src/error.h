#ifndef ERRORS_H
#define ERRORS_H

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>

typedef enum DiagnosticType_e {
  // no error
  ErrorOk,
  // unknown error
  ErrorUnknown,
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
  // Parsing Errors
  // VarDeclStmnt
  ErrorVarDeclStmntExpectedTypeNameOrModifer,
  ErrorVarDeclStmntExpectedVarName,
  ErrorVarDeclStmntExpectedVarName,
  // Generic Parsing errors
  ErrorUnexpectedToken,
  ErrorSubcomponentFailedToParse,
} DiagnosticType;


typedef struct Diagnostic_s {
  DiagnosticType type;
  uint64_t ln;
  uint64_t col;
} Diagnostic;

void logInternalError(uint32_t line, const char* func, const char* fmt, ...);

#define UNUSED(x) (void)(x)
#define PANIC() exit(EXIT_FAILURE)
#define INTERNAL_ERROR(msg) logInternalError(__LINE__, __func__, msg)

#endif
