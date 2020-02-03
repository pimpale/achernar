#ifndef ERRORS_H
#define ERRORS_H

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>



typedef enum DiagnosticType_e {
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
} DiagnosticType;

typedef struct DiagnosticLogger_s {
  bool created;
  bool destroyed;
  bool messagePrinted;
  FILE* file;
} DiagnosticLogger;

DiagnosticLogger* createDiagnosticLogger(DiagnosticLogger* dl, FILE* file);
DiagnosticLogger* destroyDiagnosticLogger(DiagnosticLogger* dl);


void logDiagnostic(DiagnosticLogger* dl, DiagnosticType dt, uint64_t ln, uint64_t col);
void logInternalError(uint64_t line, const char* func, const char* fmt, ...);

#define UNUSED(x) (void)(x)
#define PANIC() exit(EXIT_FAILURE)
#define INTERNAL_ERROR(msg) logInternalError(__LINE__, __func__, msg)

#endif
