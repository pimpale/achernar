#ifndef ERRORS_H
#define ERRORS_H

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>

#include "lncol.h"

typedef enum DiagnosticType_e {
  // no error
  E_Ok,
  // unknown error
  E_Unknown,
  // Generic Lexing Errors
  E_EOF,
  E_UnrecognizedCharacter,
  // Integer Literals
  E_IntLiteralUnrecognizedRadixCode,
  E_IntLiteralDigitExceedsRadix,
  E_IntLiteralOverflow,
  // Float Literals
  E_FloatLiteralDigitExceedsRadix,
  E_FloatLiteralExceedsMaxPrecision,
  // Character Literals
  E_CharLiteralEmpty,
  E_CharLiteralTooLong,
  E_CharLiteralUnrecognizedEscapeCode,
  // String Literals
  E_StringLiteralTooLong,
  E_StringLiteralUnrecognizedEscapeCode,
  // Parsing Errors
  // VarDeclStmnt
  E_VarDeclStmntExpectedType,
  E_VarDeclStmntExpectedIdentifier,
  E_VarDeclStmntExpectedAssign,
  E_VarDeclStmntExpectedValue,
  // FuncDeclStmnt
  E_FuncDeclStmntExpectedParen,
  E_FuncDeclStmntParamExpectedType,
  E_FuncDeclStmntParamExpectedIdentifier,
  E_FuncDeclStmntExpectedTypeAnnotation,
  E_FuncDeclStmntExpectedTypeIdentifier,
  E_FuncDeclStmntExpectedBody,
  // Generic Parsing errors
  E_UnexpectedToken,
  E_SubcomponentFailedToParse,
} DiagnosticType;

typedef struct Diagnostic_s {
  DiagnosticType type;
  Span span;
} Diagnostic;

#define DIAGNOSTIC(type, span) ((Diagnostic) {type, span})

void logInternalError(uint32_t line, const char* func, const char* fmt, ...);

#define UNUSED(x) (void)(x)
#define PANIC() exit(EXIT_FAILURE)
#define INTERNAL_ERROR(msg) logInternalError(__LINE__, __func__, msg)

#endif
