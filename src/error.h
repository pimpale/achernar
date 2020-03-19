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
  // Binding
  E_BindingExpectedType,
  E_BindingExpectedIdentifier,
  // TypeExpr
  E_TypeExprUnexpectedToken,
  // VarDeclStmnt
  E_VarDeclStmntExpectedAssign,
  E_VarDeclStmntExpectedValue,
  // AliasDeclStmnt
  E_AliasDeclStmntExpectedIdentifier,
  // StructDeclStmnt
  E_StructDeclStmntExpectedLeftBrace,
  E_StructDeclStmntExpectedRightBrace,
  // FnDeclStmnt
  E_FnDeclStmntExpectedParen,
  E_FnDeclStmntExpectedType,
  E_FnDeclStmntExpectedIdentifier,
  E_FnDeclStmntExpectedColon,
  E_FnDeclStmntExpectedAssign,
  E_FnDeclStmntExpectedBody,
  // Groups
  E_GroupExpectRightParen,
  // Match
  E_MatchNoColon,
  E_MatchNoLeftbrace,
  E_MatchNoRightBrace,
  // Block
  E_BlockExpectedSemicolon,
  // Array Access
  E_ArrayAccessExpectedBracket,
  // Function Calls
  E_FunctionCallExpectedParen,
  // Generic Parsing errors
  E_UnexpectedToken,
  E_SubcomponentFailedToParse,
  E_FieldAccessExpectedIdentifier,
} DiagnosticType;

typedef struct Diagnostic_s {
  DiagnosticType type;
  Span span;
} Diagnostic;

#define DIAGNOSTIC(type, span) ((Diagnostic) {type, span})

void logInternalError(uint32_t line, const char* func, const char* fmt, ...);

#define UNUSED(x) (void)(x)
#define ZERO(ptr) (memset(ptr, 0, sizeof(*ptr)))
#define PANIC() exit(EXIT_FAILURE)
#define INTERNAL_ERROR(msg) logInternalError(__LINE__, __func__, msg)

#endif
