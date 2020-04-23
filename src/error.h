#ifndef ERRORS_H
#define ERRORS_H

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>

#include "lncol.h"

typedef enum {
  // no error
  DK_Ok,
  // unknown error
  DK_Unknown,
  // Generic Lexing Errors
  DK_EOF,
  DK_UnrecognizedCharacter,
  // Integer Literals
  DK_IntLiteralUnrecognizedRadixCode,
  DK_IntLiteralDigitExceedsRadix,
  DK_IntLiteralOverflow,
  // Float Literals
  DK_FloatLiteralDigitExceedsRadix,
  DK_FloatLiteralExceedsMaxPrecision,
  // Character Literals
  DK_CharLiteralEmpty,
  DK_CharLiteralTooLong,
  DK_CharLiteralUnrecognizedEscapeCode,
  // String Literals
  DK_StringLiteralTooLong,
  DK_StringLiteralUnrecognizedEscapeCode,
  // Parsing Errors
  // Path
  DK_PathExpectedIdentifier,
  // Binding
  DK_BindingExpectedType,
  DK_BindingExpectedIdentifier,
  // TypeExpr
  DK_TypeExprUnexpectedToken,
  DK_TypeExprFieldAccessExpectedIdentifier,
  // VarDeclStmnt
  DK_VarDeclStmntExpectedAssign,
  DK_VarDeclStmntExpectedValue,
  // AliasDeclStmnt
  DK_AliasDeclStmntExpectedIdentifier,
  // StructDeclStmnt
  DK_StructExpectedLeftBrace,
  DK_StructExpectedRightBrace,
  DK_StructExpectedComma,
  // Tuple
  DK_TupleExpectedRightParen,
  DK_TupleExpectedComma,
  // FnDeclStmnt
  DK_FnExpectedParen,
  DK_FnExpectedType,
  DK_FnExpectedIdentifier,
  DK_FnExpectedColon,
  DK_FnExpectedAssign,
  DK_FnExpectedBody,
  // Groups
  DK_GroupExpectRightParen,
  // MatchCase
  DK_MatchCaseNoColon,
  // Match
  DK_MatchNoLeftBrace,
  DK_MatchNoRightBrace,
  DK_MatchNoComma,
  // Block
  DK_BlockExpectedSemicolon,
  DK_BlockExpectedRightBrace,
  // Array Access
  DK_ArrayAccessExpectedBracket,
  // Function Calls
  DK_FunctionCallExpectedParen,
  // Generic Parsing errors
  DK_UnexpectedToken,
  DK_SubcomponentFailedToParse,
  DK_FieldAccessExpectedIdentifier,
} DiagnosticKind;

typedef struct Diagnostic_s {
  DiagnosticKind kind;
  Span span;
} Diagnostic;


#define DIAGNOSTIC(type, span) ((Diagnostic) {type, span})

char* strDiagnosticKind(DiagnosticKind dk);
void logInternalError(uint32_t line, const char* func, const char* fmt, ...);

#define UNUSED(x) (void)(x)
#define ZERO(ptr) (memset(ptr, 0, sizeof(*ptr)))
#define PANIC() exit(EXIT_FAILURE)
#define INTERNAL_ERROR(msg) logInternalError(__LINE__, __func__, msg)

#endif
