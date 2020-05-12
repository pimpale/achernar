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
  // Integer literals
  DK_IntLiteralUnrecognizedRadixCode,
  DK_IntLiteralDigitExceedsRadix,
  DK_IntLiteralOverflow,
  DK_IntLiteralUnknownCharacter,
  // Float Literals
  DK_FloatLiteralDigitExceedsRadix,
  DK_FloatLiteralExceedsMaxPrecision,
  // Character Literals
  DK_CharLiteralEmpty,
  DK_CharLiteralTooLong,
  DK_CharLiteralUnrecognizedEscapeCode,
  // ConstExpr Errors
  DK_ConstExprUnrecognizedLiteral,
  // String Literals
  DK_StringLiteralTooLong,
  DK_StringLiteralUnrecognizedEscapeCode,
  // Struct Literals
  DK_StructLiteralExpectedEntry,
  DK_StructLiteralExpectedRightBrace,
  DK_StructLiteralExpectedLeftBrace,
  // Parsing Errors
  // Path
  DK_PathExpectedIdentifier,
  // StructMemberExpr
  DK_StructMemberExpectedType,
  DK_StructMemberExpectedIdentifier,
  // StructMemberLiteralExpr
  DK_StructMemberLiteralExpectedValue,
  DK_StructMemberLiteralExpectedIdentifier,
  // FnValueExpr
  DK_FnValueExprExpectedRightParen,
  DK_FnValueExprExpectedLeftParen,
  DK_FnValueExprExpectedArrow,
  // TypeExpr
  DK_TypeExprUnexpectedToken,
  DK_TypeExprFieldAccessExpectedIdentifier,
  // PatternExprs
  DK_PatternGroupExpectedRightBrace,
  DK_PatternStructExpectedRightBrace,
  DK_PatternStructExpectedIdentifier,
  DK_PatternStructUnexpectedAssignForValueRestriction,
  DK_PatternStructExpectedAssignForNonValueRestriction,
  // VarDeclStmnt
  DK_VarDeclStmntExpectedAssign,
  DK_VarDeclStmntExpectedValue,
  // TypeDeclStmnt
  DK_TypeDeclStmntExpectedIdentifier,
  DK_TypeDeclStmntExpectedAssign,
  // StructTypeExpr
  DK_StructExpectedLeftBrace,
  DK_StructExpectedRightBrace,
  // FnTypeExpr
  DK_FnTypeExprExpectedLeftParen,
  DK_FnTypeExprExpectedRightParen,
  DK_FnTypeExprExpectedColon,
  // MatchCase
  DK_MatchCaseNoColon,
  // Match
  DK_MatchNoLeftBrace,
  DK_MatchNoRightBrace,
  DK_MatchNoComma,
  // Block
  DK_BlockExpectedRightBrace,
  // Fn Calls
  DK_CallExpectedParen,
  // Generic Parsing errors
  DK_UnexpectedToken,
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
