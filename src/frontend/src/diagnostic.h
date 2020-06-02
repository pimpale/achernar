#ifndef DIAGNOSTIC_H
#define DIAGNOSTIC_H

#include "lncol.h"

typedef enum {
  // no error
  DK_Ok,
  // unknown error
  DK_Unknown,
  // Generic Lexing Errors
  DK_EOF,
  DK_UnrecognizedCharacter,
  // Builtins
  DK_BuiltinExpectedLeftParen,
  DK_BuiltinExpectedRightParen,
  // Integer literals
  DK_IntLiteralUnrecognizedRadixCode,
  DK_IntLiteralDigitExceedsRadix,
  DK_IntLiteralOverflow,
  DK_IntLiteralUnknownCharacter,
  // Float Literals
  DK_FloatLiteralDigitExceedsRadix,
  DK_FloatLiteralExceedsMaxPrecision,
  // Character Literals
  DK_CharLiteralUnrecognizedEscapeCode,
  DK_CharLiteralExpectedCloseSingleQuote,
  // Labels
  DK_UnexpectedLabel,
  DK_LabelUnknownCharacter,
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
  // Return
  DK_ReturnExpectedLabel,
  // Continue
  DK_ContinueExpectedLabel,
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
  // Pattern Group
  DK_PatternGroupExpectedLeftBrace,
  DK_PatternGroupExpectedRightBrace,
  // Pattern 
  DK_PatternStructExpectedLeftBrace,
  DK_PatternStructExpectedRightBrace,
  DK_PatternStructExpectedIdentifier,
  DK_PatternStructUnexpectedAssignForValueRestriction,
  DK_PatternStructExpectedAssignForNonValueRestriction,
  // ValDecl
  DK_ValDeclExpectedAssign,
  DK_ValDeclExpectedValue,
  // TypeDecl
  DK_TypeDeclExpectedIdentifier,
  DK_TypeDeclExpectedAssign,
  // StructTypeExpr
  DK_StructExpectedLeftBrace,
  DK_StructExpectedRightBrace,
  // FnTypeExpr
  DK_FnTypeExprExpectedLeftParen,
  DK_FnTypeExprExpectedRightParen,
  DK_FnTypeExprExpectedColon,
  // MatchCase
  DK_MatchCaseNoArrow,
  DK_MatchCaseNoPat,
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

#define DIAGNOSTIC(type, span) ((Diagnostic){type, span})

char *strDiagnosticKind(DiagnosticKind dk);


#endif
