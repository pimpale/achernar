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
  // MacroExpr
  DK_MacroExprExpectedClosingBacktick,
  // Number literals
  DK_NumLiteralNoFirstDigit,
  DK_NumLiteralFirstDigitUnderscore,
  DK_NumLiteralUnrecognizedRadixCode,
  DK_NumLiteralDigitExceedsRadix,
  DK_NumLiteralOverflow,
  DK_NumLiteralUnknownCharacter,
  // Float Literals
  DK_FloatLiteralFirstDigitUnderscore,
  DK_FloatLiteralNoFirstDigit,
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
  // StructMemberExpr
  DK_StructMemberExpectedType,
  DK_StructMemberExpectedIdentifier,
  // StructMemberLiteralExpr
  DK_StructMemberLiteralExpectedIdentifier,
  DK_StructMemberLiteralExpectedDefine,
  // FnValExpr
  DK_FnValExprExpectedRightParen,
  DK_FnValExprExpectedLeftParen,
  DK_FnValExprExpectedArrow,
  // TypeExpr
  DK_TypeExprUnexpectedToken,
  DK_TypeExprFieldAccessExpectedIdentifier,
  DK_TypeGroupExpectedRightBrace,
  // PatExprs
  // Pat Group
  DK_PatGroupExpectedLeftBrace,
  DK_PatGroupExpectedRightBrace,
  // Pat
  DK_PatStructExpectedLeftBrace,
  DK_PatStructExpectedRightBrace,
  DK_PatStructExpectedIdentifier,
  DK_PatStructExpectedDefine,
  // ValDecl
  DK_ValDeclExpectedDefine,
  DK_ValDeclExpectedVal,
  // TypeDecl
  DK_TypeDeclExpectedIdentifier,
  DK_TypeDeclExpectedDefine,
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
  // Namespace errors
  DK_NamespaceExpectedIdentifier,
  DK_NamespaceExpectedLeftBrace,
  DK_NamespaceExpectedRightBrace,
  // Generic Parsing errors
  DK_UnexpectedToken,
  DK_FieldAccessExpectedIdentifier,
} DiagnosticKind;

const char *strDiagnosticKind(DiagnosticKind val);

typedef struct Diagnostic_s {
  DiagnosticKind kind;
  Span span;
} Diagnostic;

#define DIAGNOSTIC(type, span) ((Diagnostic){type, span})

#endif
