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
  // Generic Parsing errors
  DK_UnexpectedToken,
  DK_FieldAccessExpectedIdentifier,
} DiagnosticKind;

static const char *strDiagnosticKind(DiagnosticKind val) {
  switch (val) {
  case DK_Ok:
    return "Ok";
  case DK_Unknown:
    return "Unknown";
  case DK_EOF:
    return "EOF";
  case DK_UnrecognizedCharacter:
    return "UnrecognizedCharacter";
  case DK_MacroExprExpectedClosingBacktick:
    return "MacroExprExpectedClosingBacktick";
  case DK_NumLiteralNoFirstDigit:
    return "NumLiteralNoFirstDigit";
  case DK_NumLiteralFirstDigitUnderscore:
    return "NumLiteralFirstDigitUnderscore";
  case DK_NumLiteralUnrecognizedRadixCode:
    return "NumLiteralUnrecognizedRadixCode";
  case DK_NumLiteralDigitExceedsRadix:
    return "NumLiteralDigitExceedsRadix";
  case DK_NumLiteralOverflow:
    return "NumLiteralOverflow";
  case DK_NumLiteralUnknownCharacter:
    return "NumLiteralUnknownCharacter";
  case DK_FloatLiteralFirstDigitUnderscore:
    return "FloatLiteralFirstDigitUnderscore";
  case DK_FloatLiteralNoFirstDigit:
    return "FloatLiteralNoFirstDigit";
  case DK_FloatLiteralDigitExceedsRadix:
    return "FloatLiteralDigitExceedsRadix";
  case DK_FloatLiteralExceedsMaxPrecision:
    return "FloatLiteralExceedsMaxPrecision";
  case DK_CharLiteralUnrecognizedEscapeCode:
    return "CharLiteralUnrecognizedEscapeCode";
  case DK_CharLiteralExpectedCloseSingleQuote:
    return "CharLiteralExpectedCloseSingleQuote";
  case DK_UnexpectedLabel:
    return "UnexpectedLabel";
  case DK_LabelUnknownCharacter:
    return "LabelUnknownCharacter";
  case DK_ConstExprUnrecognizedLiteral:
    return "ConstExprUnrecognizedLiteral";
  case DK_StringLiteralTooLong:
    return "StringLiteralTooLong";
  case DK_StringLiteralUnrecognizedEscapeCode:
    return "StringLiteralUnrecognizedEscapeCode";
  case DK_StructLiteralExpectedEntry:
    return "StructLiteralExpectedEntry";
  case DK_StructLiteralExpectedRightBrace:
    return "StructLiteralExpectedRightBrace";
  case DK_StructLiteralExpectedLeftBrace:
    return "StructLiteralExpectedLeftBrace";
  case DK_PathExpectedIdentifier:
    return "PathExpectedIdentifier";
  case DK_StructMemberExpectedType:
    return "StructMemberExpectedType";
  case DK_StructMemberExpectedIdentifier:
    return "StructMemberExpectedIdentifier";
  case DK_StructMemberLiteralExpectedIdentifier:
    return "StructMemberLiteralExpectedIdentifier";
  case DK_StructMemberLiteralExpectedDefine:
    return "StructMemberLiteralExpectedDefine";
  case DK_FnValExprExpectedRightParen:
    return "FnValExprExpectedRightParen";
  case DK_FnValExprExpectedLeftParen:
    return "FnValExprExpectedLeftParen";
  case DK_FnValExprExpectedArrow:
    return "FnValExprExpectedArrow";
  case DK_TypeExprUnexpectedToken:
    return "TypeExprUnexpectedToken";
  case DK_TypeExprFieldAccessExpectedIdentifier:
    return "TypeExprFieldAccessExpectedIdentifier";
  case DK_TypeGroupExpectedRightBrace:
    return "TypeGroupExpectedRightBrace";
  case DK_PatGroupExpectedLeftBrace:
    return "PatGroupExpectedLeftBrace";
  case DK_PatGroupExpectedRightBrace:
    return "PatGroupExpectedRightBrace";
  case DK_PatStructExpectedLeftBrace:
    return "PatStructExpectedLeftBrace";
  case DK_PatStructExpectedRightBrace:
    return "PatStructExpectedRightBrace";
  case DK_PatStructExpectedIdentifier:
    return "PatStructExpectedIdentifier";
  case DK_PatStructExpectedDefine:
    return "PatStructExpectedDefine";
  case DK_ValDeclExpectedDefine:
    return "ValDeclExpectedDefine";
  case DK_ValDeclExpectedVal:
    return "ValDeclExpectedVal";
  case DK_TypeDeclExpectedIdentifier:
    return "TypeDeclExpectedIdentifier";
  case DK_TypeDeclExpectedDefine:
    return "TypeDeclExpectedDefine";
  case DK_StructExpectedLeftBrace:
    return "StructExpectedLeftBrace";
  case DK_StructExpectedRightBrace:
    return "StructExpectedRightBrace";
  case DK_FnTypeExprExpectedLeftParen:
    return "FnTypeExprExpectedLeftParen";
  case DK_FnTypeExprExpectedRightParen:
    return "FnTypeExprExpectedRightParen";
  case DK_FnTypeExprExpectedColon:
    return "FnTypeExprExpectedColon";
  case DK_MatchCaseNoArrow:
    return "MatchCaseNoArrow";
  case DK_MatchCaseNoPat:
    return "MatchCaseNoPat";
  case DK_MatchNoLeftBrace:
    return "MatchNoLeftBrace";
  case DK_MatchNoRightBrace:
    return "MatchNoRightBrace";
  case DK_MatchNoComma:
    return "MatchNoComma";
  case DK_BlockExpectedRightBrace:
    return "BlockExpectedRightBrace";
  case DK_CallExpectedParen:
    return "CallExpectedParen";
  case DK_UnexpectedToken:
    return "UnexpectedToken";
  case DK_FieldAccessExpectedIdentifier:
    return "FieldAccessExpectedIdentifier";
  }
}

typedef struct Diagnostic_s {
  DiagnosticKind kind;
  Span span;
} Diagnostic;

#define DIAGNOSTIC(type, span) ((Diagnostic){type, span})

#endif
