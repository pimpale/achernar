#include "diagnostic.h"

#include <inttypes.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>

#include "constants.h"

char *strDiagnosticKind(DiagnosticKind dk) {
  char *errmsg;

  switch (dk) {

  case DK_Ok: {
    errmsg = "Ok";
    break;
  }
  case DK_Unknown: {
    errmsg = "Unknown";
    break;
  }
  case DK_EOF: {
    errmsg = "EOF";
    break;
  }
  case DK_UnrecognizedCharacter: {
    errmsg = "UnrecognizedCharacter";
    break;
  }
  case DK_MacroExprExpectedClosingBacktick: {
    errmsg = "MacroExprExpectedClosingBacktick";
    break;
  }
  case DK_NumLiteralNoFirstDigit: {
    errmsg = "NumLiteralNoFirstDigit";
    break;
  }
  case DK_NumLiteralFirstDigitUnderscore: {
    errmsg = "NumLiteralFirstDigitUnderscore";
    break;
  }
  case DK_NumLiteralUnrecognizedRadixCode: {
    errmsg = "NumLiteralUnrecognizedRadixCode";
    break;
  }
  case DK_NumLiteralDigitExceedsRadix: {
    errmsg = "NumLiteralDigitExceedsRadix";
    break;
  }
  case DK_NumLiteralOverflow: {
    errmsg = "NumLiteralOverflow";
    break;
  }
  case DK_NumLiteralUnknownCharacter: {
    errmsg = "NumLiteralUnknownCharacter";
    break;
  }
  case DK_FloatLiteralFirstDigitUnderscore: {
    errmsg = "FloatLiteralFirstDigitUnderscore";
    break;
  }
  case DK_FloatLiteralNoFirstDigit: {
    errmsg = "FloatLiteralNoFirstDigit";
    break;
  }
  case DK_FloatLiteralDigitExceedsRadix: {
    errmsg = "FloatLiteralDigitExceedsRadix";
    break;
  }
  case DK_FloatLiteralExceedsMaxPrecision: {
    errmsg = "FloatLiteralExceedsMaxPrecision";
    break;
  }
  case DK_CharLiteralUnrecognizedEscapeCode: {
    errmsg = "CharLiteralUnrecognizedEscapeCode";
    break;
  }
  case DK_CharLiteralExpectedCloseSingleQuote: {
    errmsg = "CharLiteralExpectedCloseSingleQuote";
    break;
  }
  case DK_UnexpectedLabel: {
    errmsg = "UnexpectedLabel";
    break;
  }
  case DK_LabelUnknownCharacter: {
    errmsg = "LabelUnknownCharacter";
    break;
  }
  case DK_ConstExprUnrecognizedLiteral: {
    errmsg = "ConstExprUnrecognizedLiteral";
    break;
  }
  case DK_StringLiteralTooLong: {
    errmsg = "StringLiteralTooLong";
    break;
  }
  case DK_StringLiteralUnrecognizedEscapeCode: {
    errmsg = "StringLiteralUnrecognizedEscapeCode";
    break;
  }
  case DK_StructLiteralExpectedEntry: {
    errmsg = "StructLiteralExpectedEntry";
    break;
  }
  case DK_StructLiteralExpectedRightBrace: {
    errmsg = "StructLiteralExpectedRightBrace";
    break;
  }
  case DK_StructLiteralExpectedLeftBrace: {
    errmsg = "StructLiteralExpectedLeftBrace";
    break;
  }
  case DK_PathExpectedIdentifier: {
    errmsg = "PathExpectedIdentifier";
    break;
  }
  case DK_StructMemberExpectedType: {
    errmsg = "StructMemberExpectedType";
    break;
  }
  case DK_StructMemberExpectedIdentifier: {
    errmsg = "StructMemberExpectedIdentifier";
    break;
  }
  case DK_StructMemberLiteralExpectedIdentifier: {
    errmsg = "StructMemberLiteralExpectedIdentifier";
    break;
  }
  case DK_StructMemberLiteralExpectedDefine: {
    errmsg = "StructMemberLiteralExpectedDefine";
    break;
  }
  case DK_FnValExprExpectedRightParen: {
    errmsg = "FnValExprExpectedRightParen";
    break;
  }
  case DK_FnValExprExpectedLeftParen: {
    errmsg = "FnValExprExpectedLeftParen";
    break;
  }
  case DK_FnValExprExpectedArrow: {
    errmsg = "FnValExprExpectedArrow";
    break;
  }
  case DK_TypeExprUnexpectedToken: {
    errmsg = "TypeExprUnexpectedToken";
    break;
  }
  case DK_TypeExprFieldAccessExpectedIdentifier: {
    errmsg = "TypeExprFieldAccessExpectedIdentifier";
    break;
  }
  case DK_TypeGroupExpectedRightBrace: {
    errmsg = "TypeGroupExpectedRightBrace";
    break;
  }
  case DK_PatGroupExpectedLeftBrace: {
    errmsg = "PatGroupExpectedLeftBrace";
    break;
  }
  case DK_PatGroupExpectedRightBrace: {
    errmsg = "PatGroupExpectedRightBrace";
    break;
  }
  case DK_PatStructExpectedLeftBrace: {
    errmsg = "PatStructExpectedLeftBrace";
    break;
  }
  case DK_PatStructExpectedRightBrace: {
    errmsg = "PatStructExpectedRightBrace";
    break;
  }
  case DK_PatStructExpectedIdentifier: {
    errmsg = "PatStructExpectedIdentifier";
    break;
  }
  case DK_PatStructExpectedDefine: {
    errmsg = "PatStructExpectedDefine";
    break;
  }
  case DK_ValDeclExpectedDefine: {
    errmsg = "ValDeclExpectedDefine";
    break;
  }
  case DK_ValDeclExpectedVal: {
    errmsg = "ValDeclExpectedVal";
    break;
  }
  case DK_TypeDeclExpectedIdentifier: {
    errmsg = "TypeDeclExpectedIdentifier";
    break;
  }
  case DK_TypeDeclExpectedDefine: {
    errmsg = "TypeDeclExpectedDefine";
    break;
  }
  case DK_StructExpectedLeftBrace: {
    errmsg = "StructExpectedLeftBrace";
    break;
  }
  case DK_StructExpectedRightBrace: {
    errmsg = "StructExpectedRightBrace";
    break;
  }
  case DK_FnTypeExprExpectedLeftParen: {
    errmsg = "FnTypeExprExpectedLeftParen";
    break;
  }
  case DK_FnTypeExprExpectedRightParen: {
    errmsg = "FnTypeExprExpectedRightParen";
    break;
  }
  case DK_FnTypeExprExpectedColon: {
    errmsg = "FnTypeExprExpectedColon";
    break;
  }
  case DK_MatchCaseNoArrow: {
    errmsg = "MatchCaseNoArrow";
    break;
  }
  case DK_MatchCaseNoPat: {
    errmsg = "MatchCaseNoPat";
    break;
  }
  case DK_MatchNoLeftBrace: {
    errmsg = "MatchNoLeftBrace";
    break;
  }
  case DK_MatchNoRightBrace: {
    errmsg = "MatchNoRightBrace";
    break;
  }
  case DK_MatchNoComma: {
    errmsg = "MatchNoComma";
    break;
  }
  case DK_BlockExpectedRightBrace: {
    errmsg = "BlockExpectedRightBrace";
    break;
  }
  case DK_CallExpectedParen: {
    errmsg = "CallExpectedParen";
    break;
  }
  case DK_UnexpectedToken: {
    errmsg = "UnexpectedToken";
    break;
  }
  case DK_FieldAccessExpectedIdentifier: {
    errmsg = "FieldAccessExpectedIdentifier";
    break;
  }
  }
  return errmsg;
}
