#include "error.h"

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
  case DK_EOF: {
    errmsg = "EOF";
    break;
  }
  case DK_Unknown: {
    errmsg = "Unknown";
    break;
  }
  case DK_UnrecognizedCharacter: {
    errmsg = "UnrecognizedCharacter";
    break;
  }
  case DK_IntLiteralUnrecognizedRadixCode: {
    errmsg = "IntLiteralUnrecognizedRadixCode";
    break;
  }
  case DK_IntLiteralDigitExceedsRadix: {
    errmsg = "IntLiteralDigitExceedsRadix";
    break;
  }
  case DK_IntLiteralOverflow: {
    errmsg = "IntLiteralOverflow";
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
  case DK_CharLiteralEmpty: {
    errmsg = "CharLiteralEmpty";
    break;
  }
  case DK_CharLiteralTooLong: {
    errmsg = "CharLiteralTooLong";
    break;
  }
  case DK_CharLiteralUnrecognizedEscapeCode: {
    errmsg = "CharLiteralUnrecognizedEscapeCode";
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
  case DK_StructMemberLiteralExpectedValue: {
    errmsg = "StructMemberLiteralExpectedValue";
    break;
  }
  case DK_StructMemberLiteralExpectedIdentifier: {
    errmsg = "StructMemberLiteralExpectedIdentifier";
    break;
  }
  case DK_PatternGroupExpectedRightBrace: {
    errmsg = "PatternGroupExpectedRightBrace";
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
  case DK_ValDeclExpectedAssign: {
    errmsg = "ValDeclExpectedAssign";
    break;
  }
  case DK_ValDeclExpectedValue: {
    errmsg = "ValDeclExpectedValue";
    break;
  }
  case DK_TypeDeclExpectedIdentifier: {
    errmsg = "TypeDeclExpectedIdentifier";
    break;
  }
  case DK_TypeDeclExpectedAssign: {
    errmsg = "TypeDeclExpectedAssign";
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
  case DK_FnValueExprExpectedRightParen: {
    errmsg = "FnValueExprExpectedRightParen";
    break;
  }
  case DK_FnValueExprExpectedLeftParen: {
    errmsg = "FnValueExprExpectedLeftParen";
    break;
  }
  case DK_FnValueExprExpectedArrow: {
    errmsg = "FnValueExprExpectedArrow";
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
  case DK_MatchCaseNoColon: {
    errmsg = "MatchCaseNoColon";
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

void logInternalError(uint32_t line, const char *func, const char *fmt, ...) {
  char macro_message_formatted[MAX_PRINT_LENGTH];
  va_list args;
  va_start(args, fmt);
  vsnprintf(macro_message_formatted, MAX_PRINT_LENGTH, fmt, args);
  va_end(args);
  fprintf(stderr, APPNAME ": internal error @ %s:%d: %s\n", func, line,
          macro_message_formatted);
  fprintf(stderr, APPNAME ": report bugs at " APP_REPORT_BUG_LINK "\n");
}
