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
  case DK_PathExpectedIdentifier: {
    errmsg = "PathExpectedIdentifier";
    break;
  }
  case DK_BindingExpectedType: {
    errmsg = "BindingExpectedType";
    break;
  }
  case DK_BindingExpectedIdentifier: {
    errmsg = "BindingExpectedIdentifier";
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
  case DK_VarDeclStmntExpectedAssign: {
    errmsg = "VarDeclStmntExpectedAssign";
    break;
  }
  case DK_VarDeclStmntExpectedValue: {
    errmsg = "VarDeclStmntExpectedValue";
    break;
  }
  case DK_TypeAliasExpectedIdentifier: {
    errmsg = "TypeAliasExpectedIdentifier";
    break;
  }
  case DK_TypeAliasExpectedAssign: {
    errmsg = "TypeAliasExpectedAssign";
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
  case DK_StructExpectedComma: {
    errmsg = "StructExpectedComma";
    break;
  }
  case DK_TupleExpectedRightParen: {
    errmsg = "TupleExpectedRightParen";
    break;
  }
  case DK_TupleExpectedComma: {
    errmsg = "TupleExpectedComma";
    break;
  }
  case DK_FnDeclStmntExpectedRightParen: {
    errmsg = "FnDeclStmntExpectedRightParen";
    break;
  }
  case DK_FnDeclStmntExpectedLeftParen: {
    errmsg = "FnDeclStmntExpectedLeftParen";
    break;
  }
  case DK_FnDeclStmntExpectedComma: {
    errmsg = "FnDeclStmntExpectedComma";
    break;
  }
  case DK_FnDeclStmntExpectedIdentifier: {
    errmsg = "FnDeclStmntExpectedIdentifier";
    break;
  }
  case DK_FnDeclStmntExpectedColon: {
    errmsg = "FnDeclStmntExpectedColon";
    break;
  }
  case DK_FnDeclStmntExpectedAssign: {
    errmsg = "FnDeclStmntExpectedAssign";
    break;
  }
  case DK_FnDeclStmntExpectedBody: {
    errmsg = "FnDeclStmntExpectedBody";
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
  case DK_FnTypeExprExpectedComma: {
    errmsg = "FnTypeExprExpectedComma";
    break;
  }
  case DK_FnTypeExprExpectedColon: {
    errmsg = "FnTypeExprExpectedColon";
    break;
  }
  case DK_GroupExpectRightParen: {
    errmsg = "GroupExpectRightParen";
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
  case DK_BlockExpectedSemicolon: {
    errmsg = "BlockExpectedSemicolon";
    break;
  }
  case DK_BlockExpectedRightBrace: {
    errmsg = "BlockExpectedRightBrace";
    break;
  }
  case DK_ArrayAccessExpectedBracket: {
    errmsg = "ArrayAccessExpectedBracket";
    break;
  }
  case DK_CallExpectedComma: {
    errmsg = "CallExpectedComma";
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
  case DK_SubcomponentFailedToParse: {
    errmsg = "SubcomponentFailedToParse";
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
