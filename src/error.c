#include "error.h"

#include <inttypes.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>

#include "constants.h"

char* strDiagnosticKind(DiagnosticKind dk) {
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
  case DK_VarDeclStmntExpectedAssign: {
    errmsg = "VarDeclStmntExpectedAssign";
    break;
  }
  case DK_VarDeclStmntExpectedValue: {
    errmsg = "VarDeclStmntExpectedValue";
    break;
  }
  case DK_AliasDeclStmntExpectedIdentifier: {
    errmsg = "AliasDeclStmntExpectedIdentifier";
    break;
  }
  case DK_StructDeclStmntExpectedLeftBrace: {
    errmsg = "StructDeclStmntExpectedLeftBrace";
    break;
  }
  case DK_StructDeclStmntExpectedRightBrace: {
    errmsg = "StructDeclStmntExpectedRightBrace";
    break;
  }
  case DK_FnDeclStmntExpectedParen: {
    errmsg = "FnDeclStmntExpectedParen";
    break;
  }
  case DK_FnDeclStmntExpectedType: {
    errmsg = "FnDeclStmntExpectedType";
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
  case DK_GroupExpectRightParen: {
    errmsg = "GroupExpectRightParen";
    break;
  }
  case DK_MatchNoColon: {
    errmsg = "MatchNoColon";
    break;
  }
  case DK_MatchNoLeftbrace: {
    errmsg = "MatchNoLeftbrace";
    break;
  }
  case DK_MatchNoRightBrace: {
    errmsg = "MatchNoRightBrace";
    break;
  }
  case DK_BlockExpectedSemicolon: {
    errmsg = "BlockExpectedSemicolon";
    break;
  }
  case DK_ArrayAccessExpectedBracket: {
    errmsg = "ArrayAccessExpectedBracket";
    break;
  }
  case DK_FunctionCallExpectedParen: {
    errmsg = "FunctionCallExpectedParen";
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
  fprintf(stderr, APPNAME ": internal error @ %s:%d: %s\n", func, line, macro_message_formatted);
  fprintf(stderr, APPNAME ": report bugs at " APP_REPORT_BUG_LINK "\n");
}
