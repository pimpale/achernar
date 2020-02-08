#include "error.h"

#include <inttypes.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>

#include "constants.h"

// TODO Good Code, will save for later
/* 
DiagnosticLogger *createDiagnosticLogger(DiagnosticLogger *dl, FILE *file) {
  dl->created = true;
  dl->destroyed = false;
  dl->messagePrinted = false;
  dl->file = file;

  fprintf(dl->file, "[");
  return dl;
}

DiagnosticLogger *destroyDiagnosticLogger(DiagnosticLogger *dl) {
  fprintf(dl->file, "]");
  dl->created = false;
  dl->messagePrinted = false;
  dl->destroyed = true;
  dl->file = NULL;
  return dl;
}

void logDiagnostic(DiagnosticLogger *dl, Diagnostic diagnostic) {

  char *severity;
  char *code;
  char *message;

  switch (diagnostic.type) {
  case ErrorOk: {
    severity = "error";
    code = "E000";
    message = "no error";
    break;
  }
  case ErrorUnknown: {
    severity = "error";
    code = "E001";
    message = "an unknown error has occured";
    break;
  }
  case ErrorEOF: {
    severity = "error";
    code = "E002";
    message = "unexpected end of file";
    break;
  }
  case ErrorUnrecognizedCharacter: {
    severity = "error";
    code = "E003";
    message = "unrecognized character";
    break;
  }
  case ErrorIntLiteralUnrecognizedRadixCode: {
    severity = "error";
    code = "E004";
    message = "radix code after 0 may only be `b` for 2, `o` for 8, `d` for "
              "10, and `x` for 16";
    break;
  }
  case ErrorIntLiteralDigitExceedsRadix: {
    severity = "error";
    code = "E005";
    message = "the digit value exceeds the radix specified";
    break;
  }
  case ErrorIntLiteralOverflow: {
    severity = "error";
    code = "E006";
    message = "the integer literal specified overflows 64 bits";
    break;
  }
  case ErrorFloatLiteralDigitExceedsRadix: {
    severity = "error";
    code = "E007";
    message = "the digit value exceeds the radix specified";
    break;
  }
  case ErrorFloatLiteralExceedsMaxPrecision: {
    severity = "error";
    code = "E008";
    message = "the float literal overflows the max precision";
    break;
  }
  case ErrorCharLiteralEmpty: {
    severity = "error";
    code = "E009";
    message = "the character literal must contain one character";
    break;
  }
  case ErrorCharLiteralUnrecognizedEscapeCode: {
    severity = "error";
    code = "E010";
    message = "unrecognized escape code";
    break;
  }
  case ErrorCharLiteralTooLong: {
    severity = "error";
    code = "E011";
    message = "the character literal is longer than one character";
    break;
  }
  case ErrorStringLiteralTooLong: {
    severity = "error";
    code = "E012";
    message = "the string literal is too long";
    break;
  }
  case ErrorStringLiteralUnrecognizedEscapeCode: {
    severity = "error";
    code = "E013";
    message = "unrecognized escape code";
    break;
  }
  case ErrorUnexpectedToken: {
    severity = "error";
    code = "E014";
    message = "unexpected token";
    break;
  }
  }

  // Print separator
  if (dl->messagePrinted) {
    fprintf(dl->file, ",");
  } else {
    dl->messagePrinted = true;
  }

  fprintf(dl->file,
          "{ \"severity\":\"%s\", \"code\":\"%s\", \"message\":\"%s\", "
          "\"ln\":%" PRIu64 ", \"col\":%" PRIu64 "}",
          severity, code, message, diagnostic.ln, diagnostic.col);
}
*/

void logInternalError(uint32_t line, const char *func, const char *fmt, ...) {
  char macro_message_formatted[MAX_PRINT_LENGTH];
  va_list args;
  va_start(args, fmt);
  vsnprintf(macro_message_formatted, MAX_PRINT_LENGTH, fmt, args);
  va_end(args);
  fprintf(stderr, APPNAME ": internal error @ %s:%d: %s\n", func, line, macro_message_formatted);
  fprintf(stderr, APPNAME ": report bugs at " APP_REPORT_BUG_LINK "\n");
}
