#include "error.h"

#include <inttypes.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>

#include "constants.h"

char *toStrSeverity(Severity level) {
  switch (level) {
  case SeverityDebug:
    return "debug";
  case SeverityInfo:
    return "info";
  case SeverityWarn:
    return "warn";
  case SeverityError:
    return "error";
  case SeverityFatal:
    return "fatal";
  case SeverityUnknown:
    return "unknown";
  }
}

char *toStrError(Error e) {
  switch (e) {
  case ErrorOk:
    return "E000: no error";
  case ErrorBadOption:
    return "E001: an invalid option or argument was provided. Use the -h flag "
           "for help.";
  case ErrorEOF:
    return "E002: unexpected end of file";
  case ErrorUnrecognizedCharacter:
    return "E003: unrecognized character";
  case ErrorIntLiteralUnrecognizedRadixCode:
    return "E004: radix code after 0 may only be `b` for 2, `o` for 8, `d` for "
           "10, and `x` for 16";
  case ErrorIntLiteralDigitExceedsRadix:
    return "E005: the digit value exceeds the radix specified";
  case ErrorIntLiteralOverflow:
    return "E006: the integer literal specified overflows 64 bits";
  case ErrorFloatLiteralDigitExceedsRadix:
    return "E007: the digit value exceeds the radix specified";
  case ErrorFloatLiteralExceedsMaxPrecision:
    return "E008: the float literal overflows the max precision";
  case ErrorCharLiteralEmpty:
    return "E009: the character literal must contain one character";
  case ErrorCharLiteralUnrecognizedEscapeCode:
    return "E010: unrecognized escape code";
  case ErrorCharLiteralTooLong:
    return "E011: the character literal is longer than one character";
  case ErrorStringLiteralTooLong:
    return "E012: the string literal is too long";
  case ErrorStringLiteralUnrecognizedEscapeCode:
    return "E013: unrecognized escape code";
  }
}

void logError(Severity severity, Error err, uint64_t ln, uint64_t col) {
  fprintf(stderr, APPNAME ": %s %s @ ln %" PRIu64 " col %" PRIu64,
          toStrSeverity(severity), toStrError(err), ln, col);
}

void logInternalError(uint64_t line, const char *func, const char *fmt, ...) {
  char macro_message_formatted[MAX_PRINT_LENGTH];
  va_list args;
  va_start(args, fmt);
  vsnprintf(macro_message_formatted, MAX_PRINT_LENGTH, fmt, args);
  va_end(args);
  fprintf(stderr, APPNAME ": internal error: %s\n", macro_message_formatted);
  fprintf(stderr, APPNAME ": report bugs at " APP_REPORT_BUG_LINK "\n");
}
