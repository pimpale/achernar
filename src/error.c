#include "error.h"

#include <inttypes.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>

#include "constants.h"

char *toStrSeverity(Severity level) {
  switch (level) {
  case SeverityDebug: return "debug";
  case SeverityInfo: return "info";
  case SeverityWarn: return "warn";
  case SeverityError: return "error";
  case SeverityFatal: return "fatal";
  case SeverityUnknown: return "unknown";
  }
}

char *toStrError(Error e) {
  switch (e) {
    case ErrorBadOption: return "E001: An invalid option or argument was provided. Use the -h flag for help.";
    case ErrorEOF: return "E002: Unexpected end of file.";
    case ErrorUnrecognizedCharacter: 
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
