#include "utils.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define MAX_PRINT_LENGTH 4096

void logInternalError(const char* appname, uint32_t line,  const char *func, const char *fmt, ...) {
  char macro_message_formatted[MAX_PRINT_LENGTH];
  va_list args;
  va_start(args, fmt);
  vsnprintf(macro_message_formatted, MAX_PRINT_LENGTH, fmt, args);
  va_end(args);
  fprintf(stderr, "%s: internal error @ %s:%d: %s\n", appname, func, line,
          macro_message_formatted);
}
