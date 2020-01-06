#ifndef ERRORS_H
#define ERRORS_H

#include <stdio.h>

#include "constants.h"

typedef enum ErrSeverity {
  ErrLevelDebug,
  ErrLevelInfo,
  ErrLevelWarn,
  ErrLevelError,
  ErrLevelFatal,
  ErrLevelUnknown,
} ErrSeverity;

typedef enum ErrVal {
  ErrOk,
  ErrOverflow,
  ErrNotsupported,
  ErrUnsafe,
  ErrBadargs,
  ErrOutofdate,
  ErrAllocfail,
  ErrMemory,
  ErrUnknown,
} ErrVal;

char* strErrSeverity(ErrSeverity level);
char* strErrVal(ErrVal val);

#define UNUSED(x) (void)(x)
#define PANIC() exit(EXIT_FAILURE)

#define LOG_ERROR(level, msg) \
  printf("%s: %s: %s\n", APPNAME, strErrSeverity(level), msg)

#define LOG_ERROR_ARGS(level, fmt, ...)                                    \
  do {                                                                     \
    char macro_message_formatted[MAX_PRINT_LENGTH];                        \
    snprintf(macro_message_formatted, MAX_PRINT_LENGTH, fmt, __VA_ARGS__); \
    printf("%s: %s: %s\n", APPNAME, strErrSeverity(level),                 \
           macro_message_formatted);                                       \
  } while (0)

#endif
