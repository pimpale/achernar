#ifndef ERRORS_H_
#define ERRORS_H_

#include <stdio.h>

#include "constants.h"

typedef enum ErrSeverity {
  ERR_LEVEL_DEBUG,
  ERR_LEVEL_INFO,
  ERR_LEVEL_WARN,
  ERR_LEVEL_ERROR,
  ERR_LEVEL_FATAL,
  ERR_LEVEL_UNKNOWN,
} ErrSeverity;

typedef enum ErrVal {
  ERR_OK,
  ERR_UNKNOWN,
  ERR_NOTSUPPORTED,
  ERR_UNSAFE,
  ERR_BADARGS,
  ERR_OUTOFDATE,
  ERR_ALLOCFAIL,
  ERR_MEMORY,
} ErrVal;

char *levelstrerror(ErrSeverity level);

#define UNUSED(x) (void)(x)
#define PANIC() exit(EXIT_FAILURE)

#define LOG_ERROR(level, msg)                                                  \
  printf("%s: %s: %s\n", APPNAME, levelstrerror(level), msg)

#define LOG_ERROR_ARGS(level, fmt, ...)                                        \
  do {                                                                         \
    char macro_message_formatted[MAX_PRINT_LENGTH];                            \
    snprintf(macro_message_formatted, MAX_PRINT_LENGTH, fmt, __VA_ARGS__);     \
    printf("%s: %s: %s\n", APPNAME, levelstrerror(level),                      \
           macro_message_formatted);                                           \
  } while (0)

#endif /* ERRORS_H_ */
