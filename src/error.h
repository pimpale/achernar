#ifndef ERRORS_H
#define ERRORS_H

#include <stdio.h>


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

void logError(ErrSeverity level, char* fmt, ...);

#define UNUSED(x) (void)(x)
#define PANIC() exit(EXIT_FAILURE)

#endif
