#include "error.h"

char* strErrSeverity(ErrSeverity level) {
  char* str = "unknown";

  switch (level) {
    case ErrLevelDebug: {
      str = "debug";
      break;
    }
    case ErrLevelInfo: {
      str = "info";
      break;
    }
    case ErrLevelWarn: {
      str = "warn";
      break;
    }
    case ErrLevelError: {
      str = "error";
      break;
    }
    case ErrLevelFatal: {
      str = "fatal";
      break;
    }
    case ErrLevelUnknown: {
      str = "unknown";
      break;
    }
  }

  return (str);
}

char* strErrVal(ErrVal val) {
  char* str = "unknown";

  switch (val) {
    case ErrOk: {
      str = "no error";
      break;
    }
    case ErrOverflow: {
      str = "integer overflow";
      break;
    }
    case ErrNotsupported: {
      str = "operation not supported";
      break;
    }
    case ErrUnsafe: {
      str = "operation would be unsafe";
      break;
    }
    case ErrBadargs: {
      str = "bad arguments or parameters provided";
      break;
    }
    case ErrOutofdate: {
      str = "result is out of date";
      break;
    }
    case ErrAllocfail: {
      str = "failed to allocate memory";
      break;
    }
    case ErrMemory: {
      str = "memory error";
      break;
    }
    case ErrUnknown: {
      str = "unknown error";
      break;
    }
  }
  return (str);
}
