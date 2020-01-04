#include "error.h"

char *levelstrerror(ErrSeverity level) {
  char *str = "unknown";

  switch (level) {
  case ERR_LEVEL_DEBUG: {
    str = "debug";
    break;
  }
  case ERR_LEVEL_INFO: {
    str = "info";
    break;
  }
  case ERR_LEVEL_WARN: {
    str = "warn";
    break;
  }
  case ERR_LEVEL_ERROR: {
    str = "error";
    break;
  }
  case ERR_LEVEL_FATAL: {
    str = "fatal";
    break;
  }
  case ERR_LEVEL_UNKNOWN: {
    str = "unknown";
    break;
  }
  }

  return (str);
}
