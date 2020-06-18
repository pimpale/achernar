#ifndef CONSTANTS_H_
#define CONSTANTS_H_

#include <stdlib.h>

#define APPNAME "bplus"
#define APP_REPORT_BUG_LINK "https://github.com/pimpale/bplus/issues"
#define DEBUG 1

#define MAX_PRINT_LENGTH 4096

#define UNUSED __attribute__ ((unused))
#define ZERO(ptr) (memset(ptr, 0, sizeof(*ptr)))

#endif
