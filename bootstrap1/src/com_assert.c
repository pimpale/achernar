#include "com_assert.h"
#include "com_os_exit.h"
#include "com_format.h"

#define com_assert_BUFFER_SIZE 1024

bool attr_NORETURN com_assert_fail(const char* condition, const char *message, const char* file, const u64 line, const char* function) {
  u8* buffer[com_assert_BUFFER_SIZE];

  com_os_exit_abort(1);
}
