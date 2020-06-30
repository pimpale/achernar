#include "com_assert.h"
#include "com_os_exit.h"
#include "com_os_io.h"
#include "com_format.h"

#include "com_allocator_passthrough.h"
#include "com_vec.h"


#define com_assert_BUFFER_SIZE 1024

bool attr_NORETURN com_assert_fail(const u8* condition, const u8* message, const u8* file, const u64 line, const u8* function) {
  // define backing buffer
  u8* buffer[com_assert_BUFFER_SIZE];

  // create passthrough allocator to use static array
  com_allocator_passthrough_Backing backing;
  com_Allocator palctr= com_allocator_passthrough(buffer, com_assert_BUFFER_SIZE, &backing);

  // create vector
  com_vec builder = com_vec_create(com_allocator_handle_create(&palctr, (com_allocator_HandleData) {
      .len=com_assert_BUFFER_SIZE,
      .flags=com_allocator_defaults(&palctr)
  }));

  // TODO write filesystem interface

  com_format_str(&builder, com_str_from_literal_m("{"));
  com_format_str
  com_format_str(&builder, com_str_from_literal_m("}"));

  com_os_exit_abort(1);
}
