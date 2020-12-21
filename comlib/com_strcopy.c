#include "com_strcopy.h"
#include "com_mem.h"

com_str_mut com_strcopy(com_str src, com_allocator_Handle handle) {
  u8 *dest = com_allocator_handle_get(handle);
  com_mem_move(dest, src.data, src.len);
  return (com_str_mut){.data = dest, .len = src.len};
}
