#include "com_strcopy.h"
#include "com_assert.h"
#include "com_mem.h"

com_str_mut com_strcopy_to_handle(com_str src, com_allocator_Handle handle) {
  com_assert_m(com_allocator_handle_query(handle).len >= src.len,
               "handle is not large enough");
  u8 *dest = com_allocator_handle_get(handle);
  com_mem_move(dest, src.data, src.len);
  return (com_str_mut){.data = dest, .len = src.len};
}

com_str_mut com_strcopy_noleak(com_str src, com_allocator *a) {
  com_assert_m(com_allocator_supports(a) & com_allocator_NOLEAK,
               "allocator does not support noleak");
  return com_strcopy_to_handle(
      src, com_allocator_alloc(a, (com_allocator_HandleData){
                                      .len = src.len,
                                      .flags = com_allocator_defaults(a) |
                                               com_allocator_NOLEAK}));
}
