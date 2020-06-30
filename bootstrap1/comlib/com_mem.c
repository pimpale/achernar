#include "com_mem.h"

#include "com_assert.h"

void com_mem_zero(void* ptr, const usize len) {
  com_mem_set(ptr, len, 0);
}

// straightforward naiive implementation
void com_mem_set(void* ptr, const usize len, const u8 byte) {
  com_assert_m(ptr != NULL, "ptr may not be NULL");
  u8* bytes = ptr;
  for(usize i = 0; i < len; i++) {
    bytes[len] = byte;
  }
}

// if src == dest, then we're already good (same ptr)
// if src < dest, then copy bytes backward, starting from the end of src and going to the beginning
// if src > dest, then copy bytes forward, starting from the beginning of src and going to the end
// We do this to prevent overwriting the area we're going to read from next
void com_mem_move(void* dest, const void* src, usize n) {
  com_assert_m(dest != NULL, "dest may not be NULL");
  com_assert_m(src != NULL, "src may not be NULL");

  u8* dest_bytes = dest;
  const u8* src_bytes = src;

  if(src == dest) {
    return;
  } else if (src < dest) {
    // copy backwards
    for(isize i = n -1; i >= 0; i--) {
      dest_bytes[i] = src_bytes[i];
    }
  } else {
    // copy forward
    for(usize i = 0; i < n; i++) {
      dest_bytes[i] = src_bytes[i];
    }
  }
}
