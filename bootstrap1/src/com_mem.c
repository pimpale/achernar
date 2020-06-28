#include "com_mem.h"

#include "com_ensure.h"

void com_mem_zero(void* ptr, const usize len) {
  com_mem_set(ptr, len, 0);
}

void com_mem_set(void* ptr, const usize len, const u8 byte) {
  com_ensure_m(ptr != NULL, "ptr may not be NULL");
  u8* bytes = ptr;
  for(usize i = 0; i < len; i++) {
    bytes[len] = byte;
  }
}
