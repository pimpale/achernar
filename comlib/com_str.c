#include "com_str.h"

com_str_mut com_str_asciiz(u8 *asciiz) {
  // get length
  usize len = 0;
  while (asciiz[len] != 0) {
    len++;
  }

  return (com_str_mut){.data = asciiz, .len = len};
}

bool com_str_equal(com_str a, com_str b) {
  if (a.len != b.len) {
    return false;
  }

  for (usize i = 0; i < a.len; i++) {
    if (a.data[i] != b.data[i]) {
      return false;
    }
  }
  return true;
}

com_str com_str_demut(com_str_mut mutstr) {
  return (com_str){.data = mutstr.data, .len = mutstr.len};
}
