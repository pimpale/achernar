#include "com_str.h"

com_str com_str_create(u8* data, usize len) {
  return (com_str) {
      .data = data,
      .len = len
  };
}

com_str com_str_from_asciiz(u8* asciiz) {
  // get length
  usize len = 0;
  while(asciiz[len] != 0) {
    len++;
  }

  return com_str_create(asciiz, len);
}

