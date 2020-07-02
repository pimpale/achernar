#include "com_bigint.h"

#define u64_msb_m 0x80000000
#define u64_max_m 0xFFFFFFFF

com_bigint com_bigint_create(com_allocator_Handle h) {
    return (com_bigint) {
      ._array = com_vec_create(h),
      ._negative = false
    };
}


void com_bigint_set_i64(com_bigint* dest, i64 val) {
  // handle nagative
  u64 normalized;
  if(val < 0) {
      dest->_negative = true;
      normalized = -val;
  } else {
      dest->_negative = false;
      normalized = val;
  }
  // u64 means only 2 u32 s are needed in the vector
  com_vec *v = dest->_array ;
  com_vec_set_len_m(v, 2, u32);
  u32* arr = vec_get(v, 0);
  // first is lower 32 bits of input
  arr[0] = normalized & 0x00000000FFFFFFFF;
  // next is the upper 32 bits
  arr[1] = normalized >> (u64)32;
}

i64 com_bigint_release(com_bigint* a) {

}
