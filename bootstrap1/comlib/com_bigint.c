#include "com_bigint.h"
#include "com_imath.h"


com_bigint com_bigint_create(com_allocator_Handle h) {
    return (com_bigint) {
      ._array = com_vec_create(h),
      ._negative = false
    };
}

i64 com_bigint_release(com_bigint* a) {
  i64 val =  com_bigint_get_i64(a);
  com_vec_destroy(&a->_array);
  return val;
}

void com_bigint_set_i64(com_bigint* dest, i64 val) {
  // handle negative
  u64 normalized;
  if(val < 0) {
      dest->_negative = true;
      normalized = -val;
  } else {
      dest->_negative = false;
      normalized = val;
  }
  // u64 means only 2 u32 s are needed in the vector
  com_vec *v = &dest->_array ;
  com_vec_set_len_m(v, 2, u32);

  u32* arr = com_vec_get(v, 0);
  // first is lower 32 bits of input
  arr[0] = normalized & 0x00000000FFFFFFFF;
  // next is the upper 32 bits
  arr[1] = normalized >> (u64)32;
}

i64 com_bigint_get_i64(const com_bigint* a) {
  u32* arr = com_vec_get(&a->_array, 0);
  u64 val = ((u64)arr[1] << (u64)32) + (u64)arr[0];

  if(a->_negative) {
    val = com_imath_u64_clamp(val, 0, -com_imath_i64_minval_m);
    return -(i64)val;
  } else {
    val = com_imath_u64_clamp(val, 0, com_imath_i64_maxval_m);
    return (i64)val;
  }
}
