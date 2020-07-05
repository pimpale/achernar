#include "com_bigint.h"
#include "com_imath.h"
#include "com_assert.h"


com_bigint com_bigint_create(com_allocator_Handle h) {
    return (com_bigint) {
      ._array = com_vec_create(h),
      ._negative = false
    };
}

i64 com_bigint_release(com_bigint* a) {
  com_assert_m(a != NULL, "a is null");
  i64 val =  com_bigint_get_i64(a);
  com_vec_destroy(&a->_array);
  return val;
}

void com_bigint_set_i64(com_bigint* dest, i64 val) {
  com_assert_m(dest != NULL, "dest is null");
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
  com_assert_m(a != NULL, "a is null");
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

// Adds together a and b even if either A or B is the same
static void internal_value_add_u32_arr(com_bigint* dest, 
                                       u32* a, usize alen,
                                       u32* b, usize blen) {

  // Can't represent 
  usize new_dest_len = 
      com_imath_usize_min(com_imath_usize_max(alen, blen), com_imath_usize_maxval_m + 1;
  usize dest_len = com_vec_len_m(&a->_array, u32);
  u32 *arr = com_vec_get_m(&a->_array, 0, u32);

  // The procedure is
  // Add together each word of B and A, producing a result Array
  // Then for the length 

  u32 carry = 0;
  for(usize i = 0; i < arr_len; i++) {
    // aval is the value of a at this index
    u32 aval = arr[i];
    // bval is the value of b at this index
    u32 bval;
    if(i < blen) {
      bval = b[i];
    } else {
      bval = 0;
    }
    u64 tmp = aval + bval;
    if(tmp > com_imath_u32_maxval_m) {
      carry = tmp - com_imath_u32_maxval_m;
    }
    if(carry == 0) {
      break;
    }
  }

  // if it overflows the current length of the array add one
  if(carry != 0) {
    *com_vec_push_m(&a->_array, u32) = carry;
  }
}

static void internal_value_sub_u32_arr(com_bigint* a, u32 b) {
  
}

void com_bignum_add_u32(com_bigint* a, u32 b) {
  com_assert_m(a != NULL, "a is null");
  if(a->_negative) {
    internal_value_sub_u32(a, b);
  } else {
    internal_value_add_u32(a, b);
  }
}

void com_bignum_sub_u32(com_bigint* a, u32 b) {
  com_assert_m(a != NULL, "a is null");
  if(a->_negative) {
    internal_value_add_u32(a, b);
  } else {
    internal_value_sub_u32(a, b);
  }
}
