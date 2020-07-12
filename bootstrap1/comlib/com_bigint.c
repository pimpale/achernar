#include "com_bigint.h"
#include "com_assert.h"
#include "com_imath.h"

com_bigint com_bigint_create(com_allocator_Handle h) {
  return (com_bigint){._array = com_vec_create(h), ._negative = false};
}

i64 com_bigint_release(com_bigint *a) {
  com_assert_m(a != NULL, "a is null");
  i64 val = com_bigint_get_i64(a);
  com_vec_destroy(&a->_array);
  return val;
}

void com_bigint_set_i64(com_bigint *dest, i64 val) {
  com_assert_m(dest != NULL, "dest is null");
  // handle negative
  u64 normalized;
  if (val < 0) {
    dest->_negative = true;
    normalized = -val;
  } else {
    dest->_negative = false;
    normalized = val;
  }
  // u64 means only 2 u32 s are needed in the vector
  com_vec *v = &dest->_array;
  com_vec_set_len_m(v, 2, u32);

  u32 *arr = com_vec_get(v, 0);
  // first is lower 32 bits of input
  arr[0] = normalized & 0x00000000FFFFFFFFu;
  // next is the upper 32 bits
  arr[1] = normalized >> (u64)32;
}

i64 com_bigint_get_i64(const com_bigint *a) {
  com_assert_m(a != NULL, "a is null");
  u32 *arr = com_vec_get(&a->_array, 0);
  u64 val = ((u64)arr[1] << (u64)32) + (u64)arr[0];

  if (a->_negative) {
    val = com_imath_u64_clamp(val, 0, -i64_min_m);
    return -(i64)val;
  } else {
    val = com_imath_u64_clamp(val, 0, i64_max_m);
    return (i64)val;
  }
}

// Adds together a and b into DEST
// REQUIRES: `dest` is a pointer to a valid com_bigint
// REQUIRES: `a` is a pointer to an array of at least `alen` u32s
// REQUIRES: `b` is a pointer to an array of at least `blen` u32s
// REQUIRES: alen >= blen
// GUARANTEES: `dest` will contain the sum of a and b
static void internal_value_add_u32_arr(com_bigint *dest, u32 *a, usize alen,
                                       u32 *b, usize blen) {
  com_assert_m(alen >= blen, "alen is less than blen");
  // first we have to find the size of the result
  // NOTE: Thanks to carrying, the result size could be 1 larger than this
  // We guarantee that `alen` is the larger len
  usize dest_len = alen;

  // extend dest arr to new size
  com_vec_set_len(&dest->_array, dest_len);

  // get destination array
  u32 *dest_arr = com_vec_get_m(&dest->_array, 0, u32);

  // represents the amount to be carried after adding one word
  u8 carry = 0;

  // procedure is:
  // add together each word, if it overflows then we carry 1 over to the next
  // word

  // in this for loop, a[i] and b[i] are both valid
  for (usize i = 0; i < blen; i++) {
    u32 aval = a[i];
    u32 bval = b[i];
    u64 tmp = aval + bval + carry;

    if (tmp > u32_max_m) {
      // if the value overflows the capacity of a u32
      carry = 1;
      dest_arr[i] = tmp - u32_max_m;
    } else {
      // if the value can fit in a u32
      carry = 0;
      dest_arr[i] = tmp;
    }
  }

  // this is essentially the loop where we are finished adding,
  // and are just handling any times where carrying the 1 forward will cause
  // overflow and require another carry

  // in this for loop, only a[i] is valid
  for (usize i = blen; i < alen; i++) {
    u32 aval = a[i];
    u64 tmp = aval + carry;

    if (tmp > u32_max_m) {
      // if the value overflows the capacity of a u32
      dest_arr[i] = tmp - u32_max_m;
      carry = 1;
    } else {
      // if the value can fit in a u32
      dest_arr[i] = tmp;
      carry = 0;
      // we can break as a shortcut here, since it won't affect anything
      // now that we're finished carrying
      break;
    }
  }

  // in the last part, if we are still carrying, we need to extend the length
  // of dest to handle that
  if (carry != 0) {
    *com_vec_push_m(&dest->_array, u32) = carry;
  }
}

// compares the magnitude of b with reference to a
/// REQUIRES: `a` is a pointer to an array of at least `alen` u32s
/// REQUIRES: `b` is a pointer to an array of at least `blen` u32s
/// REQUIRES: alen >= blen
static com_math_cmptype internal_value_cmp_u32_arr(u32 *a, usize alen, u32 *b,
                                                   usize blen) {
  com_assert_m(alen >= blen, "alen is less than blen");

  // compare backwards

  // first we compare where only A is defined

  for (usize i = alen; i > blen; i--) {
    // in this form,  i will always be 1 greater than what we want
    u32 aval = a[i - 1];
    // since bval is not defined here we default to zero
    u32 bval = 0;

    if (aval < bval) {
      return com_math_GREATER;
    } else if (aval > bval) {
      return com_math_LESS;
    }
  }

  // next compare where both a and b are defined
  for (usize i = blen; i != 0; i--) {
    // note that in this form, i will always be 1 greater than what we want
    u32 aval = a[i - 1];
    u32 bval = b[i - 1];

    if (aval < bval) {
      return com_math_GREATER;
    } else if (aval > bval) {
      return com_math_LESS;
    }
  }

  return com_math_EQUAL;
}

com_math_cmptype com_bigint_cmp(const com_bigint* a, const com_bigint* b) {
  usize alen = com_vec_len_m(&a->_array, u32);
  usize blen = com_vec_len_m(&b->_array, u32);
  if(blen > alen) {
    return com
  }
}


// sets DEST to a - b
// REQUIRES: `dest` is a pointer to a valid com_bigint
// REQUIRES: `a` is a pointer to an array of at least `alen` u32s
// REQUIRES: `b` is a pointer to an array of at least `blen` u32s
// REQUIRES: alen >= blen
// REQUIRES: the value held by a is greater than the value held by
// GUARANTEES: `dest` will contain the difference of a and b
static void internal_value_sub_u32_arr(com_bigint *dest, u32 *a, usize alen,
                                       u32 *b, usize blen) {
  com_assert_m(alen >= blen, "alen is less than blen");
  com_assert_m(internal_value_cmp_u32_arr(a, alen, b, blen) == com_math_GREATER,
               "a < b");

  // first we have to find the size of the result
  // alen is the MAX width that our result could have
  // We guarantee that `alen` is the larger len
  usize dest_len = alen;

  // resize dest arr to new size
  com_vec_set_len(&dest->_array, dest_len);

  // get destination array
  u32 *dest_arr = com_vec_get_m(&dest->_array, 0, u32);

  // represents the amount to be borrowed if necessary
  u8 borrow = 0;

  // procedure is:
  // sub each word: a + next_word - b + borrow
  // If it's greater than max length, then we didn't need the borrow, and
  // subtract it before setting the word in the dest array
  // Else, we did need the borrow, and set borrow to -1 so we can
  // subtract it from the next word

  // in this for loop, a[i] and b[i] are both valid
  for (usize i = 0; i < blen; i++) {
    u32 aval = a[i];
    u32 bval = b[i];
    u64 tmp = u32_max_m + aval - bval - borrow;

    if (tmp > u32_max_m) {
      // if the value overflows the capacity of a u32
      // Means we didn't need the borrow
      borrow = 0;
      dest_arr[i] = tmp - u32_max_m;
    } else {
      // if the value can fit in a u32
      // Means we needed the borrow
      borrow = 1;
      dest_arr[i] = tmp;
    }
  }

  // this is essentially the loop where we are finished subtracting,
  // and are just handling any times where borrowing the forward will cause
  // problems and require another carry

  // in this for loop, only a[i] is valid
  for (usize i = blen; i < alen; i++) {
    u32 aval = a[i];
    u64 tmp = u32_max_m + aval - borrow;

    if (tmp > u32_max_m) {
      // if the value overflows the capacity of a u32
      dest_arr[i] = tmp - u32_max_m;
      borrow = 1;
    } else {
      // if the value can fit in a u32
      dest_arr[i] = tmp;
      borrow = 0;
      // we can break as a shortcut here, since it won't affect anything
      // now that we're finished borrowing
      break;
    }
  }

  com_assert_m(borrow == 0,
               "borrow at end, a is less than b, mistakes were made");
}

void com_bignum_add_u32(com_bigint *a, u32 b) {
  com_assert_m(a != NULL, "a is null");
  usize a_len = com_vec_len_m(&a->_array, u32);
  u32 *a_arr = com_vec_get_m(&a->_array, 0, u32);
  if (a_len == 0) {
    // means that it's equivalnt to zero
    // zero is not negative
    a->_negative = false;
    // add with the longer argument first
    internal_value_add_u32_arr(a, &b, 1u, a_arr, a_len);
  } else {
    // we treat the `b` like a 1 length array
    if (a->_negative) {
      internal_value_sub_u32_arr(a, a_arr, a_len, &b, 1u);
    } else {
      internal_value_add_u32_arr(a, a_arr, a_len, &b, 1u);
    }
  }
}

void com_bignum_sub_u32(com_bigint *a, u32 b) {
  com_assert_m(a != NULL, "a is null");
  usize a_len = com_vec_len_m(&a->_array, u32);
  u32 *a_arr = com_vec_get_m(&a->_array, 0, u32);
  // we treat the `b` like a 1 length array
  if(a_len == 0) {
    a->_negative = false;
    internal_value_sub_u32_arr(a, &b, 1u, a_arr, a_len);
  }
  if (a->_negative) {
    internal_value_add_u32_arr(a, a_arr, a_len, &b, 1u);
  } else {
    internal_value_sub_u32_arr(a, a_arr, a_len, &b, 1u);
  }
}
