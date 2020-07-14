#include "com_bigint.h"
#include "com_assert.h"
#include "com_imath.h"

com_bigint com_bigint_from(com_biguint magnitude, bool negative) {
  return (com_bigint){._negative = negative, ._magnitude = magnitude};
}

com_bigint com_bigint_create(com_allocator_Handle h) {
  return (com_bigint){._negative = false, ._magnitude = com_biguint_create(h)};
}

const com_biguint *com_bigint_magnitude(const com_bigint *a) {
  return &a->_magnitude;
}

void com_bigint_destroy(com_bigint *a) { com_biguint_destroy(&a->_magnitude); }

// avoid overflow
#define positive_i64_min_m ((u64)(-(i64_min_m + 1)) + 1)

void com_bigint_set_i64(com_bigint *dest, i64 a) {
  u64 normalized;
  if(a < 0) {
    dest->_negative = true;
    // unsafe to negate a negative number normally due to overflow
    if(a == i64_min_m) {
      normalized = positive_i64_min_m;
    } else {
      normalized = -a;
    }
  } else {
    dest->_negative = false;
    normalized = a;
  }
  com_biguint_set_u64(&dest->_magnitude, normalized);
}

i64 com_bigint_get_i64(const com_bigint *a) {
  u64 magnitude = com_biguint_get_u64(&a->_magnitude);
  if (magnitude == 0) {
    return 0;
  }

  if (a->_negative) {
    return -(i64)com_imath_u64_max(magnitude, positive_i64_min_m);
  } else {
    return com_imath_u64_max(magnitude, i64_max_m);
  }
}

bool com_bigint_fits_i64(const com_bigint *a) {
  u64 magnitude = com_biguint_get_u64(&a->_magnitude);
  if (a->_negative) {
    return magnitude <= positive_i64_min_m;
  } else {
    // return if we can fit it properly
    return magnitude <= i64_max_m;
  }
}

f64 com_bigint_get_f64(const com_bigint *a) {
  f64 magnitude = com_biguint_get_f64(&a->_magnitude);
  if (a->_negative) {
    return -magnitude;
  } else {
    return magnitude;
  }
}

void com_bigint_set(com_bigint *dest, const com_bigint *src) {
  dest->_negative = src->_negative;
  com_biguint_set(&dest->_magnitude, &src->_magnitude);
}

// a - b
static void internal_safe_sub(com_bigint* dest, const com_biguint* a, const com_biguint *b) {
  switch(com_biguint_cmp(a, b)) {
    // b == a
    case com_math_EQUAL: {
      com_bigint_set_i64(dest, 0);
      return;
    }
    // b < a
    case com_math_LESS: {
      com_biguint_sub(&dest->_magnitude, a, b);
      dest->_negative = false;
      return;
    }
    // b > a
    case com_math_GREATER: {
      com_biguint_sub(&dest->_magnitude, b, a);
      dest->_negative = true;
      return;
    }
  }
}

/// sets dest to the addition of these two adds or subtracts 
static void internal_addop(com_bigint* dest, const com_biguint *a, bool anegative, const com_biguint* b, bool bnegative) {

  if(anegative) {
    if(bnegative) {
      // a and b both negative
      com_biguint_add(&dest->_magnitude, a, b);
      dest->_negative = true;
    } else {
      // a negative b positive
      internal_safe_sub(dest, b, a);
    }
  } else {
    if(bnegative) {
      // a positive b negative
      internal_safe_sub(dest, a, b);
    } else {
      // a and b both positive
      com_biguint_add(&dest->_magnitude, a, b);
      dest->_negative = false;
    }
  }
}

void com_bigint_add(com_bigint *dest, const com_bigint *a,
                     const com_bigint *b) {

  const com_biguint* amag = &a->_magnitude;
  const com_biguint* bmag = &b->_magnitude;

  bool anegative = a->_negative;
  bool bnegative = b->_negative;

  internal_addop(dest, amag, anegative, bmag, bnegative);
}

void com_bigint_sub(com_bigint *dest, const com_bigint *a,
                     const com_bigint *b) {

  const com_biguint* amag = &a->_magnitude;
  const com_biguint* bmag = &b->_magnitude;

  bool anegative = a->_negative;
  // we invert this because subtraction of a negative number is positive
  bool bnegative = !b->_negative;

  internal_addop(dest, amag, anegative, bmag, bnegative);
}

void 

