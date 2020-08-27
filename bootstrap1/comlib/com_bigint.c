#include "com_bigint.h"
#include "com_assert.h"
#include "com_imath.h"

com_bigint com_bigint_from(com_biguint magnitude, bool negative) {
  return (com_bigint){._negative = negative, ._magnitude = magnitude};
}

com_bigint com_bigint_create(com_allocator_Handle h) {
  return (com_bigint){._negative = false, ._magnitude = com_biguint_create(h)};
}

void com_bigint_magnitude(com_biguint *dest, const com_bigint *a) {
  com_biguint_set(dest, &a->_magnitude);
}

void com_bigint_destroy(com_bigint *a) { com_biguint_destroy(&a->_magnitude); }

// avoid overflow
#define positive_i64_min_m ((u64)(-(i64_min_m + 1)) + 1)

void com_bigint_set_i64(com_bigint *dest, i64 a) {
  u64 normalized;
  if (a < 0) {
    dest->_negative = true;
    // unsafe to negate a negative number normally due to overflow
    if (a == i64_min_m) {
      normalized = positive_i64_min_m;
    } else {
      normalized = (u64)-a;
    }
  } else {
    dest->_negative = false;
    normalized = (u64)a;
  }
  com_biguint_set_u64(&dest->_magnitude, normalized);
}

i64 com_bigint_get_i64(const com_bigint *a) {
  u64 magnitude = com_biguint_get_u64(&a->_magnitude);

  if (a->_negative) {
    // ensure no overflows
    return -(i64)com_imath_u64_min(magnitude, positive_i64_min_m -1) - 1;
  } else {
    return (i64)com_imath_u64_min(magnitude, (u64)i64_max_m);
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
static void internal_safe_sub(com_bigint *dest, const com_biguint *a,
                              const com_biguint *b) {
  switch (com_biguint_cmp(a, b)) {
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
static void internal_addop(com_bigint *dest, const com_biguint *a,
                           bool anegative, const com_biguint *b,
                           bool bnegative) {

  if (anegative) {
    if (bnegative) {
      // a and b both negative
      com_biguint_add(&dest->_magnitude, a, b);
      dest->_negative = true;
    } else {
      // a negative b positive
      internal_safe_sub(dest, b, a);
    }
  } else {
    if (bnegative) {
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

  const com_biguint *amag = &a->_magnitude;
  const com_biguint *bmag = &b->_magnitude;

  bool anegative = a->_negative;
  bool bnegative = b->_negative;

  internal_addop(dest, amag, anegative, bmag, bnegative);
}

void com_bigint_sub(com_bigint *dest, const com_bigint *a,
                    const com_bigint *b) {

  const com_biguint *amag = &a->_magnitude;
  const com_biguint *bmag = &b->_magnitude;

  bool anegative = a->_negative;
  // we invert this because subtraction of a negative number is positive
  bool bnegative = !b->_negative;

  internal_addop(dest, amag, anegative, bmag, bnegative);
}

void com_bigint_mul(com_bigint *dest, const com_bigint *a, const com_bigint *b,
                    com_allocator *allocator) {
  // if one of them is negative but not the other, it is negative
  // if both of them are the same kind, then it is not negative
  dest->_negative = a->_negative != b->_negative;
  com_biguint_mul(&dest->_magnitude, &a->_magnitude, &b->_magnitude, allocator);
}

void com_bigint_div(com_bigint *dest, const com_bigint *a, const com_bigint *b,
                    com_allocator *allocator) {
  // if one of them is negative but not the other, it is negative
  // if both of them are the same kind, then it is not negative
  dest->_negative = a->_negative != b->_negative;
  com_biguint_div(&dest->_magnitude, &a->_magnitude, &b->_magnitude, allocator);
}

void com_bigint_rem(com_bigint *dest, const com_bigint *a, const com_bigint *b,
                    com_allocator *allocator) {
  // remainder (%) always takes the sign of the dividend
  dest->_negative = a->_negative;
  com_biguint_rem(&dest->_magnitude, &a->_magnitude, &b->_magnitude, allocator);
}

void com_bigint_div_rem(com_bigint *quotient, com_bigint *remainder,
                        const com_bigint *a, const com_bigint *b,
                        com_allocator *allocator) {
  // if one of them is negative but not the other, it is negative
  // if both of them are the same kind, then it is not negative
  quotient->_negative = a->_negative != b->_negative;

  // remainder (%) always takes the sign of the dividend
  remainder->_negative = a->_negative;

  com_biguint_div_rem(&quotient->_magnitude, &remainder->_magnitude,
                      &a->_magnitude, &b->_magnitude, allocator);
}

// a - b
static void internal_safe_sub_uint_u32(com_bigint *dest, const com_biguint *a, u32 b) {
  switch (com_biguint_cmp_u64(a,  b) ) {
  // b == a
  case com_math_EQUAL: {
    com_bigint_set_i64(dest, 0);
    return;
  }
  // b < a
  case com_math_LESS: {
    com_biguint_sub_u32(&dest->_magnitude, a, b);
    dest->_negative = false;
    return;
  }
  // b > a
  case com_math_GREATER: {
    u64 ret = b - com_biguint_get_u64(a);
    com_biguint_set_u64(&dest->_magnitude, ret);
    dest->_negative = true;
    return;
  }
  }
}

static void internal_add_u32(com_bigint* dest, const com_bigint *a, u32 b) {
  const com_biguint* amag = &a->_magnitude;
  if(!a->_negative) {
    com_biguint_add_u32(&dest->_magnitude, &a->_magnitude, b);
    dest->_negative = false;
  } else {
      // a is negative
    if(com_biguint_cmp_u64(amag, b) == com_math_GREATER) {
        // b > a
        u64 ret = b - com_biguint_get_u64(amag);
        com_biguint_set_u64(&dest->_magnitude, ret);
        dest->_negative = false;
    } else {
        // b <= a
        com_biguint_sub_u32(&dest->_magnitude, amag, b);
        dest->_negative = true;
    }
  }
}

static void internal_sub_u32(com_bigint* dest, const com_bigint *a, u32 b) {
  const com_biguint* amag = &a->_magnitude;
  if(a->_negative) {
    com_biguint_add_u32(&dest->_magnitude, &a->_magnitude, b);
    dest->_negative = true;
  } else {
    // a is positive
    if(com_biguint_cmp_u64(amag, b) == com_math_GREATER) {
        // b > a
        u64 ret = b - com_biguint_get_u64(amag);
        com_biguint_set_u64(&dest->_magnitude, ret);
        dest->_negative = true;
    } else {
        // b <= a
        com_biguint_sub_u32(&dest->_magnitude, amag, b);
        dest->_negative = false;
    }
  }
}

void com_bigint_add_i32(com_bigint *dest, const com_bigint *a, i32 b) {
  if(b < 0) {
    internal_sub_u32(dest, a, (u32)(-(b +1)) + 1);
  } else {
    internal_add_u32(dest,a,(u32)b);
  }
}
void com_bigint_sub_i32(com_bigint *dest, const com_bigint *a, i32 b) {
  if(b < 0) {
    internal_add_u32(dest,a,(u32)b);
  } else {
    internal_sub_u32(dest, a, (u32)(-(b +1)) + 1);
  }
}

void com_bigint_mul_i32(com_bigint *dest, const com_bigint *a, i32 b) {
  u32 mag;
  if( b < 0 ) {
    mag = (u32)(-(b+1)) +1;
    dest->_negative = !a->_negative;
  } else {
      mag = (u32) b;
    dest->_negative = a->_negative;
  }
  com_biguint_mul_u32(&dest->_magnitude, &a->_magnitude, mag);
}

void com_bigint_div_i32(com_bigint *dest, const com_bigint *a, i32 b) {
  u32 mag;
  if( b < 0 ) {
    mag = (u32)(-(b+1)) +1;
    dest->_negative = !a->_negative;
  } else {
      mag = (u32) b;
    dest->_negative = a->_negative;
  }
  com_biguint_div_u32(&dest->_magnitude, &a->_magnitude, mag);
}

bool com_bigint_is_zero(const com_bigint *a) {
  return com_biguint_is_zero(&a->_magnitude);
}

com_math_signtype com_bigint_sign(const com_bigint *a) {
  if (com_biguint_is_zero(&a->_magnitude)) {
    return com_math_ZERO;
  } else if (a->_negative) {
    return com_math_NEGATIVE;
  } else {
    return com_math_POSITIVE;
  }
}

com_math_cmptype com_bigint_cmp(const com_bigint *a, const com_bigint *b) {
  // handle special case of a and b both being zero
  // signed zeros are a thing in this representation
  if (com_biguint_is_zero(&a->_magnitude) &&
      com_biguint_is_zero(&b->_magnitude)) {
    return com_math_EQUAL;
  }

  com_math_cmptype magcmp = com_biguint_cmp(&a->_magnitude, &b->_magnitude);

  if (b->_negative) {
    // b is negative
    switch (magcmp) {
    case com_math_GREATER: {
      return com_math_LESS;
    }
    case com_math_LESS: {
      if (a->_negative) {
        return com_math_GREATER;
      } else {
        return com_math_LESS;
      }
    }
    case com_math_EQUAL: {
      if (a->_negative) {
        return com_math_EQUAL;
      } else {
        return com_math_LESS;
      }
    }
    }
  } else {
    // b is positive
    switch (magcmp) {
    case com_math_GREATER: {
      return com_math_GREATER;
    }
    case com_math_LESS: {
      if (a->_negative) {
        return com_math_GREATER;
      } else {
        return com_math_LESS;
      }
    }
    case com_math_EQUAL: {
      if (a->_negative) {
        return com_math_EQUAL;
      } else {
        return com_math_LESS;
      }
    }
    }
  }
}

void com_bigint_negate(com_bigint *a) {
 a->_negative = !a->_negative;
}


usize com_bigint_len(const com_bigint *a) {
  return com_biguint_len(&a->_magnitude);
}

u32 com_bigint_get_at(const com_bigint *a, usize i) {
  return com_biguint_get_at(&a->_magnitude, i);
}

void com_bigint_set_at(com_bigint *a, usize i, u32 val) {
  com_biguint_set_at(&a->_magnitude, i, val);
}
