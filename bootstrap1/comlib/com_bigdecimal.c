#include "com_bigdecimal.h"
#include "com_assert.h"
#include "com_imath.h"

com_bigdecimal com_bigdecimal_from(com_bigint value) {
  return (com_bigdecimal){._downscale = 0, ._value = value};
}

com_bigdecimal com_bigdecimal_create(com_allocator_Handle h) {
  return com_bigdecimal_from(com_bigint_create(h));
}

void com_bigdecimal_magnitude(com_biguint *dest, const com_bigdecimal *a) {
  com_bigint_magnitude(dest, &a->_value);
}

void com_bigdecimal_destroy(com_bigdecimal *a) {
  com_bigint_destroy(&a->_value);}

// avoid overflow
#define positive_i64_min_m ((u64)(-(i64_min_m + 1)) + 1)

void com_bigdecimal_set_i64(com_bigdecimal *dest, i64 a) {
  // set value
  com_bigint_set_i64(&dest->_value, a);
  // scale is zero
  dest->_downscale = 0;
}

static u64 internal_get_magnitude(const com_bigdecimal *a) {
  usize len = com_bigint_len(&a->_value);

  // if there are more than 2 u32 words then it won't fit in a u64
  isize scale = (isize)len - (isize)a->_downscale > 2;
  if (scale > 2) {
    return false;
  }
  if (scale <= 1) {
    return true;
  }

  // here scale must be 2
  // this means that both of these are defined
  u64 lsw = com_bigint_get_at(&a->_value, a->_downscale);
  u64 msw = com_bigint_get_at(&a->_value, a->_downscale + 1);

  // reconstruct magnitude
  return (msw << 32) + lsw;
}

i64 com_bigdecimal_get_i64(const com_bigdecimal *a) {
  u64 magnitude = internal_get_magnitude(a);

  if (com_bigint_sign(&a->_value) == com_math_NEGATIVE) {
    // ensure no owverflows
    return -(i64)com_imath_u64_min(magnitude, positive_i64_min_m - 1) - 1;
  } else {
    // return if we can fit it properly
    return magnitude <= i64_max_m;
  }
}

bool com_bigdecimal_fits_i64(const com_bigdecimal *a) {
  u64 magnitude = internal_get_magnitude(a);

  if (com_bigint_sign(&a->_value) == com_math_NEGATIVE) {
    return magnitude <= positive_i64_min_m;
  } else {
    // return if we can fit it properly
    return magnitude <= i64_max_m;
  }
}

// lossy by definiton but we could definitely make this more accurate
// TODO
f64 com_bigdecimal_get_f64(const com_bigdecimal *a) {
  f64 magnitude = com_bigint_get_f64(&a->_value);
  magnitude /= 2 << (a->_downscale * 32);
  return magnitude;
}

void com_bigdecimal_set(com_bigdecimal *dest, const com_bigdecimal *src) {
  dest->_downscale = src->_downscale;
  com_bigint_set(&dest->_value, &src->_value);
}

// Sets the downscale  of dest
// REQUIRES: `dest` is a valid pointer to a valid `com_bigdecimal`
// GUARANTEES: if `downscale` > `dest._downscale`, `dest` will have the same int
// value as old dest 
// GUARANTEES: `dest`'s downscale will be equal to `downscale`
static void internal_set_scale(com_bigdecimal *dest, u32 downscale) {
  u32 sc1 = dest->_downscale;
  dest->_downscale = downscale;
  if (downscale >= sc1) {
    com_bigint_lshift(&dest->_value, &dest->_value, downscale - sc1);
  } else {
    com_bigint_rshift(&dest->_value, &dest->_value, sc1 - downscale);
  }
}



void com_bigdecimal_add(com_bigdecimal *dest, const com_bigdecimal *a,
                        const com_bigdecimal *b) {
  u64 ascale = a->_downscale;
  u64 bscale = b->_downscale;

  // shift smaller entry so that they are both the same size (adjusting the
  // scale) add rescale a and b
}

void com_bigdecimal_sub(com_bigdecimal *dest, const com_bigdecimal *a,
                        const com_bigdecimal *b) {

  const com_biguint *amag = &a->_magnitude;
  const com_biguint *bmag = &b->_magnitude;

  bool anegative = a->_negative;
  // we invert this because subtraction of a negative number is positive
  bool bnegative = !b->_negative;

  internal_addop(dest, amag, anegative, bmag, bnegative);
}

void com_bigdecimal_mul(com_bigdecimal *dest, const com_bigdecimal *a,
                        const com_bigdecimal *b, com_allocator *allocator) {
  // if one of them is negative but not the other, it is negative
  // if both of them are the same kind, then it is not negative
  dest->_negative = a->_negative != b->_negative;
  com_biguint_mul(&dest->_magnitude, &a->_magnitude, &b->_magnitude, allocator);
}

void com_bigdecimal_div(com_bigdecimal *dest, const com_bigdecimal *a,
                        const com_bigdecimal *b, com_allocator *allocator) {
  // if one of them is negative but not the other, it is negative
  // if both of them are the same kind, then it is not negative
  dest->_negative = a->_negative != b->_negative;
  com_biguint_div(&dest->_magnitude, &a->_magnitude, &b->_magnitude, allocator);
}

void com_bigdecimal_rem(com_bigdecimal *dest, const com_bigdecimal *a,
                        const com_bigdecimal *b, com_allocator *allocator) {
  // remainder (%) always takes the sign of the dividend
  dest->_negative = a->_negative;
  com_biguint_rem(&dest->_magnitude, &a->_magnitude, &b->_magnitude, allocator);
}

void com_bigdecimal_div_rem(com_bigdecimal *quotient, com_bigdecimal *remainder,
                            const com_bigdecimal *a, const com_bigdecimal *b,
                            com_allocator *allocator) {
  // if one of them is negative but not the other, it is negative
  // if both of them are the same kind, then it is not negative
  quotient->_negative = a->_negative != b->_negative;

  // remainder (%) always takes the sign of the dividend
  remainder->_negative = a->_negative;

  com_biguint_div_rem(&quotient->_magnitude, &remainder->_magnitude,
                      &a->_magnitude, &b->_magnitude, allocator);
}

bool com_bigdecimal_is_zero(const com_bigdecimal *a) {
  return com_biguint_is_zero(&a->_magnitude);
}

com_math_signtype com_bigdecimal_sign(const com_bigdecimal *a) {
  if (com_biguint_is_zero(&a->_magnitude)) {
    return com_math_ZERO;
  } else if (a->_negative) {
    return com_math_NEGATIVE;
  } else {
    return com_math_POSITIVE;
  }
}

com_math_cmptype com_bigdecimal_cmp(const com_bigdecimal *a,
                                    const com_bigdecimal *b) {
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
