#include "com_bigdecimal.h"
#include "com_assert.h"
#include "com_imath.h"

com_bigdecimal com_bigdecimal_from(com_bigint value) {
  return (com_bigdecimal){._precision = 0, ._value = value};
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
  dest->_precision = 0;
}

static u64 internal_get_magnitude(const com_bigdecimal *a) {
  usize len = com_bigint_len(&a->_value);

  // if there are more than 2 u32 words then it won't fit in a u64
  isize scale = (isize)len - (isize)a->_precision > 2;
  if (scale > 2) {
    return false;
  }
  if (scale <= 1) {
    return true;
  }

  // here scale must be 2
  // this means that both of these are defined
  u64 lsw = com_bigint_get_at(&a->_value, a->_precision);
  u64 msw = com_bigint_get_at(&a->_value, a->_precision + 1);

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
  magnitude /= 2 << (a->_precision * 32);
  return magnitude;
}

void com_bigdecimal_set(com_bigdecimal *dest, const com_bigdecimal *src) {
  dest->_precision = src->_precision;
  com_bigint_set(&dest->_value, &src->_value);
}

// Sets the precision of dest
// REQUIRES: `dest` is a valid pointer to a valid `com_bigdecimal`
// GUARANTEES: if `precision` > `dest._precision`, no information will be lost
// GUARANTEES: `dest`'s precision will be equal to `precision`
static void internal_set_precision(com_bigdecimal *target, u32 precision) {
  u32 sc1 = target->_precision;
  target->_precision = downscale;
  if (downscale >= sc1) {
    com_bigint_lshift(&target->_value, &target->_value, downscale - sc1);
  } else {
    com_bigint_rshift(&target->_value, &target->_value, sc1 - downscale);
  }
}

// add together a and b 
// REQUIRES: a len > b len
// 
static internal_add(com_bigdecimal* dest, com_bigdecimal* a, com_bigdecimal *b);

void com_bigdecimal_add(com_bigdecimal *dest, const com_bigdecimal *a,
                        const com_bigdecimal *b) {
  com_assert_m(a != NULL, "a is null");
  com_assert_m(b != NULL, "b is null");
  com_assert_m(dest != NULL, "dest is null");
  com_assert_m(a->_precision == b->_precision, "a and b do not have the same precision");

  com_bigint_add(&dest->_value, &a->_value, &b->_value);
  dest->_precision = a->_precision;
}

void com_bigdecimal_sub(com_bigdecimal *dest, const com_bigdecimal *a,
                        const com_bigdecimal *b) {
  com_assert_m(a != NULL, "a is null");
  com_assert_m(b != NULL, "b is null");
  com_assert_m(dest != NULL, "dest is null");
  com_assert_m(a->_precision == b->_precision, "a and b do not have the same precision");

  com_bigint_sub(&dest->_value, &a->_value, &b->_value);
  dest->_precision = a->_precision;
}

void com_bigdecimal_mul(com_bigdecimal *dest, const com_bigdecimal *a,
                        const com_bigdecimal *b, com_allocator *allocator) {
  com_assert_m(a != NULL, "a is null");
  com_assert_m(b != NULL, "b is null");
  com_assert_m(dest != NULL, "dest is null");

  com_bigint_mul(&dest->_value, &a->_value, &b->_value, allocator);
  dest->_precision = a->_precision + b->_precision;
}

void com_bigdecimal_div(com_bigdecimal *dest, const com_bigdecimal *a,
                        const com_bigdecimal *b, com_allocator *allocator) {
  // if one of them is negative but not the other, it is negative
  // if both of them are the same kind, then it is not negative
  dest->_negative = a->_negative != b->_negative;
  com_biguint_div(&dest->_magnitude, &a->_magnitude, &b->_magnitude, allocator);
}

bool com_bigdecimal_is_zero(const com_bigdecimal *a) {
  com_assert_m(a != NULL, "a is null");
  return com_bigint_is_zero(&a->_value);
}

com_math_signtype com_bigdecimal_sign(const com_bigdecimal *a) {
  com_assert_m(a != NULL, "a is null");
    return com_bigint_sign(&a->_value) ;
}

com_math_cmptype com_bigdecimal_cmp(const com_bigdecimal *a,
                                    const com_bigdecimal *b) {

  com_assert_m(a != NULL, "a is null");
  com_assert_m(b != NULL, "b is null");
  com_assert_m(a->_precision == b->_precision, "a and b do not have the same precision") ;
  return com_bigint_cmp(&a->_value, &b->_value);
}
