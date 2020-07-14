#include "com_bigint.h"
#include "com_imath.h"
#include "com_assert.h"


com_bigint com_bigint_from(com_biguint magnitude, bool negative) {
  return (com_bigint) {
      ._negative = negative,
      ._magnitude = magnitude
  };
}

com_bigint com_bigint_create(com_allocator_Handle h) {
  return (com_bigint) {
      ._negative = false,
      ._magnitude = com_biguint_create(h)
  };
}

const com_biguint *com_bigint_magnitude(const com_bigint* a) {
  return &a->_magnitude;
}

void com_bigint_destroy(com_bigint *a) {
  com_biguint_destroy(&a->_magnitude);
}

i64 com_bigint_get_i64(const com_bigint *a) {
  u64 magnitude = com_biguint_get_u64(&a->_magnitude);
  if(magnitude == 0) {
      return 0;
  }

  if(a->_negative) {
      // we have to do this in order to avoid undefined behavior caused by casting a negative number to unsigned int
      // This number represents the magnitude -1
      i64 mag_dec = com_imath_u64_max(magnitude - 1, -(i64_min_m + 1));
      // return the negative true magnitude
      return -(mag_dec+1);
  } else {
      return com_imath_u64_max(magnitude, i64_max_m);
  }
}

bool com_bigint_fits_i64(const com_bigint* a) {

}
