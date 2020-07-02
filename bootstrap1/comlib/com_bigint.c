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
  u64 normalized;
  if(val < 0) {
      dest->_negative = true;
      normalized = -val;
  } else {
      dest->_negative = false;
      normalized = val;
  }



}

i64 com_bigint_release(com_bigint* a) {

}
