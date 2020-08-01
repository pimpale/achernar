#include "com_imath.h"

#include "com_imath.h"

#define GEN_TYPE_DEFS(i_type_m)                                                \
  i_type_m com_imath_##i_type_m##_min(i_type_m a, i_type_m b) {                \
    if (a < b) {                                                               \
      return a;                                                                \
    } else {                                                                   \
      return b;                                                                \
    }                                                                          \
  }                                                                            \
  i_type_m com_imath_##i_type_m##_max(i_type_m a, i_type_m b) {                \
    if (a > b) {                                                               \
      return a;                                                                \
    } else {                                                                   \
      return b;                                                                \
    }                                                                          \
  }                                                                            \
  i_type_m com_imath_##i_type_m##_clamp(i_type_m a, i_type_m min,              \
                                        i_type_m max) {                        \
    if (a > max) {                                                             \
      return max;                                                              \
    } else if (a < min) {                                                      \
      return min;                                                              \
    } else {                                                                   \
      return a;                                                                \
    }                                                                          \
  }

#define IGEN_TYPE_DEFS(i_type_m)                                               \
  i_type_m com_imath_##i_type_m##_abs(i_type_m a) {                            \
    if (a < 0) {                                                               \
      return -a;                                                               \
    } else {                                                                   \
      return a;                                                                \
    }                                                                          \
  }                                                                            \
  GEN_TYPE_DEFS(i_type_m)

IGEN_TYPE_DEFS(i8)
IGEN_TYPE_DEFS(i16)
IGEN_TYPE_DEFS(i32)
IGEN_TYPE_DEFS(i64)
IGEN_TYPE_DEFS(isize)

GEN_TYPE_DEFS(u8)
GEN_TYPE_DEFS(u16)
GEN_TYPE_DEFS(u32)
GEN_TYPE_DEFS(u64)
GEN_TYPE_DEFS(usize)
