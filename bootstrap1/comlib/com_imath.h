#ifndef COM_IMATH_H
#define COM_IMATH_H

// fixed size integer math only

#include "com_define.h"


#define DEFINE_TYPE(i_type_m)  \
  i_type_m com_imath_##i_type_m##_min(i_type_m a, i_type_m b); \
  i_type_m com_imath_##i_type_m##_max(i_type_m a, i_type_m b); \
  i_type_m com_imath_##i_type_m##_clamp(i_type_m a, i_type_m min, i_type_m max); \

#define IDEFINE_TYPE(i_type_m)  \
  i_type_m com_imath_##i_type_m##_abs(i_type_m a); \
  DEFINE_TYPE(i_type_m)


IDEFINE_TYPE(i8)
IDEFINE_TYPE(i16)
IDEFINE_TYPE(i32)
IDEFINE_TYPE(i64)
IDEFINE_TYPE(isize)

DEFINE_TYPE(u8)
DEFINE_TYPE(u16)
DEFINE_TYPE(u32)
DEFINE_TYPE(u64)
DEFINE_TYPE(usize)

#undef DEFINE_TYPE
#undef IDEFINE_TYPE

#define MAX_UNSIGNED_VAL(type) x


#define com_imath_i8_minval_m (-0x80)
#define com_imath_i8_maxval_m ( 0x7F)

#define com_imath_i16_minval_m (-0x8000)
#define com_imath_i16_maxval_m ( 0x7FFF)

#define com_imath_i32_minval_m (-0x80000000)
#define com_imath_i32_maxval_m ( 0x7FFFFFFF)

#define com_imath_i64_minval_m (-0x8000000000000000)
#define com_imath_i64_maxval_m ( 0x7FFFFFFFFFFFFFFF)

#define com_imath_u8_maxval_m (0xFF)
#define com_imath_u8_msb_m (0x80)

#define com_imath_u16_maxval_m (0xFFFF)
#define com_imath_u16_msb_m (0x8000)

#define com_imath_u32_maxval_m (0xFFFFFFFF)
#define com_imath_u32_msb_m (0x80000000)

#define com_imath_u64_maxval_m (0xFFFFFFFFFFFFFFFF)
#define com_imath_u64_msb_m (0x8000000000000000)

#define com_imath_u64_maxval_m (0xFFFFFFFFFFFFFFFF)
#define com_imath_u64_msb_m (0x8000000000000000)

#endif

