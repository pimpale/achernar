#ifndef COM_IMATH_H
#define COM_IMATH_H

// fixed size integer math only

#include "com_define.h"


#define DEFINE_TYPE(i_type_m)  \
  i_type_m com_math_##i_type_m##min(i_type_m a, i_type_m b); \
  i_type_m com_math_##i_type_m##max(i_type_m a, i_type_m b); \
  i_type_m com_math_##i_type_m##clamp(i_type_m a, i_type_m min, i_type_m max); \

#define IDEFINE_TYPE(i_type_m)  \
  i_type_m com_math_##i_type_m##abs(i_type_m a); \
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


#endif

