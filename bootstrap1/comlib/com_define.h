#ifndef COM_DEFINE_H
#define COM_DEFINE_H

#define NULL ((void*)0)

// TODO 
// what to do if your compiler isn't gcc/clang

typedef __INT64_TYPE__ i64;
typedef __UINT64_TYPE__ u64;

typedef __INT32_TYPE__ i32;
typedef __UINT32_TYPE__ u32;

typedef __INT16_TYPE__ i16;
typedef __UINT16_TYPE__ u16;

typedef __INT8_TYPE__ i8;
typedef __UINT8_TYPE__ u8;

#define i64_max_m __INT64_MAX__
#define i32_max_m __INT32_MAX__
#define i16_max_m __INT64_MAX__
#define i8_max_m __INT8_MAX__

#define i64_min_m (-__INT64_MAX__-1)
#define i32_min_m (-__INT32_MAX__-1)
#define i16_min_m (-__INT16_MAX__-1)
#define i8_min_m (-__INT8_MAX__-1)

#define u64_max_m __UINT64_MAX__
#define u32_max_m __UINT32_MAX__
#define u16_max_m __UINT16_MAX__
#define u8_max_m __UINT8_MAX__

typedef __UINTPTR_TYPE__ usize;
#define usize_max_m __UINTPTR_MAX__

typedef __INTPTR_TYPE__ isize;
#define isize_max_m __INTPTR_MAX__
#define isize_min_m (-__INTPTR_MAX__-1)

typedef double f64;
typedef float f32;

// if no c++ then we define bool
#ifndef __cplusplus
typedef _Bool bool;
#define true 1
#define false 0
#endif

#define attr_UNUSED __attribute__((unused))
#define attr_NORETURN __attribute__((noreturn))

#endif
