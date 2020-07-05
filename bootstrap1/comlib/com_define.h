#ifndef COM_DEFINE_H
#define COM_DEFINE_H

// TODO do ifdef cancer
#include <stdbool.h>

// TODO do ifdef cancer
#include <stdint.h>
typedef int64_t i64;
typedef uint64_t u64;
typedef int32_t i32;
typedef uint32_t u32;
typedef int16_t i16;
typedef uint16_t u16;
typedef int8_t i8;
typedef uint8_t u8;

#define i64_max_m INT64_MAX
#define i64_min_m INT64_MIN
#define u64_max_m UINT64_MAX
#define i32_max_m INT32_MAX
#define i32_min_m INT32_MIN
#define u32_max_m UINT32_MAX
#define i16_max_m INT16_MAX
#define i16_min_m INT16_MIN
#define u16_max_m UINT16_MAX
#define i8_max_m INT8_MAX
#define i8_min_m INT8_MIN
#define u8_max_m UINT8_MAX

// TODO do ifdef cancer
#include <stddef.h>
typedef size_t usize;
#define usize_max_m SIZE_MAX
typedef ptrdiff_t isize;
#define isize_max_m PTRDIFF_MAX
#define isize_min_m PTRDIFF_MIN


// TODO do ifdef cancer
typedef double f64;
typedef float f32;

#define attr_UNUSED __attribute__((unused))
#define attr_NORETURN __attribute__((noreturn))

#endif
