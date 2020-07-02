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

// TODO do ifdef cancer
#include <stddef.h>
typedef size_t usize;
typedef ptrdiff_t isize;

// TODO do ifdef cancer
typedef double f64;
typedef float f32;

#define attr_UNUSED __attribute__((unused))
#define attr_NORETURN __attribute__((noreturn))

#endif
