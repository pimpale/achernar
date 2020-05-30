#ifndef UTILS_H
#define UTILS_H

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

// returns `value` rounded up to the nearest multiple of `multiple`
// REQUIRES: multiple is a power of two
// GUARANTEES: return value is a multiple of `roundTo` and >= value
static inline size_t roundToMultipleofTwo(size_t value, size_t roundTo) {
  return (value + (roundTo - 1)) & ~(roundTo - 1);
}


#define UNUSED(x) (void)(x)
#define ZERO(ptr) (memset(ptr, 0, sizeof(*ptr)))
#define PANIC() abort()
#define INTERNAL_ERROR(msg) logInternalError(__LINE__, __func__, msg)
void logInternalError(char* appname, uint32_t line, char *func, const char *fmt, ...);

#endif