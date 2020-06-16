#include "utils.h"

// returns `value` rounded up to the nearest multiple of `multiple`
// REQUIRES: multiple is a power of two
// GUARANTEES: return value is a multiple of `roundTo` and >= value
size_t roundToMultipleofTwo(size_t value, size_t roundTo) {
  return (value + (roundTo - 1)) & ~(roundTo - 1);
}

// defined behavior for all values of val
uint64_t safe_abs(int64_t val) {
  if(val < 0) {
    return (uint64_t) -val;
  } else {
    return (uint64_t) val;
  }
}

