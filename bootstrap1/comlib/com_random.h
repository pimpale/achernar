#ifndef COM_RANDOM_H
#define COM_RANDOM_H

// pseudorandom number generators

#include "com_define.h"

/// type of a function returning a random u64 evenly distributed in the range [0, u64_max_m]
typedef u64 (*com_random_fn)();

/// type of a function returning a random u64 evenly distributed in the range [0, u64_max_m]
/// and 1 u64 seed
typedef u64 (*com_random_seed_fn)(u64);

#endif

