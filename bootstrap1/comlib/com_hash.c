#include "com_hash.h"

u64 com_hash_fnv1a(u64 seed0, u64 seed1, const com_str data) {
  u64 hash = seed0 + seed1;

  // avoid degenerate case when seed == 0
  if (hash == 0) {
    hash = 1;
  }

  for (usize i = 0; i < data.len; i++) {
    hash ^= data.data[i];
    hash *= 0x100000001b3;
  }
  return hash;
}
