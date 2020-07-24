#include "com_hash.h"

u64 com_hash_fnv1a(u64 seed, const com_str data) {
  u64 hash = seed;
  for (usize i = 0; i < data.len; i++) {
    hash ^= data.data[i];
    hash *= 0x100000001b3;
  }
  return hash;
}
