#ifndef COM_HASH_H
#define COM_HASH_H

#include "com_define.h"
#include "com_str.h"

typedef u64 (*com_hash)(u64,u64,const com_str);

/// Hashes str using FNV-1a (https://softwareengineering.stackexchange.com/questions/49550/which-hashing-algorithm-is-best-for-uniqueness-and-speed)
/// REQUIRES: `seed0` is a seed for the hash function
/// REQUIRES: `seed1` is a seed for the hash function
/// REQUIRES: `data` is a valid com_str
/// GUARANTEES: returns a u64 value based on a hash
u64 com_hash_fnv1a(u64 seed0, u64 seed1, const com_str data);

#endif 
