#ifndef COM_HASH_H
#define COM_HASH_H

#include "com_define.h"
#include "com_str.h"

// type of a function accepting 1 64bit seed and a com_str returning a 64bit hash 
typedef u64 (*com_hash_fn)(u64,const com_str);

/// Hashes str using FNV-1a (https://softwareengineering.stackexchange.com/questions/49550/which-hashing-algorithm-is-best-for-uniqueness-and-speed)
/// this hash is faster, but is vulnerable to DOS attack if the user can provide data to hash
/// REQUIRES: `seed` is a seed for the hash function
/// REQUIRES: `data` is a valid com_str
/// GUARANTEES: returns a u64 value based on a hash
u64 com_hash_fnv1a(u64 seed, const com_str data);

/// Hashes str using SipHash algorithm (https://en.wikipedia.org/wiki/SipHash)
/// this hash is slower, but secure against DOS attacks
/// REQUIRES: `seed` is a seed for the hash function
/// REQUIRES: `data` is a valid com_str
/// GUARANTEES: returns a u64 value based on a hash
u64 com_hash_sip(u64 seed0, u64 seed1, const com_str data);

#endif 
