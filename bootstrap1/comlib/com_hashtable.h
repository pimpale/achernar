#ifndef COM_HASHTABLE_H
#define COM_HASHTABLE_H

// somewhat fast robin hood open addressing hashtable
// hashmap owns keys but stores only a pointer to the value

#include "com_allocator.h"
#include "com_define.h"
#include "com_hash.h"
#include "com_str.h"
#include "com_vec.h"

typedef struct {
	bool valid;
	void* value_ptr;
} com_hashtable_Result;

// Do not manually modify
typedef struct {
  // allocator to use
  com_allocator *_allocator;
  // hasher fn
  com_hash_fn _hasher;
  // where the key value pairs are stored
  com_vec _pairs;
} com_hashtable;

typedef struct {
  // how large the hashtable starts off as
  // It may reallocate as necessary
  usize starting_capacity;
  // hasher function to use
  com_hash_fn hasher;
  // whether to randomly generate the seed for the hasher fn
  bool randomly_generate_seed;
  // seed for the hasher function
  // will only work if randomly_generate_seed is false
  u64 seed;
} com_hashtable_Settings;

#define com_hashtable_DEFAULT_SETTINGS                                         \
  ((com_hashtable_Settings){.starting_capacity = 16,                           \
                            .hasher = com_hash_sip,                            \
                            .randomly_generate_seed = true,                    \
                            .seed = 0})

// Creates a hashtable using memory allocated from `a` with default capacity
/// REQUIRES: `a` is a valid pointer to an allocator with REALLOCABLE supported
/// GUARANTEES: returns a valid com_hashtable
com_hashtable com_hashtable_create(com_allocator *a);

// Creates a hashtable using memory allocated from `a` with a settings
/// REQUIRES: `a` is a valid pointer to an allocator
/// REQUIRES: `settings` is a valid com_hashtable_settings
/// GUARANTEES: returns a valid com_hashtable adhering to the settings provided
com_hashtable com_hashtable_createSettings(com_allocator *a,
                                       com_hashtable_Settings settings);

// frees all memory associated with this hashtable
/// REQUIRES: `table` is a pointer to a valid com_hashtable
/// GUARANTEES: `table` is no longer valid
/// GUARANTEES: any pointers to keys in `table` are no longer valid
/// GUARANTEES: memory allocated from this table will be released
void com_hashtable_destroy(com_hashtable *table);

// Adds or inserts a new K V pair to the hashtable
/// REQUIRES: `table` is a valid pointer to a com_hashtable
/// REQUIRES: `key` is a valid com_str
/// REQUIRES: `value` is a pointer (does not have to be valid)
/// GUARANTEES: if a KV pair with `key` doesn't exist, a KV pair will be
/// created, and the value of `key` will be copied GUARANTEES: if not, the value
/// of `key` will not by copied 
/// GUARANTEES: if a KV pair with `key` doesn't exist, a new entry in the hash table will be created 
/// GUARANTEES: if not, the KV pair with key `key` will have its value erased
void com_hashtable_set(com_hashtable *hashtable, const com_str key, void *value);

// out of the KV Pair with key `key`, returns a the value if it exists
/// REQUIRES: `table` is a valid pointer to a com_hashtable
/// REQUIRES: `key` is a valid pointer to `keylen` bytes of memory
/// REQUIRES: `keylen` is greater than 0
/// REQUIRES: `value` is a valid pointer to `valuelen` bytes of memory
/// GUARANTEES: returns a pointer to the the value of the KV pair or a .valid=false
/// GUARANTEES: this pointer is valid until the next operation on `table`
com_hashtable_Result com_hashtable_get(const com_hashtable *table, const com_str key);

// deletes the KV pair with key `key` from `table`
/// REQUIRES: `table` is a valid pointer to a com_hashtable.
/// REQUIRES: `key` is a valid pointer to `keylen` bytes
/// REQUIRES: `keylen` is greater than 0
/// REQUIRES: a KV pair with key `key` exists
/// GUARANTEES: memory for the key and the value will be deallocated
/// GUARANTEES: there are no KV pairs with key `key`
void com_hashtable_remove(com_hashtable *table, const com_str key);

#endif
