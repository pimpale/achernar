#ifndef COM_HASHTABLE_H
#define COM_HASHTABLE_H

#include "com_define.h"
#include "com_allocator.h"

typedef struct {
  // if this value exists
  bool exists;
  // what the length of this value is
  // (is only defined if exists)
  size_t length;
} HashTableValueLength;

typedef struct {
  void *key;
  size_t keylen;
  AllocId key_allocId;

  void *value;
  size_t valuelen;
  AllocId value_allocId;

  bool existent;
} HashTableKVPair;

// Do not manually modify
typedef struct {
  Allocator *a;

  // The KV Pairs in the table
  HashTableKVPair *pairs;
  AllocId pairs_allocId;

  // Number of existent KV Pairs in `pairs`
  size_t pair_count;

  // length of `pairs`
  size_t pairs_capacity;
} HashTable;

// Creates a hashtable using memory allocated from `a` with default capacity
/// REQUIRES: `a` is a valid pointer to an Allocator
/// GUARANTEES: returns a valid HashTable
HashTable hashtable_create(Allocator *a);

// Creates a hashtable using memory allocated from `a` with a capacity of
// `capacity`
/// REQUIRES: `a` is a valid pointer to an Allocator
/// GUARANTEES: returns a valid HashTable with capacity `capacity`
HashTable hashtable_createWithCapacity(Allocator *a, size_t capacity);

// frees all memory associated with this hashtable
/// REQUIRES: `table` is a pointer to a valid HashTable
/// GUARANTEES: `table` is no longer valid
/// GUARANTEES: any pointers to elements in `table` are no longer valid
/// GUARANTEES: memory allocated from this table will be released
void hashtable_destroy(HashTable *table);


// Adds or inserts a new K V pair to the hashtable
/// REQUIRES: `hashtable` is a valid pointer to a HashTable
/// REQUIRES: `key` is a valid pointer to `keylen` bytes
/// REQUIRES: `keylen` is greater than 0
/// REQUIRES: `valuelen` is greater than 0
/// GUARANTEES: returns a pointer to `valuelen` bytes of memory
/// GUARANTEES: if a KV pair with `key` doesn't exist, a KV pair will be created, and the value of `key` will be copied
/// GUARANTEES: if not, the value of `key` will not by copied
/// GUARANTEES: if a KV pair with `key` doesn't exist, a new entry in the hash table will be created
/// GUARANTEES: if not, the KV pair with key `key` will have its value erased
void *hashtable_set(HashTable *hashtable, void *key, size_t keylen,
                    size_t valuelen);

// Returns if the a KV pair with key `key` exists and if so, the length of its value
/// REQUIRES: `hashtable` is a valid pointer to a HashTable.
/// REQUIRES: `key` is a valid pointer to keylen bytes of memory
/// GUARANTEES: if a KV pair with key `key` does not exist, the return value.exists is false
/// GUARANTEES: if a KV pair with key `key` does exist, the return value.exists is true
/// GUARANTEES: if a KV pair with key `key` does exist, the return value.length is the length of the pair's value
HashTableValueLength hashtable_valueLength(const HashTable *hashtable, void *key, size_t keylen);

// out of the KV Pair with key `key, copies the first `valuelen` bytes of the pair's value
/// REQUIRES: `hashtable` is a valid pointer to a HashTable
/// REQUIRES: `key` is a valid pointer to `keylen` bytes of memory
/// REQUIRES: `keylen` is greater than 0
/// REQUIRES: `value` is a valid pointer to `valuelen` bytes of memory
/// REQUIRES: a KV pair must be defined for the value of `key`
/// GUARANTEES: returns a pointer to the the value of the KV pair
/// GUARANTEES: this pointer is valid until the next operation on `hashtable`
void* hashtable_get(const HashTable *hashtable, void *key, size_t keylen);

// deletes the KV pair with key `key` from `hashtable`
/// REQUIRES: `hashtable` is a valid pointer to a HashTable.
/// REQUIRES: `key` is a valid pointer to `keylen` bytes
/// REQUIRES: `keylen` is greater than 0
/// REQUIRES: a KV pair with key `key` exists
/// GUARANTEES: memory for the key and the value will be deallocated
/// GUARANTEES: there are no KV pairs with key `key`
void hashtable_remove(HashTable *hashtable, void *key, size_t keylen);

#endif
