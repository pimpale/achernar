#include "com_hashtable.h"
#include "com_assert.h"
#include "com_mem.h"
#include "com_os_time.h"

// you can modify these
#define MIN_LOAD_FACTOR 0.1
#define OPT_LOAD_FACTOR 0.3
#define MAX_LOAD_FACTOR 0.6


// copies src to a version owned by allocator a
// REQUIRES: `src` is a valid com_str
// REQUIRES: `ht` is a valid pointer to a valid hashtable
static inline Key create_owned_key(const com_str src, com_hashtable *ht) {
  // allocate memory
  com_allocator_Handle key_handle = com_allocator_alloc(
      ht->_allocator, (com_allocator_HandleData){.flags = com_allocator_defaults(ht->_allocator),
                                    .len = src.len});

  com_assert_m(key_handle.valid, "allocation failure");

	// clone key data
  u8 *key_data = com_allocator_handle_get(key_handle);
  com_mem_move(key_data, src.data, src.len);

  com_str key = com_str_create(key_data, src.len);

	return (Key) {
		.key=key,
		.hash=ht->_hasher(ht->_seed, 0, src),
		.handle=key_handle
	};
}

// Initializes the memory pointed to as a table with given capacity
com_hashtable com_hashtable_createSettings(com_allocator *a,
                                           com_hashtable_Settings settings) {
  com_assert_m(com_allocator_supports(a) & com_allocator_REALLOCABLE |
                   com_allocator_ZERO,
               "allocator is unsuitable for this hashtable, must support "
               "reallocable and zeroable memory");

  u64 seed = settings.randomly_generate_seed
                 ? com_os_time_monotonic() + com_os_time_unix()
                 : settings.seed;

  return (com_hashtable){
      ._allocator = a,
      ._hasher = settings.hasher,
      ._buckets = com_vec_create(com_allocator_alloc(
          a,
          (com_allocator_HandleData){
              .len = settings.starting_capacity * sizeof(Bucket),
              .flags = com_allocator_defaults(a) | com_allocator_REALLOCABLE |
                       com_allocator_ZERO})),
      ._seed = seed,
  };
}

// Creates table with default initial capacity
com_hashtable hashtable_create(com_allocator *a) {
  return com_hashtable_createSettings(a, com_hashtable_DEFAULT_SETTINGS);
}

void com_hashtable_destroy(com_hashtable *hashtable) {
  // Free all mappings
  com_vec *buckets_vec = &hashtable->_buckets;
  Bucket *buckets = com_vec_get(buckets_vec, 0);
  usize buckets_len = com_vec_len_m(buckets_vec, Bucket);

  for (usize i = 0; i < buckets_len; i++) {
    if (buckets[i].initialized) {
  		com_allocator_dealloc(buckets[i].value.key.handle);
    }
  }

  // Now free memory
  com_vec_destroy(buckets_vec);
}

	key.hash == hcache && com_str_equal(key.key, str);


/// inserts Key `key` into the `buckets` vector following robin hood hashing rules
static void internal_hashtable_insert(com_vec *buckets_vec, Key key, void* value) {
  Bucket * buckets = com_vec_get(buckets_vec, 0);
  usize buckets_len = com_vec_len_m(buckets_vec, Bucket);
	while(true) {
		if(buckets[key % 
	}
}

void com_hashtable_set(com_hashtable *hashtable, const com_str key,
                       void *value);

/*

// Resizes table to the given number of spots, and then attempts to fit all
//  hashes inside
static void internal_hashtable_resize(com_hashtable *table, usize new_capacity){
  // check that new size is not too small
  assert((double)hashtable->pair_count / (double)capacity < MAX_LOAD_FACTOR);

  HashTable new =  hashtable_createWithCapacity(hashtable->a, capacity);
  for (size_t i = 0; i < hashtable->pairs_capacity; i++) {
    KVPair htkvp = hashtable->pairs[i];
    if (htkvp.existent) {
      // clone mapping
      void* value = hashtable_set(&new, htkvp.key, htkvp.keylen,htkvp.valuelen);memcpy(value, htkvp.value, htkvp.valuelen);
    }
  }
  // free the old table
  hashtable_destroy(hashtable);
  // overwrite with new table
  *hashtable = new;
}
*/

static KVPair *hashtable_getKVPair(const HashTable *table, void *key,
                                   size_t keylen) {
  uint64_t attempt = 0;
  // Double hashing algorithm
  while (true) {
    // Calculate index and retrieve mapping
    size_t index = simpleHash(attempt, key, keylen) % table->pairs_capacity;
    KVPair *kvp = &table->pairs[index];
    if (!kvp->existent) {
      // If the mapping does not exist yet
      return kvp;
    } else if (keylen == kvp->keylen && memcmp(kvp->key, key, keylen) == 0) {
      // If the mapping exists and keys match
      return kvp;
    } else if (attempt < table->pairs_capacity) {
      // If there is another attempt availaible
      attempt++;
    } else {
      // Once we've iterated through all possibilities
      // This is probably because we have a bad hashing function or the table is
      // too small
      assert(attempt < table->pairs_capacity);
    }
  }
}

void *hashtable_set(HashTable *hashtable, void *key, size_t keylen,
                    size_t valuelen) {
  // if potentially adding this element would cause the load to increase too
  // much, then we must resize
  if ((double)(hashtable->pair_count + 1) / (double)hashtable->pairs_capacity >
      MAX_LOAD_FACTOR) {
    // resize so that the table is at the OPT_LOAD_FACTOR
    hashtable_resize(hashtable, (size_t)(((double)hashtable->pair_count + 1) /
                                         OPT_LOAD_FACTOR));
  }

  KVPair *kvp = hashtable_getKVPair(hashtable, key, keylen);

  // If kvp exists, just update it
  if (kvp->existent) {
    // if the value length is different we must reallocate
    if (kvp->valuelen != valuelen) {
      a_dealloc(hashtable->a, kvp->key_allocId);
      kvp->key_allocId = a_alloc(hashtable->a, valuelen);
      kvp->key = a_get(hashtable->a, kvp->key_allocId);
      kvp->valuelen = valuelen;
    }
    return kvp->value;
  } else {
    // update the count
    hashtable->pair_count++;
    // Create a new key value pair
    kvp_init(kvp, key, keylen, valuelen, hashtable->a);
    return kvp->value;
  }
}

void hashtable_remove(HashTable *hashtable, void *key, size_t keylen) {
  KVPair *kvp = hashtable_getKVPair(hashtable, key, keylen);
  kvp_destroy(kvp, hashtable->a);

  if ((double)(hashtable->pair_count) / (double)(hashtable->pairs_capacity) <
      MIN_LOAD_FACTOR) {
    // resize so that the table is at the OPT_LOAD_FACTOR
    hashtable_resize(hashtable, (size_t)(((double)hashtable->pair_count + 1) /
                                         OPT_LOAD_FACTOR));
  }
}

HashTableValueLength hashtable_valueLength(const HashTable *hashtable,
                                           void *key, size_t keylen) {
  KVPair *kvp = hashtable_getKVPair(hashtable, key, keylen);
  // If m exists, return the valuelen, otherwise return 0
  return (HashTableValueLength){.exists = kvp->existent,
                                .length = kvp->valuelen};
}

void *hashtable_get(const HashTable *hashtable, void *key, size_t keylen) {
  KVPair *kvp = hashtable_getKVPair(hashtable, key, keylen);
  assert(kvp->existent);
  return kvp->value;
}
