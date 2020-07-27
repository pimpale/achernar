#include "com_hashtable.h"
#include "com_mem.h"
#include "com_assert.h"

// you can modify these
#define MIN_LOAD_FACTOR 0.1
#define OPT_LOAD_FACTOR 0.3
#define MAX_LOAD_FACTOR 0.6

typedef struct {
  // if the kv pair was never initialized
  bool initialized;

	com_str key;
  com_allocator_Handle key_handle;

	// pointer to the value
	void* value;

	// how many probes were necessary to create this
	usize probe_sequence;
} KVPair;


/* copies data to mapping pointer */
static KVPair kvpair_init(const com_str src, void* value, com_allocator* a) {
  com_allocator_Handle key_handle = com_allocator_alloc(a, (com_allocator_HandleData) {
      .flags=com_allocator_defaults(a),
      .len=src.len
  });

  u8* key_data = com_allocator_handle_get(key_handle);
	com_mem_move(key_data, src.data, src.len);

  com_str key = com_str_create(
  	key_data,
  	src.len
  );

	return (KVPair) {
    	.initialized=true,
    	.key=key,
    	.key_handle=key_handle,
    	.value=value,
    	.probe_count=0
	};
}

static void kvpair_destroy(KVPair *kvp) {
  com_assert_m(kvp->existent, "kvpair is invalid or nonexistent");
  com_allocator_dealloc(kvp->key_handle);
  kvp->existent = false;
}

// Initializes the memory pointed to as a table with given capacity
com_hashtable com_hashtable_createSettings(com_allocator *a, com_hashtable_Settings settings) {
  com_assert_m(com_allocator_supports(a) & com_allocator_REALLOCABLE, "allocator is unsuitable for this hashtable, must support reallocable");

  return (com_hashtable) {
  ._allocator = a,
  ._hasher=settings.hasher,
  ._pairs = com_vec_create(com_allocator_alloc(a, (com_allocator_HandleData) {
      .len=settings.starting_capacity,
      .flags=com_allocator_defaults(a) | com_allocator_REALLOCABLE
  })),
  };
}

// Creates table with default initial capacity
com_hashtable hashtable_create(com_allocator *a) { 
  return com_hashtable_createSettings(a, com_hashtable_DEFAULT_SETTINGS);
}


void com_hashtable_destroy(com_hashtable *hashtable) {
  // Free all mappings
  com_vec* pairs_vec = &hashtable->_pairs;
  KVPair* pairs = com_vec_get(pairs_vec, 0);
  usize pairs_len = com_vec_len_m(pairs_vec, KVPair);

  for (usize i = 0; i < pairs_len; i++) {
    if (pairs[i].existent) {
      kvpair_destroy(&pairs[i]);
    }
  }

  // Now free memory
  com_vec_destroy(pairs_vec);
}

/* Resizes table to the given number of spots, and then attempts to fit all
 * hashes inside */
// Literally clones the whole hashmap, super expensive operation
// TODO: make this faster
static void hashtable_resize(HashTable *hashtable, size_t capacity) {
  // check that new size is not too small
  assert((double)hashtable->pair_count / (double)capacity < MAX_LOAD_FACTOR);

  HashTable new =  hashtable_createWithCapacity(hashtable->a, capacity);
  for (size_t i = 0; i < hashtable->pairs_capacity; i++) {
    KVPair htkvp = hashtable->pairs[i];
    if (htkvp.existent) {
      // clone mapping
      void* value = hashtable_set(&new, htkvp.key, htkvp.keylen, htkvp.valuelen);
      memcpy(value, htkvp.value, htkvp.valuelen);
    }
  }
  // free the old table
  hashtable_destroy(hashtable);
  // overwrite with new table
  *hashtable = new;
}

static KVPair* hashtable_getKVPair(const HashTable *table, void *key, size_t keylen) {
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
      // This is probably because we have a bad hashing function or the table is too small
      assert(attempt < table->pairs_capacity);
    }
  }
}

void *hashtable_set(HashTable *hashtable, void *key, size_t keylen, size_t valuelen) {
  // if potentially adding this element would cause the load to increase too much, then we must resize
  if((double)(hashtable->pair_count+1) / (double)hashtable->pairs_capacity > MAX_LOAD_FACTOR) {
    // resize so that the table is at the OPT_LOAD_FACTOR
    hashtable_resize(hashtable, (size_t)(((double)hashtable->pair_count+1)/OPT_LOAD_FACTOR));
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

  if((double)(hashtable->pair_count)/(double)(hashtable->pairs_capacity) < MIN_LOAD_FACTOR) {
    // resize so that the table is at the OPT_LOAD_FACTOR
    hashtable_resize(hashtable, (size_t)(((double)hashtable->pair_count+1)/OPT_LOAD_FACTOR));
  }
}

HashTableValueLength hashtable_valueLength(const HashTable *hashtable, void *key, size_t keylen) {
  KVPair *kvp = hashtable_getKVPair(hashtable, key, keylen);
  // If m exists, return the valuelen, otherwise return 0
  return (HashTableValueLength) {
      .exists=kvp->existent,
      .length=kvp->valuelen
  };
}

void* hashtable_get(const HashTable *hashtable, void *key, size_t keylen) {
  KVPair *kvp = hashtable_getKVPair(hashtable, key, keylen);
  assert(kvp->existent);
  return kvp->value;
}
