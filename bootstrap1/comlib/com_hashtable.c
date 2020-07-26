#include "com_hashtable.h"
#include "com_assert.h"

#define MIN_LOAD_FACTOR 0.01
#define OPT_LOAD_FACTOR 0.05
#define MAX_LOAD_FACTOR 0.2

#define DEFAULT_INITIAL_CAPACITY 1

typedef struct {
  // if the kv pair exists at all
  bool existent;

	com_str key;
  com_allocator_Handle key_handle;

	void* value;

} KVPair;


/* copies data to mapping pointer */
static KVPair kvpair_init(const com_str src, void* value, com_allocator* a) {
  com_allocator_Handle key_handle = com_allocator_alloc(a, (com_allocator_HandleData)
	KVPair p = {
    	.existent=true;
  kvp->existent = true;

  kvp->keylen = keylen;
  kvp->key_allocId = a_alloc(a, keylen);
  kvp->key = a_get(a, kvp->key_allocId);
  memcpy(kvp->key, key, keylen);

}

static void kvp_destroy(KVPair *kvp, Allocator *a) {
  com_assert_m(kvp->existent, );
  a_dealloc(a, kvp->key_allocId);
  a_dealloc(a, kvp->value_allocId);
  kvp->existent = false;
}

// Creates table with default initial capacity
HashTable hashtable_create(Allocator *a) { 
  return hashtable_createWithCapacity(a, INITIAL_CAPACITY); 
}

// Initializes the memory pointed to as a table with given capacity
HashTable hashtable_createWithCapacity(Allocator *a, size_t capacity) {
  HashTable table;
  table.a = a;
  table.pair_count= 0;
  table.pairs_capacity = capacity;
  // Initialize array to zero
  size_t arr_size = capacity * sizeof(KVPair);
  table.pairs_allocId = a_alloc(a, arr_size);
  table.pairs = a_get(a, table.pairs_allocId);
  memset(table.pairs, 0, arr_size);
  return table;
}

void hashtable_destroy(HashTable *hashtable) {
  // Free all mappings
  for (size_t i = 0; i < hashtable->pairs_capacity; i++) {
    if (hashtable->pairs[i].existent) {
      kvp_destroy(&hashtable->pairs[i], hashtable->a);
    }
  }
  // Now free memory
  a_dealloc(hashtable->a, hashtable->pairs_allocId);
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
