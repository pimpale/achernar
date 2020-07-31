#include "com_hashtable.h"
#include "com_assert.h"
#include "com_imath.h"
#include "com_mem.h"
#include "com_os_time.h"

// represent how the hash table will scale up under load
#define MIN_LOAD_FACTOR 0.1
#define OPT_LOAD_FACTOR 0.2
#define MAX_LOAD_FACTOR 0.6

// Initializes the memory pointed to as a table with given capacity
com_hashtable com_hashtable_createSettings(com_allocator_Handle handle,
                                           com_hashtable_Settings settings) {

  com_assert_m(
      !(settings.fixed_size &&
        com_allocator_handle_query(handle).flags & com_allocator_REALLOCABLE),
      "if the hashmap has variable size, then the handle provided "
      "must support reallocation");

  const usize capacity =
      com_allocator_handle_query(handle).len / sizeof(com_hashtable_Bucket);

  return (com_hashtable){
      ._hasher = settings.hasher,
      ._seed = settings.randomly_generate_seed
                   ? com_os_time_monotonic() + com_os_time_unix()
                   : settings.seed,
      ._buckets = com_allocator_handle_get(handle),
      ._buckets_capacity = capacity,
      ._buckets_handle = handle,
      ._buckets_used = 0,
      // an empty hashmap cannot yet be shrunk
      ._buckets_shrink_threshold = 0,
      // the load factor = used/capacity
      ._buckets_expand_threshold = (usize)(MAX_LOAD_FACTOR * (double)capacity),
      // any color will do
      ._resize_color = com_hashtable_RED,
      ._fixed = settings.fixed_size};
}

// Creates table with default initial capacity
com_hashtable com_hashtable_create(com_allocator_Handle handle) {
  return com_hashtable_createSettings(handle, com_hashtable_DEFAULT_SETTINGS);
}

void com_hashtable_destroy(com_hashtable *ht) {
  // free memory
  com_allocator_dealloc(ht->_buckets_handle);
}

static bool internal_key_eq(const com_hashtable_Key a,
                            const com_hashtable_Key b) {
  return a._hash_cache == b._hash_cache && com_str_equal(a._key, b._key);
}

static com_hashtable_ResizeColor invert_color(com_hashtable_ResizeColor c) {
  switch (c) {
  case com_hashtable_RED:
    return com_hashtable_BLUE;
  case com_hashtable_BLUE:
    return com_hashtable_RED;
  }
}

/// inserts Key `key` into the `buckets` vector following robin hood hashing
/// rules
/// REQUIRES: `buckets` is a valid pointer to at least `bckts_len`
/// `com_hashtable_Bucket`s
/// REQUIRES: `key` is a valid `com_hashtable_Key`
/// REQUIRES: `buckets_used_ptr` is a valid pointer to the number of buckets in
/// use before adding this one
/// REQUIRES: `resize_color` is the current color of the hashmap;
/// GUARANTEES: returns true if a new pair was created, and false if otherwise
static bool internal_hashtable_set(com_hashtable_Bucket *buckets,
                                   usize bckts_len, const com_hashtable_Key key,
                                   void *value,
                                   com_hashtable_ResizeColor resize_color) {
  // this is the current key to insert
  com_hashtable_KeyValuePair current_pair = (com_hashtable_KeyValuePair){
      ._key = key,
      ._value = value,
      ._probe_count = 0,
  };

  while (true) {
    // we use linear probing
    com_hashtable_Bucket *b =
        &buckets[(key._hash_cache + current_pair._probe_count) % bckts_len];

    if (b->_initialized) {
      if (internal_key_eq(current_pair._key, b->_pair._key)) {
        // this means that the keys match and we can just set the value
        b->_pair._value = value;
        b->_resize_color = resize_color;

        return false;
      } else if (b->_pair._probe_count < current_pair._probe_count ||
                 b->_resize_color != resize_color) {
        // this means that we can do the robin hood swap
        // we put the key we're currently searching for in this spot
        // and then start looking for a place to put the old key that used to
        // occupy this slot

        // do the swap
        com_hashtable_KeyValuePair tmp = current_pair;
        current_pair = b->_pair;
        b->_pair = tmp;
        b->_resize_color = resize_color;
      } else {
        // this means that we keep looking
        current_pair._probe_count++;

        // assertion to see if we're obviously stuck
        com_assert_m(current_pair._probe_count < bckts_len,
                     "all buckets in hashmap are full");
      }
    } else {
      // if this bucket was not initialized, it means we found an unoccupied
      // slot and we can insert the key here
      b->_initialized = true;
      b->_resize_color = resize_color;
      b->_pair = current_pair;
      return true;
    }
  }
}

static void internal_hashtable_resize(com_hashtable *ht, usize new_capacity) {
  // check that new size is not too small
  com_assert_m(ht->_buckets_used < new_capacity,
               "new capacity is too small to fit all entries");

  com_allocator_Handle handle = ht->_buckets_handle;
  com_hashtable_Bucket *buckets = ht->_buckets;
  const usize old_capacity = ht->_buckets_capacity;
  const usize old_used = ht->_buckets_used;

  // if we're expanding the buffer then we have to grow it first
  if (new_capacity > old_capacity) {
    // realloc if we're expanding the array
    com_allocator_Handle ret = com_allocator_realloc(handle, new_capacity);
    com_assert_m(ret.valid, "reallocation failed");
    handle = ret;
    buckets = com_allocator_handle_get(ret);
  }

  const com_hashtable_ResizeColor old_color = ht->_resize_color;
  const com_hashtable_ResizeColor new_color = invert_color(old_color);

  for (usize i = 0; i < old_capacity; i++) {
    com_hashtable_Bucket *b = &buckets[i];
    if (b->_initialized && b->_resize_color == old_color) {
      b->_initialized = false;
      internal_hashtable_set(
          buckets,         // buckets array
          new_capacity,    // we use new capacity so it can distribute evenly
          b->_pair._key,   // clone key
          b->_pair._value, // clone value
          new_color // we give it the new color so that it knows to displace old
                    // colors that it may encounter
      );
    }
  }

  // if we're shrinking the buffer we can shrink it now that we've safely moved
  // everything within the boundary
  if (new_capacity < old_capacity) {
    // realloc if we're shrinking the array
    com_allocator_Handle ret = com_allocator_realloc(handle, new_capacity);
    com_assert_m(ret.valid, "reallocation failed");
    handle = ret;
    buckets = com_allocator_handle_get(ret);
  }

  // update all changed fields
  ht->_buckets_handle = handle;
  ht->_buckets = buckets;
  ht->_buckets_capacity = new_capacity;
  ht->_buckets_used = old_used;
  ht->_buckets_expand_threshold = (usize)(MAX_LOAD_FACTOR * (double)new_capacity);
  ht->_buckets_shrink_threshold = (usize)(MIN_LOAD_FACTOR * (double)new_capacity);
  ht->_resize_color = new_color;
}

void com_hashtable_set(com_hashtable *ht, const com_str key, void *value) {

  // if ht is fixed and we would have to insert another key and we are out of
  // space throw an error
  com_assert_m(!(ht->_fixed && ht->_buckets_used == ht->_buckets_capacity &&
                 !com_hashtable_get(ht, key).valid),
               "hashmap is out of memory");

  // if we aren't fixed, check if we need to resize and do so
  if (!ht->_fixed) {
    if (ht->_buckets_used + 1 > ht->_buckets_expand_threshold) {
      internal_hashtable_resize(ht, (usize)((double)(ht->_buckets_used + 1) / OPT_LOAD_FACTOR));
    }
  }

  com_hashtable_Key keyobj = (com_hashtable_Key){
      ._key = key, ._hash_cache = ht->_hasher(ht->_seed, key)};

  bool newcreated = internal_hashtable_set(ht->_buckets, ht->_buckets_capacity,
                                           keyobj, value, ht->_resize_color);

  if (newcreated) {
    ht->_buckets_used++;
  }
}

com_hashtable_Result com_hashtable_get(const com_hashtable *ht,
                                       const com_str key) {
  const com_hashtable_Key keyobj = (com_hashtable_Key){
      ._key = key, ._hash_cache = ht->_hasher(ht->_seed, key)};

  usize probe_count = 0;
  while (true) {
    // we use linear probing
    usize i = keyobj._hash_cache + probe_count;
    com_hashtable_Bucket *b = &ht->_buckets[i % ht->_buckets_capacity];

    // if we encounter an uninitialized value we can end our search since
    // the linear probing pattern we use always places the key-value pair in the
    // first uninitialized bucket
    if (!b->_initialized) {
      return (com_hashtable_Result){.valid = false};
    } else {
      if (internal_key_eq(keyobj, b->_pair._key)) {
        return (com_hashtable_Result){.valid = true, .value = b->_pair._value};
      } else {
        probe_count++;
        // if we've checked all spaces (possible only on a fully filled hashmap)
        if (probe_count < ht->_buckets_capacity) {
          return (com_hashtable_Result){.valid = false};
        }
      }
    }
  }
}

static com_hashtable_Result
internal_hashtable_remove(com_hashtable_Bucket *buckets, usize buckets_capacity,
                          const com_hashtable_Key key) {
  usize probe_count = 0;
  while (true) {
    // we use linear probing
    usize i = key._hash_cache + probe_count;
    com_hashtable_Bucket *b = &buckets[i % buckets_capacity];

    // if we encounter an uninitialized value we can end our search since
    // the linear probing pattern we use always places the key-value pair in the
    // first uninitialized bucket
    if (!b->_initialized) {
      return (com_hashtable_Result){.valid = false};
    } else {
      if (internal_key_eq(key, b->_pair._key)) {
        // save the value
        com_hashtable_Result ret =
            (com_hashtable_Result){.valid = true, .value = b->_pair._value};

        // deinitialize this bucket
        b->_initialized = false;

        while (true) {
          // get the previous and current bucket
          com_hashtable_Bucket *prev = &buckets[i % buckets_capacity];
          com_hashtable_Bucket *current = &buckets[(i + 1) % buckets_capacity];
          if (!current->_initialized || current->_pair._probe_count == 0) {
            // if the next bucket isn't initialized  or has a zero probe cout
            // then we've hit the end of the probing sequence and  can exit
            break;
          } else {
            // otherwise it's part of the probing sequence and we can move it
            // back one slot while decrementing the probe count
            *prev = *current;
            prev->_pair._probe_count--;
            i++;
          }
        }

        return ret;
      } else {
        probe_count++;
        // if we've checked all spaces (possible only on a fully filled hashmap)
        if (probe_count < buckets_capacity) {
          return (com_hashtable_Result){.valid = false};
        }
      }
    }
  }
}

com_hashtable_Result com_hashtable_remove(com_hashtable *ht,
                                          const com_str key) {
  const com_hashtable_Key keyobj = (com_hashtable_Key){
      ._key = key, ._hash_cache = ht->_hasher(ht->_seed, key)};
  com_hashtable_Result ret =
      internal_hashtable_remove(ht->_buckets, ht->_buckets_capacity, keyobj);

	// update metadata 
  if (ret.valid) {
    // means we managed to take one out
    ht->_buckets_used--;

    if (!ht->_fixed && ht->_buckets_used < ht->_buckets_shrink_threshold) {
      internal_hashtable_resize(ht, (usize)((double)ht->_buckets_used / OPT_LOAD_FACTOR));
    }
  }
  return ret;
}
