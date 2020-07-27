#include "com_vec.h"

#include "com_assert.h"
#include "com_mem.h"

// The percent it will increase when out of room MUST BE > 1
// Ex. 1.5 ->_ 50% expansion each time the limit is hit
#define EXPANSION_FACTOR 2.0
// ratio of length/capacity that motivates us to reduce capacity
#define SHRINK_THRESHOLD_RATIO 0.2
// when we do a shrink the ratio we want to be left with
#define SHRINK_GOAL_RATIO 0.5

void internal_vec_setCapacity(com_vec *vector, usize size);
void internal_vec_expand(com_vec *vector, usize size);
void internal_vec_shrink(com_vec *vector);

// Sets the size of the vector
void internal_vec_setCapacity(com_vec *vector, usize size) {
  vector->_handle = com_allocator_realloc(vector->_handle, size);

  // com_vec fails on allocation failure
  com_assert_m(vector->_handle.valid, "allocation failure");

  vector->_capacity = size;
  vector->_length_to_shrink = (usize) (vector->_capacity * SHRINK_THRESHOLD_RATIO);

  // the vector's base pointer may change during this operation
  vector->_data = com_allocator_handle_get(vector->_handle);
}

// increases the capacity of the vector in order to fit an element of this size
void internal_vec_expand(com_vec *vector, usize size) {
  // This is the new size of the vector if we used the loadFactor
  usize newCapacity = (usize)((vector->_length + size) * EXPANSION_FACTOR);
  internal_vec_setCapacity(vector, newCapacity);
}

// shrinks the capacity of the vector if the length is too small
void internal_vec_shrink(com_vec *vector) {
	if(vector->_length_to_shrink > vector->_length) {
		internal_vec_setCapacity(vector, vector->_capacity * SHRINK_GOAL_RATIO);
	}
}

com_vec com_vec_create(com_allocator_Handle handle) {
  com_assert_m(handle.valid, "handle is invalid");
  com_allocator_HandleData hdata = com_allocator_handle_query(handle);

  return (com_vec){._length = 0,
                   ._length_to_shrink  = 0,
                   ._capacity = hdata.len,
                   ._handle = handle,
                   ._data = com_allocator_handle_get(handle)};
}

com_vec *com_vec_destroy(com_vec *vector) {
  internal_vec_setCapacity(vector, 0);
  vector->_length = 0;
  return vector;
}

void *com_vec_release(com_vec *vector) {
  internal_vec_setCapacity(vector, vector->_length);
  return vector->_data;
}

void *com_vec_get(const com_vec *vector, usize loc) {
  com_assert_m(loc <= vector->_length, "out of bounds vector access");
  u8 *data = vector->_data;
  return data + loc;
}

void *com_vec_push(com_vec *vector, usize len) {
  return com_vec_insert(vector, vector->_length, len);
}

void com_vec_pop(com_vec *vector, void *data, usize len) {
  com_assert_m(len <= vector->_length, "vector underflow");
  com_vec_remove(vector, data, vector->_length - len, len);
}

void *com_vec_insert(com_vec *vector, usize loc, usize len) {
  if (vector->_length + len >= vector->_capacity) {
    internal_vec_expand(vector, len);
  }
  vector->_length += len;
  // copy data currently at loc back
  u8 *src = com_vec_get(vector, loc);
  u8 *dest = com_vec_get(vector, loc + len);
  // Move memory from end of allocation back
  com_mem_move(dest, src, vector->_length - (loc + len));
  return src;
}

void com_vec_remove(com_vec *vector, void *data, usize loc, usize len) {
  com_assert_m(len <= vector->_length - loc, "vector underflow");
  u8 *src = com_vec_get(vector, loc + len);
  u8 *dest = com_vec_get(vector, loc);

  if (data != NULL) {
    com_mem_move(data, dest, len);
  }

  vector->_length -= len;
  com_mem_move(dest, src, vector->_length - loc);
	// shrink if necessary
  internal_vec_shrink(vector);
}

void com_vec_set_len(com_vec *vec, usize len) {
  if (vec->_capacity < len) {
    internal_vec_setCapacity(vec, len);
  }
  vec->_length = len;
	// shrink if necessary
  internal_vec_shrink(vec);
}

com_str com_vec_to_str(com_vec *vec) {
  usize len = com_vec_len_m(vec, u8);
  u8 *data = com_vec_release(vec);
  return com_str_create(data, len);
}

usize com_vec_length(const com_vec *vector) { return vector->_length; }
usize com_vec_capacity(const com_vec *vector) { return vector->_capacity; }
