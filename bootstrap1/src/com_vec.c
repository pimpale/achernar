#include "com_vec.h"

#include "com_mem.h"
#include "com_assert.h"

// The percent it will increase when out of room MUST BE POSITIVE
// Ex. 1.5 ->_ 50% expansion each time the limit is hit
#define LOAD_FACTOR 2

size_t com_vec_length(const com_Vec *vector) { return vector->_length; }
size_t com_vec_capacity(const com_Vec *vector) { return vector->_capacity; }

void com_vec_setCapacity(com_Vec *vector, size_t size);
void com_vec_resize(com_Vec *vector, size_t size);

// Sets the size of the vector
void com_vec_setCapacity(com_Vec *vector, size_t size) {
  if (vector->_data == NULL) {
    vector->_mem_handle = com_allocator_alloc_flags(vector->_allocator, size, vector->_mem_flags);
  } else {
    vector->_mem_handle = com_allocator_realloc(vector->_allocator, vector->_mem_handle, size);
    // com_Vec fails on allocation failure
    com_assert_m(vector->_mem_handle.valid, "allocation failure");
  }
  vector->_capacity = size;
  // the vector's base pointer may change durin this operation
  vector->_data = com_allocator_get(vector->_allocator, vector->_mem_handle);
}

// Resizes the vector in order to fit an element of this size in
void com_vec_resize(com_Vec *vector, size_t size) {
  // This is the new size of the vector if we used the loadFactor
  size_t newCapacity = (size_t)((vector->_length + size) * LOAD_FACTOR);
  com_vec_setCapacity(vector, newCapacity);
}

com_Vec com_vec_createOptions(const com_Allocator *allocator, size_t initialCapacity,
                         com_allocator_Flags flags) {
  com_Vec vector;
  vector._data = NULL;
  vector._length = 0;
  vector._allocator = allocator;
  vector._mem_flags = flags;
  com_vec_setCapacity(&vector, initialCapacity);
  return vector;
}

com_Vec com_vec_create(const com_Allocator *allocator) {
  return com_vec_createOptions(allocator, 0, com_allocator_Reallocable);
}

com_Vec *com_vec_destroy(com_Vec *vector) {
  com_vec_setCapacity(vector, 0);
  vector->_length = 0;
  return vector;
}

void *com_vec_release(com_Vec *vector) {
  com_vec_setCapacity(vector, vector->_length);
  return vector->_data;
}

void *com_vec_get(com_Vec *vector, size_t loc) {
  com_assert_m(loc <= vector->_length, "out of bounds vector access");
  uint8_t *data = vector->_data;
  return data + loc;
}

void *com_vec_push(com_Vec *vector, size_t len) {
  return com_vec_insert(vector, vector->_length, len);
}

void com_vec_pop(com_Vec *vector, void *data, size_t len) {
  com_assert_m(len <= vector->_length, "vector underflow");
  com_vec_remove(vector, data, vector->_length - len, len);
}

void *com_vec_insert(com_Vec *vector, size_t loc, size_t len) {
  if (vector->_length + len >= vector->_capacity) {
    com_vec_resize(vector, len);
  }
  vector->_length += len;
  // copy data currently at loc back
  uint8_t *src = com_vec_get(vector, loc);
  uint8_t *dest = com_vec_get(vector, loc + len);
  // Move memory from end of allocation back
  com_mem_move(dest, src, vector->_length - (loc + len));
  // Zero out new memory
  com_mem_zero(src, len);
  return src;
}

void com_vec_remove(com_Vec *vector, void *data, size_t loc, size_t len) {
  com_assert_m(len <= vector->_length - loc, "vector underflow");
  uint8_t *src = com_vec_get(vector, loc + len);
  uint8_t *dest = com_vec_get(vector, loc);

  if (data != NULL) {
    com_mem_move(data, dest, len);
  }

  vector->_length -= len;
  com_mem_move(dest, src, vector->_length - loc);
}

void com_vec_append(com_Vec *dest, com_Vec *src) {
  size_t len = com_vec_length(src);
  com_vec_pop(src, com_vec_push(dest, len), len);
  com_vec_destroy(src);
}
