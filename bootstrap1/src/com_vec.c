#include "com_vec.h"

#include "com_assert.h"
#include "com_mem.h"

// The percent it will increase when out of room MUST BE POSITIVE
// Ex. 1.5 ->_ 50% expansion each time the limit is hit
#define LOAD_FACTOR 2

size_t com_vec_length(const com_vec *vector) { return vector->_length; }
size_t com_vec_capacity(const com_vec *vector) { return vector->_capacity; }

void com_vec_setCapacity(com_vec *vector, size_t size);
void com_vec_resize(com_vec *vector, size_t size);

// Sets the size of the vector
void com_vec_setCapacity(com_vec *vector, size_t size) {
  vector->_handle = com_allocator_handle_realloc(vector->_handle, size);

  // com_vec  fails on allocation failure
  com_assert_m(vector->_handle.valid, "allocation failure");

  vector->_capacity = size;

  // the vector's base pointer may change during this operation
  vector->_data = com_allocator_handle_get(vector->_handle);
}

// Resizes the vector in order to fit an element of this size in
void com_vec_resize(com_vec *vector, size_t size) {
  com_assert_m(vector->_resizeable, "vector is not resizable");
  // This is the new size of the vector if we used the loadFactor
  size_t newCapacity = (size_t)((vector->_length + size) * LOAD_FACTOR);
  com_vec_setCapacity(vector, newCapacity);
}

com_vec com_vec_create(com_allocator_Handle handle) {
  com_assert_m(handle.valid, "handle is invalid");
  com_allocator_HandleData hdata = com_allocator_handle_query(handle);

  return (com_vec){._length = 0,
                   ._capacity = hdata.len,
                   ._handle = handle,
                   ._data = com_allocator_handle_get(handle),
                   ._resizeable = hdata.flags & com_allocator_Reallocable};
}

com_vec *com_vec_destroy(com_vec *vector) {
  com_vec_setCapacity(vector, 0);
  vector->_length = 0;
  return vector;
}

void *com_vec_release(com_vec *vector) {
  com_vec_setCapacity(vector, vector->_length);
  return vector->_data;
}

void *com_vec_get(com_vec *vector, size_t loc) {
  com_assert_m(loc <= vector->_length, "out of bounds vector access");
  uint8_t *data = vector->_data;
  return data + loc;
}

void *com_vec_push(com_vec *vector, size_t len) {
  return com_vec_insert(vector, vector->_length, len);
}

void com_vec_pop(com_vec *vector, void *data, size_t len) {
  com_assert_m(len <= vector->_length, "vector underflow");
  com_vec_remove(vector, data, vector->_length - len, len);
}

void *com_vec_insert(com_vec *vector, size_t loc, size_t len) {
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

void com_vec_remove(com_vec *vector, void *data, size_t loc, size_t len) {
  com_assert_m(len <= vector->_length - loc, "vector underflow");
  uint8_t *src = com_vec_get(vector, loc + len);
  uint8_t *dest = com_vec_get(vector, loc);

  if (data != NULL) {
    com_mem_move(data, dest, len);
  }

  vector->_length -= len;
  com_mem_move(dest, src, vector->_length - loc);
}

void com_vec_append(com_vec *dest, com_vec *src) {
  size_t len = com_vec_length(src);
  com_vec_pop(src, com_vec_push(dest, len), len);
  com_vec_destroy(src);
}

com_str com_vec_to_str(com_vec *vec) {
  usize len = com_vec_len_m(vec, u8);
  u8 *data = com_vec_release(vec);
  return com_str_create(data, len);
}
