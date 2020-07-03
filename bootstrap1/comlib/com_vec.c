#include "com_vec.h"

#include "com_assert.h"
#include "com_mem.h"

// The percent it will increase when out of room MUST BE POSITIVE
// Ex. 1.5 ->_ 50% expansion each time the limit is hit
#define LOAD_FACTOR 2

usize com_vec_length(const com_vec *vector) { return vector->_length; }
usize com_vec_capacity(const com_vec *vector) { return vector->_capacity; }

void com_vec_setCapacity(com_vec *vector, usize size);
void com_vec_resize(com_vec *vector, usize size);

// Sets the size of the vector
void com_vec_setCapacity(com_vec *vector, usize size) {
  vector->_handle = com_allocator_realloc(vector->_handle, size);

  // com_vec fails on allocation failure
  com_assert_m(vector->_handle.valid, "allocation failure");

  vector->_capacity = size;

  // the vector's base pointer may change during this operation
  vector->_data = com_allocator_handle_get(vector->_handle);
}

// increases the capacity of the vector in order to fit an element of this size in
void com_vec_resize(com_vec *vector, usize size) {
  com_assert_m(vector->_resizeable, "vector is not resizable");
  // This is the new size of the vector if we used the loadFactor
  usize newCapacity = (usize)((vector->_length + size) * LOAD_FACTOR);
  com_vec_setCapacity(vector, newCapacity);
}

com_vec com_vec_create(com_allocator_Handle handle) {
  com_assert_m(handle.valid, "handle is invalid");
  com_allocator_HandleData hdata = com_allocator_handle_query(handle);

  return (com_vec){._length = 0,
                   ._capacity = hdata.len,
                   ._handle = handle,
                   ._data = com_allocator_handle_get(handle),
                   ._resizeable = hdata.flags & com_allocator_REALLOCABLE};
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
    com_vec_resize(vector, len);
  }
  vector->_length += len;
  // copy data currently at loc back
  u8 *src = com_vec_get(vector, loc);
  u8 *dest = com_vec_get(vector, loc + len);
  // Move memory from end of allocation back
  com_mem_move(dest, src, vector->_length - (loc + len));
  // Zero out new memory
  com_mem_zero(src, len);
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
}

void com_vec_set_len(com_vec* vec, usize len) {
  com_assert_m(vector->_resizeable, "vector is not resizable");
  if(vec->_capacity < len) {
    com_vec_setCapacity(vec, len);
  }
  vec->_length = len;
}
    
void com_vec_append(com_vec *dest, com_vec *src) {
  usize len = com_vec_length(src);
  com_vec_pop(src, com_vec_push(dest, len), len);
  com_vec_destroy(src);
}



com_str com_vec_to_str(com_vec *vec) {
  usize len = com_vec_len_m(vec, u8);
  u8 *data = com_vec_release(vec);
  return com_str_create(data, len);
}
