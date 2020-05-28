#include "vector.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "error.h"

// The initial capacity of the vector
#define DEFAULT_INITIAL_CAPACITY 10
// The percent it will increase when out of room MUST BE POSITIVE
// Ex. 1.5 -> 50% expansion each time the limit is hit
#define LOAD_FACTOR 2

void vec_setCapacity(Vector *vector, size_t size);
void vec_resize(Vector *vector, size_t size);

// Sets the size of the vector
void vec_setCapacity(Vector *vector, size_t size) {
  if (size == 0) {
    // free vector data
    a_dealloc(vector->allocator, vector->data);
    vector->data = NULL;
  } else {
    // realloc vector data
    vector->data = a_realloc(vector->allocator, vector->data, size);
  }
  vector->capacity = size;
}

// Resizes the vector in order to fit an element of this size in
void vec_resize(Vector *vector, size_t size) {
  // This is the new size of the vector if we used the loadFactor
  size_t newCapacity = (size_t)((vector->length + size) * LOAD_FACTOR);
  vec_setCapacity(vector, newCapacity);
}

Vector *vec_createWithCapacity(Vector *vector, Allocator* allocator, size_t initialCapacity) {
  assert(a_realloc_possible(allocator));

  vector->data = NULL;
  vector->length = 0;
  vector->allocator = allocator;
  vec_setCapacity(vector, initialCapacity);
  return vector;
}

Vector *vec_create(Vector *vector, Allocator* allocator) {
  return vec_createWithCapacity(vector, allocator, DEFAULT_INITIAL_CAPACITY);
}

Vector *vec_destroy(Vector *vector) {
  vec_setCapacity(vector, 0);
  vector->length = 0;
  return vector;
}

void *vec_release(Vector *vector) {
  vec_setCapacity(vector, vector->length);
  return vector->data;
}

void *vec_get(Vector *vector, size_t loc) {
  assert(loc < vector->length);
  uint8_t *data = vector->data;
  return data + loc;
}

void *vec_push(Vector *vector, size_t len) {
  return vec_insert(vector, vector->length, len);
}

void vec_pop(Vector *vector, void *data, size_t len) {
  assert(len <= vector->length);
  vec_remove(vector, data, vector->length - len, len);
}

void *vec_insert(Vector *vector, size_t loc, size_t len) {
  if (vector->length + len >= vector->capacity) {
    vec_resize(vector, len);
  }
  vector->length += len;
  // copy data currently at loc back
  uint8_t *src = vec_get(vector, loc);
  uint8_t *dest = vec_get(vector, loc + len);
  // Move memory from end of allocation back
  memmove(dest, src, vector->length - (loc + len));
  // Zero out new memory
  memset(src, 0, len);
  return src;
}

void vec_remove(Vector *vector, void *data, size_t loc, size_t len) {
  assert(len <= vector->length - loc);
  uint8_t *src = vec_get(vector, loc + len);
  uint8_t *dest = vec_get(vector, loc);

  if (data != NULL) {
    memmove(data, dest, len);
  }

  vector->length -= len;
  memmove(dest, src, vector->length - loc);
}

size_t vec_length(Vector *vector) { return vector->length; }
size_t vec_capacity(Vector *vector) { return vector->capacity; }
