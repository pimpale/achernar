#include "vector.h"

#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"

// The percent it will increase when out of room MUST BE POSITIVE
// Ex. 1.5 -> 50% expansion each time the limit is hit
#define LOAD_FACTOR 2

size_t vec_length(const Vector *vector) { return vector->length; }
size_t vec_capacity(const Vector *vector) { return vector->capacity; }

void vec_setCapacity(Vector *vector, size_t size);
void vec_resize(Vector *vector, size_t size);

// Sets the size of the vector
void vec_setCapacity(Vector *vector, size_t size) {
  if (vector->data == NULL) {
    vector->alloc_id = a_alloc_flags(vector->allocator, size, vector->mflags);
  } else {
    vector->alloc_id = a_realloc(vector->allocator, vector->alloc_id, size);
    // Vector fails on allocation failure
    assert(vector->alloc_id.valid);
  }
  vector->capacity = size;
  // the vector's base pointer may change durin this operation
  vector->data = a_get(vector->allocator, vector->alloc_id);
}

// Resizes the vector in order to fit an element of this size in
void vec_resize(Vector *vector, size_t size) {
  // This is the new size of the vector if we used the loadFactor
  size_t newCapacity = (size_t)((vector->length + size) * LOAD_FACTOR);
  vec_setCapacity(vector, newCapacity);
}

Vector vec_createOptions(const Allocator *allocator, size_t initialCapacity,
                         AllocatorFlags mflags) {
  Vector vector;
  vector.data = NULL;
  vector.length = 0;
  vector.allocator = allocator;
  vector.mflags = mflags;
  vec_setCapacity(&vector, initialCapacity);
  return vector;
}

Vector vec_create(const Allocator *allocator) {
  return vec_createOptions(allocator, 0, A_REALLOCABLE);
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
  assert(loc <= vector->length);
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

void vec_append(Vector *dest, Vector *src) {
  size_t len = vec_length(src);
  vec_pop(src, vec_push(dest, len), len);
  vec_destroy(src);
}
