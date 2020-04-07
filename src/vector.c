#include "vector.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"

// The initial capacity of the vector
#define INITIAL_CAPACITY 10
// The percent it will increase when out of room MUST BE POSITIVE
// Ex. 1.5 -> 50% expansion each time the limit is hit
#define LOAD_FACTOR 1.5f

void setSizeVector(Vector *vector, size_t size);
void resizeVector(Vector *vector, size_t size);

// Sets the size of the vector
void setSizeVector(Vector *vector, size_t size) {
  vector->data = realloc(vector->data, size);
  vector->capacity = size;
}

// Resizes the vector in order to fit an element of this size in
void resizeVector(Vector *vector, size_t size) {
  // This is the new size of the vector if we used the loadFactor
  size_t newCapacity = (size_t)((vector->length + size) * LOAD_FACTOR);
  setSizeVector(vector, newCapacity);
}

Vector *createWithCapacityVector(Vector *vector, size_t initialCapacity) {
  vector->data = NULL;
  vector->length = 0;
  vector->capacity = 0;
  resizeVector(vector, initialCapacity);
  return vector;
}

Vector *createVector(Vector *vector) {
  return createWithCapacityVector(vector, INITIAL_CAPACITY);
}

Vector *destroyVector(Vector *vector) {
  free(vector->data);
  vector->capacity = 0;
  vector->length = 0;
  return vector;
}

void* releaseVector(Vector *vector) {
  if(vector->length == 0) {
    destroyVector(vector);
    return NULL;
  } else {
    vector->data = realloc(vector->data, vector->length);
  }
  return vector->data;
}

void *getVector(Vector *vector, size_t loc) {
  if (loc > vector->length) {
    INTERNAL_ERROR("out of bounds");
    PANIC();
  }
  uint8_t *data = vector->data;
  return data + loc;
}

void *pushVector(Vector *vector, size_t len) {
  return insertVector(vector, vector->length, len);
}

void popVector(Vector *vector, void *data, size_t len) {
  if (len > vector->length) {
    INTERNAL_ERROR("vector underflow");
    PANIC();
  }
  removeVector(vector, data, vector->length - len, len);
}

// Insert a segment of empty data of len length at the specified position loc
void *insertVector(Vector *vector, size_t loc, size_t len) {
  if (vector->length + len >= vector->capacity) {
    resizeVector(vector, len);
  }
  vector->length += len;
  // copy data currently at loc back
  uint8_t *src = getVector(vector, loc);
  uint8_t *dest = getVector(vector, loc + len);
  // Move memory from end of allocation back
  memmove(dest, src, vector->length - (loc + len));
  // Zero out new memory
  memset(src, 0, len);
  return src;
}

void removeVector(Vector *vector, void* data, size_t loc, size_t len) {
  if (len > vector->length - loc) {
    INTERNAL_ERROR("vector underflow");
    PANIC();
  }

  uint8_t *src = getVector(vector, loc + len);
  uint8_t *dest = getVector(vector, loc);

  if (data != NULL) {
    memmove(data, dest, len);
  }

  vector->length -= len;
  memmove(dest, src, vector->length - loc);
}

size_t lengthVector(Vector *vector) { return vector->length; }
size_t capacityVector(Vector *vector) { return vector->capacity; }
