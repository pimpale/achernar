#ifndef VECTOR_H
#define VECTOR_H

#include <stddef.h>
#include <stdint.h>

typedef struct __attribute__((__packed__)) {
  void *key;
  // Do not realloc data
  void *data;
  // Do not manually modify length or capacity
  size_t length;
  size_t capacity;
  float loadFactor;
} Vector;

// Creates empty vector
Vector *initVector(Vector *vector);
// Frees vector
void freeVector(Vector *vector);

/* Returns pointer to section of memory guaranteed to be valid until next
 * operation */
void *getVector(Vector *vector, size_t loc);

/* Returns pointer to section of memory guaranteed to be valid until next
 * operation */
void *insertVector(Vector *vector, size_t loc, size_t len);
void removeVector(Vector *vector, size_t loc, size_t len);

/* Returns pointer to section of memory guaranteed to be valid until next
 * operation */
void *pushVector(Vector *vector, size_t len);
void popVector(Vector *vector, void *data, size_t len);

size_t lengthVector(Vector *vector);

// Macros to help work with only one element
#define VEC_GET(vector, index, type)                                           \
  ((type *)getVector(vector, index * sizeof(type)))
#define VEC_INS(vector, index, type)                                           \
  ((type *)insertVector(vector, index * sizeof(type), sizeof(type)))
#define VEC_REM(vector, index, type)                                           \
  removeVector(vector, index * sizeof(type), sizeof(type))
#define VEC_PUSH(vector, type) ((type *)pushVector(vector, sizeof(type)))
#define VEC_POP(vector, data, type) popVector(vector, data, sizeof(type))

#endif
