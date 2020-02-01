#ifndef VECTOR_H
#define VECTOR_H

#include <stddef.h>
#include <stdint.h>

// Do not manually modify this struct
typedef struct {
  // Do not realloc data
  size_t length;
  size_t capacity;
  void *data;
} Vector;

// Creates empty vector, accepting memory for the vector
Vector *createVector(Vector *mem);
// Frees vector, returning the memory for the vector
// Don't use the vector after this
Vector *destroyVector(Vector *vector);

// Deinitializes vector, but returns pointer to backing array
// Don't use the vector after this
// The memory is as long as the vector's length
void *releaseVector(Vector *vector);

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

// Get the length of vector safely
size_t lengthVector(Vector *vector);
// Get the capacity of vector safely
size_t capacityVector(Vector *vector);

// Macros to help work with only one element
#define VEC_GET(vector, index, type)                                           \
  ((type *)getVector(vector, index * sizeof(type)))
#define VEC_INS(vector, index, type)                                           \
  ((type *)insertVector(vector, index * sizeof(type), sizeof(type)))
#define VEC_REM(vector, index, type)                                           \
  removeVector(vector, index * sizeof(type), sizeof(type))
#define VEC_PUSH(vector, type) ((type *)pushVector(vector, sizeof(type)))
#define VEC_POP(vector, data, type) popVector(vector, data, sizeof(type))
#define VEC_LEN(vector, type) (lengthVector(vector) / sizeof(type))

#endif
