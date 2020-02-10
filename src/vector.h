#ifndef VECTOR_H
#define VECTOR_H

#include <stddef.h>
#include <stdint.h>

// Do not manually modify this struct
typedef struct vector_s {
  size_t length;
  size_t capacity;
  void *data;
} Vector;

/// Creates a vector (10 is default initial capacity)
/// REQUIRES: mem is a pointer to a valid section of memory
/// GUARANTEES: returns a pointer to a valid Vector
Vector *createVector(Vector *mem);

/// Creates a vector with capacity
/// REQUIRES: mem is a pointer to a valid section of memory
/// REQUIRES: initialCapacity > 0
/// GUARANTEES: returns a pointer to a valid Vector
/// GUARANTEES: created vector will have >= initialCapacity bytes of storage
Vector *createWithCapacityVector(Vector *mem, size_t initialCapacity);

/// Frees vector's data
/// REQUIRES: vector is a pointer to a valid Vector
/// GUARANTEES: memory held by vector is deallocated
/// GUARANTEES: vector is no longer valid
Vector *destroyVector(Vector *vector);

/// Return the data held by vector, while destroying the vector
/// REQUIRES: vector is a pointer to a valid Vector
/// GUARANTEES: vector is no longer valid
/// GUARANTEES: if length of vector is 0, will return null pointer
/// GUARANTEES: return contains a pointer to a valid section of memory
///             at least the length of the vector containing the contents 
///             of vector
void *releaseVector(Vector *vector);

/// Gets a pointer to the loc'th byte of the vector's memory
/// REQUIRES: vector is a pointer to a valid Vector
/// REQUIRES: loc < vector's length
/// GUARANTEES: until the subsequent operation, return value will be a valid
///             pointer to the loc'th byte of the vector's data
void *getVector(Vector *vector, size_t loc);

/// Inserts len bytes of memory
/// REQUIRES: vector is a pointer to a a valid Vector
/// REQUIRES: loc < vector's current length
/// GUARANTEES: until the subsequent operation, return value will point to len
///             bytes of valid memory
/// GUARANTEES: the value returned will a pointer to the loc'th byte of the
///             vector's data
/// GUARANTEES: the remainder of the vector will be moved forward len bytes
void *insertVector(Vector *vector, size_t loc, size_t len);

/// Removes len bytes of memory
/// REQUIRES: vector is a pointer to a valid Vector
/// REQUIRES: loc < vector's current length
/// REQUIRES: len <= vector's current length - loc
/// GUARANTEES: vector's length is decreased by len byte
/// GUARANTEES: vector's byes from [loc, loc + len) will be removed
/// GUARANTEES: the remainder of the vector will be moved backward len bytes
void removeVector(Vector *vector, size_t loc, size_t len);

/// Appends len bytes of memory to the end of the vector
/// REQUIRES: vector is a pointer to a valid vector
/// GUARANTEES: until the subsequent operation, return value will point to len
///             bytes of valid memory
/// GUARANTEES: return value will be located directly after the current last
///             byte of the vector
void *pushVector(Vector *vector, size_t len);

/// Deletes len bytes of memory from the end of the vector
/// REQUIRES: vector is a pointer to a valid vector
/// GUARANTEES: until the subsequent operation, return value will point to len
///             bytes of valid memory
void popVector(Vector *vector, void *data, size_t len);

/// Returns the length of the vector
/// REQUIRES: vector is a pointer to a valid vector
/// GUARANTEES: returns the current length of the vector in bytes
size_t lengthVector(Vector *vector);

/// Returns the capacity of the vector
/// REQUIRES: vector is a pointer to a valid vector
/// GUARANTEES: returns the current capacity of the vector in bytes
size_t capacityVector(Vector *vector);

// Macros to help work with vectors
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
