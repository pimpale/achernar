#ifndef VECTOR_H
#define VECTOR_H

#include <stddef.h>
#include <stdint.h>

#include "allocator.h"

// Do not manually modify this struct
typedef struct Vector_s {
  const Allocator* allocator;
  AllocatorFlags mflags;
  size_t length;
  size_t capacity;
  void *data;
} Vector;

/// Creates a vector (0 is default initial capacity)
/// REQUIRES: `allocator` is a pointer to a valid Allocator
/// GUARANTEES: returns a valid Vector
Vector vec_create(const Allocator* allocator);

/// Creates a vector with capacity and additional flags
/// REQUIRES: `allocator` is a pointer to a valid Allocator
/// REQUIRES: `mflags` is a set of allocator flags
/// GUARANTEES: returns a valid Vector
/// GUARANTEES: returned vector will have a capacity >= initialCapacity
/// GUARANTEES: memory allocated for this vector will be allocated using `mflags`
Vector vec_createOptions(const Allocator* allocator, size_t initialCapacity, AllocatorFlags mflags);

/// Frees `vec`'s data
/// REQUIRES: `vec` is a pointer to a valid Vector
/// GUARANTEES: memory held by `vec` is deallocated
/// GUARANTEES: `vec` is no longer valid
Vector *vec_destroy(Vector *vec);

/// Return the data held by `vec`, and invalidates `vec`
/// REQUIRES: `vec` is a pointer to a valid Vector
/// GUARANTEES: `vec` is no longer valid
/// GUARANTEES: if length of `vec` is 0, will return NULL pointer
/// GUARANTEES: if length of `vec` is greater than 0, returns a pointer to a
///             valid section of memory  at least the length of the `vec`
///             containing the contents of `vec`'s data
void *vec_release(Vector *vec);

/// Gets a pointer to the `loc`'th byte of the vector's memory that is valid
/// till the next operation performed on the memory
/// REQUIRES: `vec` is a pointer to a valid Vector
/// REQUIRES: `loc` < vector's length
/// GUARANTEES: until the subsequent operation, return value will be a valid
///             pointer to the `loc`'th byte of the vector's data
void *vec_get(Vector *vec, size_t loc);

/// Inserts `len` bytes of memory
/// REQUIRES: `vec` is a pointer to a a valid Vector
/// REQUIRES: `loc` < `vec`'s current length
/// GUARANTEES: until the subsequent operation, return value will point to `len`
///             bytes of valid memory
/// GUARANTEES: returns a pointer to the `loc`'th byte of `vec`'s data
/// GUARANTEES: all data previously located at or after `loc` will be moved to
///             the right by `len` bytes
void *vec_insert(Vector *vec, size_t loc, size_t len);

/// Removes `len` bytes of memory
/// If `data` is not NULL, the removed memory will be copied to `data`
/// REQUIRES: `data` is NULL or a pointer to a segment of memory at least `len`
/// bytes REQUIRES: `vec` is a pointer to a valid Vector REQUIRES: `loc` <
/// vector's current length REQUIRES: `len` <= `vec`'s current length - `loc`
/// GUARANTEES: vector's length is decreased by `len` byte
/// GUARANTEES: vector's byes from [loc, `loc` + len) will be removed
/// GUARANTEES: the remainder of the vector will be moved backward `len` bytes
/// GUARANTEES: `len` bytes at data will be overwritten with the removed data
void vec_remove(Vector *vec, void *data, size_t loc, size_t len);

/// Appends `len` bytes of memory to the end of `vec`
/// REQUIRES: `vec` is a pointer to a valid Vector
/// GUARANTEES: until the subsequent operation, return value will point to `len`
///             bytes of valid memory
/// GUARANTEES: return value will be located directly after the current last
///             byte of `vec`
void *vec_push(Vector *vec, size_t len);

/// Deletes `len` bytes of memory from the end of `vec`
/// If `data` is not NULL, the removed memory will be copied to `data`
/// REQUIRES: `vec` is a pointer to a valid vector
/// REQUIRES: `len` < vector's current length
/// REQUIRES: data is either NULL, or a pointer to a segment of memory at least
/// `len` bytes long GUARANTEES: the vector's length is decreased by `len` bytes
/// GUARANTEES: if `data` is NULL, the bytes from the end of the array will be
/// lost GUARANTEES: if `data` is not NULL, the the bytes from the
void vec_pop(Vector *vec, void *data, size_t len);

/// Returns the length of `vec`
/// REQUIRES: `vec` is a pointer to a valid vector
/// GUARANTEES: returns the current length of the vector in bytes
size_t vec_length(const Vector *vec);

/// Returns the capacity of `vec`
/// REQUIRES: `vec` is a pointer to a valid vector
/// GUARANTEES: returns the current capacity of `vec` in bytes
size_t vec_capacity(const Vector *vec);

// Appends the contents of src to dest and destroys src
/// REQUIRES: `dest` is a valid pointer to a valid Vector
/// REQUIRES: `src` is a valid pointer to a valid Vector
/// GUARANTEES: `src` is no longer valid
/// GUARANTEES: `dest`'s length is now `dest`'s length + `src`'s length
/// GUARANTEES: the contents of `src` are appended to `dest`
void vec_append(Vector* dest, Vector* src);

// Macros to help work with vectors
#define VEC_GET(vector, index, type)                                           \
  ((type *)vec_get(vector, (index) * sizeof(type)))
#define VEC_INS(vector, index, type)                                           \
  ((type *)vec_insert((vector), (index) * sizeof(type), sizeof(type)))
#define VEC_REMOVE(vector, data, index, type)                                     \
  removeVector((vector), (data), (index) * sizeof(type), sizeof(type))
#define VEC_PUSH(vector, type) ((type *)vec_push((vector), sizeof(type)))
#define VEC_POP(vector, data, type) vec_pop(vector, (data), sizeof(type))
#define VEC_LEN(vector, type) (vec_length(vector) / sizeof(type))

#endif
