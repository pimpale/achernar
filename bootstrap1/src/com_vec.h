#ifndef com_com_vec_h_m
#define com_com_vec_h_m

#include "com_allocator.h"

// Do not manually modify this struct
typedef struct com_Vec_s {
  const com_Allocator* _allocator;
  com_allocator_Flags _mem_flags;
  size_t _length;
  size_t _capacity;
  com_allocator_Handle _mem_handle;
  void *_data;
} com_Vec;

/// Creates a vector (0 is default initial capacity)
/// REQUIRES: `allocator` is a pointer to a valid com_Allocator
/// GUARANTEES: returns a valid com_Vec
com_Vec com_vec_create(const com_Allocator* allocator);

/// Creates a vector with capacity and additional flags
/// REQUIRES: `allocator` is a pointer to a valid com_Allocator
/// REQUIRES: `flags` is a set of allocator flags
/// GUARANTEES: returns a valid com_Vec
/// GUARANTEES: returned vector will have a capacity >= initialCapacity
/// GUARANTEES: memory allocated for this vector will be allocated using `mflags`
com_Vec com_vec_createOptions(const com_Allocator* allocator, size_t initialCapacity, com_allocator_Flags flags);

/// Frees `vec`'s data
/// REQUIRES: `vec` is a pointer to a valid com_Vec
/// GUARANTEES: memory held by `vec` is deallocated
/// GUARANTEES: `vec` is no longer valid
com_Vec *com_vec_destroy(com_Vec *vec);

/// Return the data held by `vec`, and invalidates `vec`
/// REQUIRES: `vec` is a pointer to a valid com_Vec
/// GUARANTEES: `vec` is no longer valid
/// GUARANTEES: if length of `vec` is 0, will return NULL pointer
/// GUARANTEES: if length of `vec` is greater than 0, returns a pointer to a
///             valid section of memory  at least the length of the `vec`
///             containing the contents of `vec`'s data
void *com_vec_release(com_Vec *vec);

/// Gets a pointer to the `loc`'th byte of the vector's memory that is valid
/// till the next operation performed on the memory
/// REQUIRES: `vec` is a pointer to a valid com_Vec
/// REQUIRES: `loc` < vector's length
/// GUARANTEES: until the subsequent operation, return value will be a valid
///             pointer to the `loc`'th byte of the vector's data
void *com_vec_get(com_Vec *vec, size_t loc);

/// Inserts `len` bytes of memory
/// REQUIRES: `vec` is a pointer to a a valid com_Vec
/// REQUIRES: `loc` < `vec`'s current length
/// GUARANTEES: until the subsequent operation, return value will point to `len`
///             bytes of valid memory
/// GUARANTEES: returns a pointer to the `loc`'th byte of `vec`'s data
/// GUARANTEES: all data previously located at or after `loc` will be moved to
///             the right by `len` bytes
void *com_vec_insert(com_Vec *vec, size_t loc, size_t len);

/// Removes `len` bytes of memory
/// If `data` is not NULL, the removed memory will be copied to `data`
/// REQUIRES: `data` is NULL or a pointer to a segment of memory at least `len`
///           bytes 
/// REQUIRES: `vec` is a pointer to a valid com_Vec
/// REQUIRES: `loc` < vector's current length 
/// REQUIRES: `len` <= `vec`'s current length - `loc`
/// GUARANTEES: vector's length is decreased by `len` byte
/// GUARANTEES: vector's byes from [loc, `loc` + len) will be removed
/// GUARANTEES: the remainder of the vector will be moved backward `len` bytes
/// GUARANTEES: `len` bytes at data will be overwritten with the removed data
void com_vec_remove(com_Vec *vec, void *data, size_t loc, size_t len);

/// Appends `len` bytes of memory to the end of `vec`
/// REQUIRES: `vec` is a pointer to a valid com_Vec
/// GUARANTEES: until the subsequent operation, return value will point to `len`
///             bytes of valid memory
/// GUARANTEES: return value will be located directly after the current last
///             byte of `vec`
void *com_vec_push(com_Vec *vec, size_t len);

/// Deletes `len` bytes of memory from the end of `vec`
/// If `data` is not NULL, the removed memory will be copied to `data`
/// REQUIRES: `vec` is a pointer to a valid vector
/// REQUIRES: `len` < vector's current length
/// REQUIRES: data is either NULL, or a pointer to a segment of memory at least
///           `len` bytes long 
/// GUARANTEES: the vector's length is decreased by `len` bytes
/// GUARANTEES: if `data` is NULL, the bytes from the end of the array will be lost
/// GUARANTEES: if `data` is not NULL, the last `len` bytes from the array will be copied to `data`
void com_vec_pop(com_Vec *vec, void *data, size_t len);

/// Returns the length of `vec`
/// REQUIRES: `vec` is a pointer to a valid vector
/// GUARANTEES: returns the current length of the vector in bytes
size_t com_vec_length(const com_Vec *vec);

/// Returns the capacity of `vec`
/// REQUIRES: `vec` is a pointer to a valid vector
/// GUARANTEES: returns the current capacity of `vec` in bytes
size_t com_vec_capacity(const com_Vec *vec);

// Appends the contents of src to dest and destroys src
/// REQUIRES: `dest` is a valid pointer to a valid com_Vec
/// REQUIRES: `src` is a valid pointer to a valid com_Vec
/// GUARANTEES: `src` is no longer valid
/// GUARANTEES: `dest`'s length is now `dest`'s length + `src`'s length
/// GUARANTEES: the contents of `src` are appended to `dest`
void com_vec_append(com_Vec* dest, com_Vec* src);

// Macros to help work with vectors
#define com_vec_get_m(vector, index, type)                                           \
  ((type *)com_vec_get(vector, (index) * sizeof(type)))
#define com_vec_ins_m(vector, index, type)                                           \
  ((type *)com_vec_insert((vector), (index) * sizeof(type), sizeof(type)))
#define com_vec_rem_m(vector, data, index, type)                                     \
  com_vec_remove((vector), (data), (index) * sizeof(type), sizeof(type))
#define com_vec_push_m(vector, type) ((type *)com_vec_push((vector), sizeof(type)))
#define com_vec_pop_m(vector, data, type) com_vec_pop(vector, (data), sizeof(type))
#define com_vec_len_m(vector, type) (com_vec_length(vector) / sizeof(type))

#endif
