#ifndef com_com_vec_h_m
#define com_com_vec_h_m

#include "com_allocator.h"
#include "com_str.h"

typedef struct {
  usize _length;
  usize _capacity;
  com_allocator_Handle _handle;
  void *_data;
  bool _resizeable;
} com_vec;

/** Creates a vector using the memory held by `handle`
 * REQUIRES: `handle` is a valid com_allocator_Handle
 * GUARANTEES: returns a valid com_vec
 * GUARANTEES: the initial capacity of `vec` is identical to the size of `handle`
 */
com_vec com_vec_create(com_allocator_Handle handle);

/** Frees `vec`'s data
 * REQUIRES: `vec` is a pointer to a valid com_vec
 * GUARANTEES: memory held by `vec` is deallocated
 * GUARANTEES: `vec` is no longer valid
 */
com_vec *com_vec_destroy(com_vec *vec);

/** Return the data held by `vec`, and invalidates `vec`
 * REQUIRES: `vec` is a pointer to a valid com_vec
 * GUARANTEES: `vec` is no longer valid
 * GUARANTEES: if length of `vec` is 0, will return NULL pointer
 * GUARANTEES: if length of `vec` is greater than 0, returns a pointer to a
 *             valid section of memory  at least the length of the `vec`
 *             containing the contents of `vec`'s data
 */
void *com_vec_release(com_vec *vec);

/** Gets a pointer to the `loc`'th byte of the vector's memory that is valid
 * till the next operation performed on the memory
 * REQUIRES: `vec` is a pointer to a valid com_vec
 * REQUIRES: `loc` < vector's length
 * GUARANTEES: until the subsequent operation, return value will be a valid
 *             pointer to the `loc`'th byte of the vector's data
 */
void *com_vec_get(const com_vec *vec, usize loc);

/// Inserts `len` bytes of memory
/// REQUIRES: `vec` is a pointer to a a valid com_vec
/// REQUIRES: `loc` < `vec`'s current length
/// GUARANTEES: until the subsequent operation, return value will point to `len`
///             bytes of valid memory
/// GUARANTEES: returns a pointer to the `loc`'th byte of `vec`'s data
/// GUARANTEES: all data previously located at or after `loc` will be moved to
///             the right by `len` bytes
void *com_vec_insert(com_vec *vec, usize loc, usize len);

/// Removes `len` bytes of memory
/// If `data` is not NULL, the removed memory will be copied to `data`
/// REQUIRES: `data` is NULL or a pointer to a segment of memory at least `len`
///           bytes 
/// REQUIRES: `vec` is a pointer to a valid com_vec
/// REQUIRES: `loc` < vector's current length 
/// REQUIRES: `len` <= `vec`'s current length - `loc`
/// GUARANTEES: vector's length is decreased by `len` byte
/// GUARANTEES: vector's byes from [loc, `loc` + len) will be removed
/// GUARANTEES: the remainder of the vector will be moved backward `len` bytes
/// GUARANTEES: `len` bytes at data will be overwritten with the removed data
void com_vec_remove(com_vec *vec, void *data, usize loc, usize len);

/// Appends `len` bytes of memory to the end of `vec`
/// REQUIRES: `vec` is a pointer to a valid com_vec
/// GUARANTEES: until the subsequent operation, return value will point to `len`
///             bytes of valid memory
/// GUARANTEES: return value will be located directly after the current last
///             byte of `vec`
void *com_vec_push(com_vec *vec, usize len);

/// Deletes `len` bytes of memory from the end of `vec`
/// If `data` is not NULL, the removed memory will be copied to `data`
/// REQUIRES: `vec` is a pointer to a valid vector
/// REQUIRES: `len` < vector's current length
/// REQUIRES: data is either NULL, or a pointer to a segment of memory at least
///           `len` bytes long 
/// GUARANTEES: the vector's length is decreased by `len` bytes
/// GUARANTEES: if `data` is NULL, the bytes from the end of the array will be lost
/// GUARANTEES: if `data` is not NULL, the last `len` bytes from the array will be copied to `data`
void com_vec_pop(com_vec *vec, void *data, usize len);

/// Returns the length of `vec`
/// REQUIRES: `vec` is a pointer to a valid vector
/// GUARANTEES: returns the current length of the vector in bytes
usize com_vec_length(const com_vec *vec);

/// Returns the capacity of `vec`
/// REQUIRES: `vec` is a pointer to a valid vector
/// GUARANTEES: returns the current capacity of `vec` in bytes
usize com_vec_capacity(const com_vec *vec);

// Appends the contents of src to dest and destroys src
/// REQUIRES: `dest` is a valid pointer to a valid com_vec
/// REQUIRES: `src` is a valid pointer to a valid com_vec
/// GUARANTEES: `src` is no longer valid
/// GUARANTEES: `dest`'s length is now `dest`'s length + `src`'s length
/// GUARANTEES: the contents of `src` are appended to `dest`
void com_vec_append(com_vec* dest, com_vec* src);

// Sets the length of `vec`, potentially deleting from the end or extending
/// REQUIRES: `vec` is a valid pointer to a valid com_vec
/// REQUIRES: `len` is the new length of the vector that it will be resized to in bytes
/// GUARANTEES: `vec` is now `len` bytes long
/// GUARANTEES: if `vec` is longer than `len` then the extra data will be truncated
/// GUARANTEES: if `vec` is shorter than `len` then the additional space will be undefined contents
void com_vec_set_len(com_vec* vec, usize len);

// Releases `vec` and turns it into a string
/// REQUIRES: `vec` is a valid com_vec
/// REQUIRES: `vec`contains a valid utf8 string
/// GUARANTEES: returns a valid `com_str`
/// GUARANTEES: returned com_str has a length equivalent to `vec`'s length
/// GUARANTEES: returned com_str vector
/// GUARANTEES: `vec` is released
com_str com_vec_to_str(com_vec* vec);

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
#define com_vec_set_len_m(vector, len, type) com_vec_set_len((vector), (len)*sizeof(type))

#endif
