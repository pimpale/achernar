#ifndef ALLOCATOR_H
#define ALLOCATOR_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>

// Dynamic allocator for fun
// ALLOCATORS ARE NOT THREAD SAFE
typedef struct Allocator_s {
  // is realloc valid?
  bool realloc_possible;
  // is aligned memory possible
  bool aligned_possible;

  // Opaque pointer to reallocator backend
  void* allocator_backing;

  // void* allocate(void* backing, size_t size) 
  void *(*allocator_fn)(void*, size_t);
  // void* allocate(void* backing, size_t size, uint8_t alignment_power) 
  void *(*aligned_allocator_fn)(void*, size_t, uint8_t);
  // void* deallocate(void* backing, void* ptr) 
  void (*deallocator_fn)(void*, void*);
  // void* realloc(void* backing, void* ptr, size_t size) 
  void *(*reallocator_fn)(void*, void*, size_t);
  // void* destroy_allocator(void* backing)
  void (*destroy_allocator_fn)(void*);
} Allocator;

/** determines if realloc is possible for this allocator
 * REQUIRES: `a` is a valid pointer to an Allocator
 * GUARANTEES: returns if `a` supports reallocating pointers
 */
static inline bool a_realloc_possible(Allocator* a) {
  return a->realloc_possible;
}

/** determines if aligned memory is possible for this allocator
 * REQUIRES: `a` is a valid pointer to an Allocator
 * GUARANTEES: returns if `a` supports aligned pointers
 */
static inline bool a_aligned_possible(Allocator *a) {
  return a->realloc_possible;
}

/** allocate memory
 * REQUIRES: `a` is a valid pointer to an Allocator
 * GUARANTEES: if `size` is 0, NULL will be returned
 * GUARANTEES: if allocation succeeds, a pointer to `size` bytes of contiguous memory will be returned 
 * GUARANTEES: if allocation succeeds and `failed` is not null, failed will be false
 * GUARANTEES: if allocation fails, NULL will be returned
 */
static inline void* a_alloc(Allocator* a, size_t size, bool* failed) {
  return a->allocator_fn(a->allocator_backing, size);
}


/** allocate aligned memory
 * REQUIRES: `a` is a valid pointer to an Allocator
 * REQUIRES: `a` must support aligned pointers (verify with `a_aligned_possible`)
 * REQUIRES: `alignment_power` is the power of two that the memory should be aligned to
 * REQUIRES: `failed` is a valid pointer to a bool or is NULL
 * GUARANTEES: if `size` is 0, NULL will be returned
 * GUARANTEES: if allocation succeeds, a pointer to `size` bytes of contiguous memory will be returned
 * GUARANTEES: if allocation succeeds, the returned pointer will be a multiple of 2^`alignment_power`
 * GUARANTEES: if allocation fails, NULL will be returned
 */
static inline void* a_alloc_aligned(Allocator* a, size_t size, uint8_t alignment_power) {
  assert(a_aligned_possible(a));
  return a->aligned_allocator_fn(a->allocator_backing, size, alignment_power);
}

/** deallocate memory
 * REQUIRES: `a` is a valid pointer to an Allocator
 * REQUIRES: `ptr` is a memory location returned by a previous call to `a_alloc` or `a_aligned_alloc` using `a`
 * GUARANTEES: `ptr` will be freed if possible, with memory returning to the OS
 */
static inline void a_dealloc(Allocator* a, void* ptr) {
  a->deallocator_fn(a->allocator_backing, ptr);
}

/** reallocate memory
 * REQUIRES: `a` is a valid pointer to an Allocator
 * REQUIRES: `ptr` is a memory location returned by a previous call to `a_alloc` or `a_aligned_alloc` using `a`
 * GUARANTEES: if `size` is 0, NULL will be returned
 * GUARANTEES: if allocation succeeds, a pointer to `size` bytes of contiguous memory will be returned, preserving data
 * GUARANTEES: if allocation succeeds, the returned pointer will be of the same alignment as `ptr`
 * NOT GUARANTEED: it is explicitly not guaranteed that the returned value is the same as `ptr`
 * GUARANTEES: if allocation fails, NULL will be returned
 */
static inline void* a_realloc(Allocator* a, void* ptr, size_t size) {
  assert(a_realloc_possible(a));
  return a->reallocator_fn(a->allocator_backing, ptr, size);
}

/** destroy allocator
 * REQUIRES: `a` is a valid pointer to an Allocator
 * GUARANTEES: `a` is no longer a valid pointer to an Allocator
 * GUARANTEES: all memory held by `a` is in an UNDEFINED state
 * NOT GUARANTEED: it is not guaranteed that all memory held by the allocator will be returned to the OS. it is implementation specific
 */
static inline void a_destroy(Allocator *a) {
  a->destroy_allocator_fn(a->allocator_backing);
}

// Created this macro for type safety
#define ALLOC(allocator, type) ALLOC_ARR((allocator), 1, (type))
#define ALLOC_ARR(allocator, n, type)                                             \
  (type *)a_alloc((allocator), ((n) * sizeof(type)))

#endif
