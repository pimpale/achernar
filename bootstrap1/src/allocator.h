#ifndef ALLOCATOR_H
#define ALLOCATOR_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>

typedef enum AllocatorFlag_e {
  A_NOFLAGS = 0,
  /// The size of this memory allocation can be grown by a call to `a_realloc`
  A_REALLOCABLE = 1<<0,
  /// The memory will NOT be cleaned up when the allocator is destroyed (can cause resource leak)
  /// You are responsible for cleaning up this memory, and the means of doing so will vary per implementation
  A_NO_CLEANUP_ON_DESTROY = 1<<1,
} AllocatorFlag;

typedef uint32_t AllocatorFlags;

// Handle to allocation, this remains constant over the allocation's life
typedef struct AllocId_t {
  size_t id;
  bool valid;
} AllocId;

// Dynamic allocator for fun
// ALLOCATORS ARE NOT THREAD SAFE
typedef struct Allocator_s {
  AllocatorFlags default_flags;
  AllocatorFlags supported_flags;

  // Opaque pointer to reallocator backend
  void* allocator_backing;

  // AllocId allocate(void* backing, size_t size) 
  AllocId (*allocator_fn)(void*, size_t);
  // AllocId allocate(void* backing, size_t size, uint8_t alignment_power) 
  AllocId (*allocator_flags_fn)(void*, size_t, AllocatorFlags);
  // void deallocate(void* backing, MemHandle h) 
  void (*deallocator_fn)(void*, AllocId);
  // AllocId realloc(void* backing, AllocId id, size_t size) 
  AllocId(*reallocator_fn)(void*, AllocId, size_t);
  // void* get(void* backing, AllocId id);
  void* (*get_fn)(void*, AllocId);

  // void* destroy_allocator(void* backing)
  void (*destroy_allocator_fn)(void*);
} Allocator;

/** flags that are enabled by default for an allocator (cannot be disabled)
 * REQUIRES: `a` is a valid pointer to an Allocator
 * GUARANTEES: returns flags supported by default by `a`
 */
static inline AllocatorFlags a_defaults(const Allocator * a) {
  return a->supported_flags;
}

/** flags that are valid for `a` (may be enabled)
 * REQUIRES: `a` is a valid pointer to an Allocator
 * GUARANTEES: returns flags supported by default by `a`
 */
static inline AllocatorFlags a_supports(const Allocator * a) {
  return a->supported_flags;
}

/** allocate memory
 * REQUIRES: `a` is a valid pointer to an Allocator
 * GUARANTEES: if allocation succeeds, a valid AllocId will be returned representing an allocation for `size` bytes of memory
 * GUARANTEES: if allocation fails, an invalid AllocId will be returned
 * GUARANTEES: the memory will be allocated with default properties of the implementing allocator
 */
static inline AllocId a_alloc(const Allocator * a, size_t size) {
  return a->allocator_fn(a->allocator_backing, size);
}

/** allocate aligned memory
 * REQUIRES: `a` is a valid pointer to an Allocator
 * REQUIRES: all bits enabled in `flags` must be supported by `a`
 * GUARANTEES: if `size` is 0, NULL will be returned
 * GUARANTEES: if allocation succeeds, a pointer to `size` bytes of contiguous memory will be returned
 * GUARANTEES: if allocation succeeds, the memory returned will have the properties specified by the flags
 * GUARANTEES: if allocation fails, NULL will be returned
 */
static inline AllocId a_alloc_flags(const Allocator * a, size_t size, AllocatorFlags flags) {
  // guarantee all flags are supported by this allocator
  assert((a_supports(a) & flags) == flags);
  return a->allocator_flags_fn(a->allocator_backing, size, flags);
}

/** deallocate memory
 * REQUIRES: `a` is a valid pointer to an Allocator
 * REQUIRES: `id` is a valid AllocId returned by a previous call to `a_alloc`, `a_realloc`  or `a_alloc_flags` using `a`
 * GUARANTEES: the memory represented by `id` will be freed if possible, with memory returning to the OS
 */
static inline void a_dealloc(const Allocator * a, AllocId id) {
  a->deallocator_fn(a->allocator_backing, id);
}

/** reallocate memory
 * REQUIRES: `a` is a valid pointer to an Allocator
 * REQUIRES: `id` is a valid AllocId returned by a previous call to `a_alloc_flags` using `a` with the `A_REALLOCABLE` bit enabled
 * GUARANTEES: if `size` is 0, the allocation represented by `id` will be deallocated
 * GUARANTEES: if allocation succeeds, the `size` bytes of contiguous memory will be allocated, preserving data
 * GUARANTEES: if allocation succeeds, a possibly different AllocId will be returned representing the new memory
 * GUARANTEES: if allocation succeeds, allocated memory will have the same properties as `ptr`
 * GUARANTEES: all pointers previously got from `id` are invalidated
 * GUARANTEES: if the reallocation fails, an invalid AllocId will be returned, but `id` will still be valid
 */
static inline AllocId a_realloc(const Allocator * a, AllocId id, size_t size) {
  assert(a_supports(a) & A_REALLOCABLE);
  return a->reallocator_fn(a->allocator_backing, id, size);
}

/** get memory
 * REQUIRES: `a` is a valid pointer to an Allocator
 * REQUIRES: `id` is a valid AllocId returned by a previous call to `a_alloc_flags`, `a_alloc`, or `a_realloc` using `a`
 * GUARANTEES: the returned value will be a pointer to the start of a block of contiguous memory represented by `id`
 * GUARANTEES: this memory is valid till a subsequent call to `a_dealloc` or `a_realloc` with the `id` as an argument
 * GUARANTEES: this memory may be invalidated if `a` is destroyed
 */
static inline void* a_get(const Allocator *a, AllocId id) {
  assert(id.valid);
  return a->get_fn(a->allocator_backing, id);
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
#define ALLOC_ARR(allocator, n, type)                                             \
  ((type *)a_get(allocator, a_alloc((allocator), (n) * sizeof(type))))
#define ALLOC(allocator, type) ALLOC_ARR(allocator, 1, type)

#endif
