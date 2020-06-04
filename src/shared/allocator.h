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
  size_t alloc_id;
  void* ptr;
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
 * GUARANTEES: if `size` is 0, NULL will be returned
 * GUARANTEES: if allocation succeeds, a pointer to `size` bytes of contiguous memory will be returned 
 * GUARANTEES: if allocation fails, NULL will be returned
 * GUARANTEES: the memory will be allocated with default properties of the implementing allocator
 */
static inline MemHandle a_alloc(const Allocator * a, size_t size) {
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
static inline MemHandle a_alloc_flags(const Allocator * a, size_t size, AllocatorFlags flags) {
  // guarantee all flags are supported by this allocator
  assert((a_supports(a) & flags) == flags);
  return a->allocator_flags_fn(a->allocator_backing, size, flags);
}

/** deallocate memory
 * REQUIRES: `a` is a valid pointer to an Allocator
 * REQUIRES: `ptr` is a memory location returned by a previous call to `a_alloc` or `a_alloc_flags` using `a`
 * GUARANTEES: `ptr` will be freed if possible, with memory returning to the OS
 */
static inline void a_dealloc(const Allocator * a, MemHandle h) {
  a->deallocator_fn(a->allocator_backing, h);
}

/** reallocate memory
 * REQUIRES: `a` is a valid pointer to an Allocator
 * REQUIRES: `ptr` is a memory location returned by a previous call to `a_alloc_flags` using `a` with the `A_REALLOCABLE` bit enabled
 * GUARANTEES: if `size` is 0, NULL will be returned
 * GUARANTEES: if allocation succeeds, a pointer to `size` bytes of contiguous memory will be returned, preserving data
 * GUARANTEES: if allocation succeeds, the returned pointer will have the same properties as `ptr` 
 * NOT GUARANTEED: it is explicitly not guaranteed that the returned value is the same as `ptr`
 * GUARANTEES: if allocation fails, NULL will be returned
 */
static inline MemHandle a_realloc(const Allocator * a, MemHandle h, size_t size) {
  assert(a_supports(a) & A_REALLOCABLE);
  return a->reallocator_fn(a->allocator_backing, h, size);
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
