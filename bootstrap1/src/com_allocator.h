#ifndef COM_ALLOCATOR_H
#define COM_ALLOCATOR_H

#include "com_define.h"
#include "com_assert.h"

typedef enum {
  com_allocator_None = 0,
  /// The size of this memory allocation can be grown by a call to `com_allocator_realloc`
  com_allocator_Reallocable = 1<<0,
  /// The memory will NOT be cleaned up when the allocator is destroyed (can cause resource leak)
  /// You are responsible for cleaning up this memory, and the means of doing so will vary per implementation
  com_allocator_Persistent = 1<<1,
} com_allocator_Flag;

typedef u32 com_allocator_Flags;

// Handle to allocation, this remains constant over the allocation's life
typedef struct {
  usize _id;
  bool valid;
} com_allocator_Handle;

typedef struct  {
  com_allocator_Flags _default_flags;
  com_allocator_Flags _supported_flags;

  // Opaque pointer to reallocator backend
  void* _allocator_backing;

  // com_allocator_Handle allocate(void* backing, size_t size) 
  com_allocator_Handle (*allocator_fn)(void*, size_t);
  // com_allocator_Handle allocate(void* backing, size_t size, uint8_t alignment_power) 
  com_allocator_Handle (*allocator_flags_fn)(void*, size_t, com_allocator_Flags);
  // void deallocate(void* backing, MemHandle h) 
  void (*deallocator_fn)(void*, com_allocator_Handle);
  // com_allocator_Handle realloc(void* backing, com_allocator_Handle hndl, size_t size) 
  com_allocator_Handle(*reallocator_fn)(void*, com_allocator_Handle, size_t);
  // void* get(void* backing, com_allocator_Handle handle);
  void* (*get_fn)(void*, com_allocator_Handle);

  // void* destroy_allocator(void* backing)
  void (*destroy_allocator_fn)(void*);
} com_Allocator;

/** flags that are enabled by default for an allocator (cannot be disabled)
 * REQUIRES: `a` is a valid pointer to a com_Allocator
 * GUARANTEES: returns flags supported by default by `a`
 */
static inline com_allocator_Flags com_allocator_defaults(const com_Allocator * a) {
  return a->_supported_flags;
}

/** flags that are valid for `a` (may be enabled)
 * REQUIRES: `a` is a valid pointer to a com_Allocator
 * GUARANTEES: returns flags supported by default by `a`
 */
static inline com_allocator_Flags com_allocator_supports(const com_Allocator * a) {
  return a->_supported_flags;
}

/** allocate memory
 * REQUIRES: `a` is a valid pointer to a com_Allocator
 * GUARANTEES: if allocation succeeds, a valid com_allocator_Handle will be returned representing an allocation for `size` bytes of memory
 * GUARANTEES: if allocation fails, an invalid com_allocator_Handle will be returned
 * GUARANTEES: the memory will be allocated with default properties of the implementing allocator
 */
static inline com_allocator_Handle com_allocator_alloc(const com_Allocator * a, size_t size) {
  return a->allocator_fn(a->_allocator_backing, size);
}

/** allocate aligned memory
 * REQUIRES: `a` is a valid pointer to a com_Allocator
 * REQUIRES: all bits enabled in `flags` must be supported by `a`
 * GUARANTEES: if `size` is 0, NULL will be returned
 * GUARANTEES: if allocation succeeds, a pointer to `size` bytes of contiguous memory will be returned
 * GUARANTEES: if allocation succeeds, the memory returned will have the properties specified by the flags
 * GUARANTEES: if allocation fails, NULL will be returned
 */
static inline com_allocator_Handle com_allocator_alloc_flags(const com_Allocator * a, size_t size, com_allocator_Flags flags) {
  // guarantee all flags are supported by this allocator
  com_assert_m((com_allocator_supports(a) & flags) == flags, "some flags that were given are not supported by this allocator");
  return a->allocator_flags_fn(a->_allocator_backing, size, flags);
}

/** deallocate memory
 * REQUIRES: `a` is a valid pointer to a com_Allocator
 * REQUIRES: `handle` is a valid com_allocator_Handle returned by a previous call to `com_allocator_alloc`, `com_allocator_realloc`  or `com_allocator_alloc_flags` using `a`
 * GUARANTEES: the memory represented by `handle` will be freed if possible, with memory returning to the OS
 */
static inline void com_allocator_dealloc(const com_Allocator * a, com_allocator_Handle handle) {
  a->deallocator_fn(a->_allocator_backing, handle);
}

/** reallocate memory
 * REQUIRES: `a` is a valid pointer to a com_Allocator
 * REQUIRES: `handle` is a valid com_allocator_Handle returned by a previous call to `com_allocator_alloc_flags` using `a` with the `A_REALLOCABLE` bit enabled
 * GUARANTEES: if `size` is 0, the allocation represented by `handle` will be deallocated
 * GUARANTEES: if allocation succeeds, the `size` bytes of contiguous memory will be allocated, preserving data
 * GUARANTEES: if allocation succeeds, a possibly different com_allocator_Handle will be returned representing the new memory
 * GUARANTEES: if allocation succeeds, allocated memory will have the same properties as `ptr`
 * GUARANTEES: all pointers previously got from `handle` are invalidated
 * GUARANTEES: if the reallocation fails, an invalid com_allocator_Handle will be returned, but `handle` will still be valid
 */
static inline com_allocator_Handle com_allocator_realloc(const com_Allocator * a, com_allocator_Handle handle, size_t size) {
  com_assert_m(com_allocator_supports(a) & com_allocator_Reallocable, "this allocator does not support reallocation");
  return a->reallocator_fn(a->_allocator_backing, handle, size);
}

/** get memory
 * REQUIRES: `a` is a valid pointer to a com_Allocator
 * REQUIRES: `handle` is a valid com_allocator_Handle returned by a previous call to `com_allocator_alloc_flags`, `com_allocator_alloc`, or `com_allocator_realloc` using `a`
 * GUARANTEES: the returned value will be a pointer to the start of a block of contiguous memory represented by `handle`
 * GUARANTEES: this memory is valid till a subsequent call to `com_allocator_dealloc` or `com_allocator_realloc` with the `handle` as an argument
 * GUARANTEES: this memory may be invalidated if `a` is destroyed
 */
static inline void* com_allocator_get(const com_Allocator *a, com_allocator_Handle handle) {
  com_assert_m(handle.valid, "this handle is invalid");
  return a->get_fn(a->_allocator_backing, handle);
}

/** destroy allocator
 * REQUIRES: `a` is a valid pointer to a com_Allocator
 * GUARANTEES: `a` is no longer a valid pointer to a com_Allocator
 * GUARANTEES: all memory held by `a` is in an UNDEFINED state
 * NOT GUARANTEED: it is not guaranteed that all memory held by the allocator will be returned to the OS. it is implementation specific
 */
static inline void com_allocator_destroy(com_Allocator *a) {
  a->destroy_allocator_fn(a->_allocator_backing);
}

// Created this macro for type safety + convenience
#define ALLOC_ARR(allocator, n, type)                                             \
  ((type *)com_allocator_get(allocator, com_allocator_alloc((allocator), (n) * sizeof(type))))
#define ALLOC(allocator, type) ALLOC_ARR(allocator, 1, type)

#endif
