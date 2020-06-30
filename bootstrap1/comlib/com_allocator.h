#ifndef COM_ALLOCATOR_H
#define COM_ALLOCATOR_H

#include "com_define.h"

typedef enum {
  com_allocator_FLAGS_NONE = 0,
  /// lensize of this memory allocation can be grown by a call to `com_allocator_realloc`
  com_allocator_REALLOCABLE = 1<<0,
  /// The memory will NOT be cleaned up when the allocator is destroyed (can cause resource leak)
  /// You are responsible for cleaning up this memory, and the means of doing so will vary per implementation
  com_allocator_PERSISTENT = 1<<1,
  /// specifically should align memory to sixteen bytes 
  /// (the max value necessary for floats. If disabled, then alignment is undefined)
  com_allocator_ALIGNED_16 = 1<<2,
} com_allocator_Flag;

typedef u32 com_allocator_Flags;

// forward declare com_Allocator
typedef struct com_Allocator_s com_Allocator;

// Handle to allocation, this remains constant over the allocation's life
typedef struct {
  const com_Allocator* _allocator;
  usize _id;
  bool valid;
} com_allocator_Handle;

// data associated with a handle
typedef struct {
  usize len;
  com_allocator_Flags flags;
} com_allocator_HandleData;

typedef struct com_Allocator_s {
  bool _valid;

  com_allocator_Flags _default_flags;
  com_allocator_Flags _supported_flags;

  // Opaque pointer to reallocator backend
  void* _backing;

  // com_allocator_Handle allocate(void* backing, handleData options) 
  com_allocator_Handle(*_allocator_fn)(const com_Allocator*, com_allocator_HandleData);
  // void deallocate(void* backing, MemHandle h) 
  void (*_deallocator_fn)(const com_allocator_Handle);
  // com_allocator_Handle realloc(void* backing, com_allocator_Handle hndl, usize len) 
  com_allocator_Handle(*_reallocator_fn)(com_allocator_Handle, usize);
  // void* get(void* backing, com_allocator_Handle handle);
  void* (*_get_fn)(const com_allocator_Handle);
  // void* query(void* backing, com_allocator_Handle handle);
  com_allocator_HandleData (*_query_fn)(const com_allocator_Handle);

  // void* destroy_allocator(void* backing)
  void (*_destroy_allocator_fn)(com_Allocator*);
} com_Allocator;

/** flags that are enabled by default for an allocator (cannot be disabled)
 * REQUIRES: `a` is a valid pointer to a com_Allocator
 * GUARANTEES: returns flags supported by default by `a`
 */
com_allocator_Flags com_allocator_defaults(const com_Allocator * a);

/** flags that are valid for `a` (may be enabled)
 * REQUIRES: `a` is a valid pointer to a com_Allocator
 * GUARANTEES: returns flags supported by default by `a`
 */
com_allocator_Flags com_allocator_supports(const com_Allocator * a);

/** allocate memory
 * REQUIRES: `a` is a valid pointer to a com_Allocator
 * GUARANTEES: if allocation succeeds, a valid com_allocator_Handle will be returned representing an allocation for `data.len` bytes of memory
 * GUARANTEES: if allocation succeeds, the memory returned will have the properties specified by the `data.flags`
 * GUARANTEES: if allocation fails, an invalid com_allocator_Handle will be returned
 * GUARANTEES: the memory will be allocated with default properties of the implementing allocator
 */
com_allocator_Handle com_allocator_alloc(const com_Allocator *a, com_allocator_HandleData data) ;

/** deallocate memory
 * REQUIRES: `a` is a valid pointer to a com_Allocator
 * REQUIRES: `handle` is a valid com_allocator_Handle
 * GUARANTEES: the memory represented by `handle` will be freed if possible, with memory returning to the OS
 */
void com_allocator_dealloc(com_allocator_Handle handle);

/** reallocate memory
 * REQUIRES: `a` is a valid pointer to a com_Allocator
 * REQUIRES: `handle` is a valid com_allocator_Handle returned by a previous call to `com_allocator_alloc_flags` using `a` with the `A_REALLOCABLE` bit enabled
 * GUARANTEES: if `len` is 0, the allocation represented by `handle` will be deallocated
 * GUARANTEES: if allocation succeeds, the `len` bytes of contiguous memory will be allocated, preserving data
 * GUARANTEES: if allocation succeeds, a possibly different com_allocator_Handle will be returned representing the new memory
 * GUARANTEES: if allocation succeeds, allocated memory will have the same properties as `ptr`
 * GUARANTEES: all pointers previously got from `handle` are invalidated
 * GUARANTEES: if the reallocation fails, an invalid com_allocator_Handle will be returned, but `handle` will still be valid
 */
com_allocator_Handle com_allocator_realloc(com_allocator_Handle handle, usize len);


/** query info from handle
 * REQUIRES: `handle` is a valid com_allocator_Handle
 * GUARANTEES: returns the data associated with `handle` 
 */
com_allocator_HandleData com_allocator_handle_query(com_allocator_Handle handle);


/** get memory from handle
 * REQUIRES: `handle` is a valid com_allocator_Handle
 * GUARANTEES: the returned value will be a pointer to the start of a block of contiguous memory represented by `handle`
 * GUARANTEES: this memory is valid till a subsequent call to `com_allocator_dealloc` or `com_allocator_realloc` with the `handle` as an argument
 * GUARANTEES: this memory may be invalidated if the allocator used to create `handle` is destroyed
 */
void* com_allocator_handle_get(com_allocator_Handle handle);


/** destroy allocator
 * REQUIRES: `a` is a valid pointer to a com_Allocator
 * GUARANTEES: `a` is no longer a valid pointer to a com_Allocator
 * GUARANTEES: all memory held by `a` is in an UNDEFINED state
 * NOT GUARANTEED: it is not guaranteed that all memory held by the allocator will be returned to the OS. it is implementation specific
 */
void com_allocator_destroy(com_Allocator *a);

// Created this macro for type safety + convenience
#define ALLOC_ARR(allocator, n, type)                                             \
  ((type *)com_allocator_get(allocator, com_allocator_alloc((allocator), (n) lensizeof(type))))
#define ALLOC(allocator, type) ALLOC_ARR(allocator, 1, type)

#endif
