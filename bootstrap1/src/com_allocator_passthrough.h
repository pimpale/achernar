#ifndef COM_ALLOCATOR_PASSTHROUGH
#define COM_ALLOCATOR_PASSTHROUGH

// this allocator simply is a no-op that allows one to pass through preexisting pointers into a valid allocator handle
// You can only allocate once from the allocator you create, and it gives you back a handle to the memory you passed in initially
// Uses no heap

#include "com_allocator.h"

typedef struct  {
  // if the allocator has been destroyed or not
  bool _allocator_valid;
  // if the given input is still valid
  bool _input_valid;
  // if the given input has been used yet
  bool _input_used;
  // pointer from given input
  void* _input_ptr;
  // the original length provided
  usize _original_input_len;
  // the length that may be reallocated
  usize _input_len;
} com_allocator_passthrough_Backing;

/** Creates a passthrough allocator with space `ptr` and length `len`
 * REQUIRES: `ptr` is a valid ptr to at least `len` bytes of contiguous memory
 * REQUIRES: `backing_storage` is a valid pointer
 * REQUIRES: `backing_storage` must be stored at the same address for the duration of the allocator
 * REQUIRES: the returned allocator must not be called more than once
 * REQUIRES: the requested length of memory must not be greater than `len`
 * GUARANTEES: returns a valid allocator
 * GUARANTEES: `backing_storage` will be overwritten
 * GUARANTEES: on the first call to allocate memory, will return a handle representing `ptr` with length `len`
 */
com_Allocator com_allocator_passthrough(void* ptr, usize len, com_allocator_passthrough_Backing* backing_storage);

#endif

