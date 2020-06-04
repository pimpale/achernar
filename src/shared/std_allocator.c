#include "std_allocator.h"

#include <stdbool.h>

#include "allocator.h"
#include "utils.h"

// normalize alloc behavior
static void *std_allocator_fn(void *backing, size_t size) {
  UNUSED(backing);
  if (size == 0) {
    return NULL;
  }
  return malloc(size);
}

static void *std_allocator_flags_fn(void *backing, size_t size,
                                    AllocatorFlags flags) {
  assert(flags == (A_REALLOCABLE | A_NO_CLEANUP_ON_DESTROY));
  return std_allocator_fn(backing, size);
}

static void std_deallocator_fn(void *backing, void *ptr) {
  UNUSED(backing);
  free(ptr);
}

// normalize realloc behavior
static void *std_reallocator_fn(void *backing, void *ptr, size_t size) {
  UNUSED(backing);
  if (size == 0) {
    return NULL;
  }
  return realloc(ptr, size);
}

static void std_destroy_allocator_fn(void *backing) {
  UNUSED(backing);
  // nothing
}

// Allocator constant struct variable
const Allocator std_allocator =
    (Allocator){// just use the system malloc and free funcs
                .default_flags = A_REALLOCABLE | A_NO_CLEANUP_ON_DESTROY,
                .supported_flags = A_REALLOCABLE | A_NO_CLEANUP_ON_DESTROY,
                // No state needs to be preserved
                .allocator_backing = NULL,
                // Set functions
                .allocator_fn = std_allocator_fn,
                .allocator_flags_fn = std_allocator_flags_fn,
                .deallocator_fn = std_deallocator_fn,
                .destroy_allocator_fn = std_destroy_allocator_fn};
