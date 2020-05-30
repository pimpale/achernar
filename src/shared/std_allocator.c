#include "std_allocator.h"

#include <stdbool.h>

#include "allocator.h"

// normalize alloc behavior
static void *std_allocator_fn(void *backing, size_t size) {
  if (size == 0) {
    return NULL;
  }
  return malloc(size);
}

static void* std_allocator_flags_fn(void* backing, size_t size, AllocatorFlags flags) {
  assert(flags == (A_REALLOCABLE | A_NO_CLEANUP_ON_DESTROY));
  return std_allocator_fn(backing, size);
}

static void std_deallocator_fn(void *backing, void *ptr) { free(ptr); }

// normalize realloc behavior
static void *std_reallocator_fn(void *backing, void *ptr, size_t size) {
  if (size == 0) {
    return NULL;
  }
  return realloc(ptr, size);
}

static void std_destroy_allocator_fn(void *backing) {
  // nothing
}

void std_a_create(Allocator *allocator) {
  // no state needs to be preserved
  allocator->allocator_backing = NULL;
  // we can realloc, but aligned malloc is disabled (TODO add it)
  allocator->default_flags = A_REALLOCABLE | A_NO_CLEANUP_ON_DESTROY;
  allocator->supported_flags = A_REALLOCABLE | A_NO_CLEANUP_ON_DESTROY;
  // set functions
  allocator->allocator_fn = std_allocator_fn;
  allocator->allocator_flags_fn = std_allocator_flags_fn;
  allocator->deallocator_fn = std_deallocator_fn;
  allocator->reallocator_fn = std_reallocator_fn;
  allocator->destroy_allocator_fn = std_destroy_allocator_fn;
}
