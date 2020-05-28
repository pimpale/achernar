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
  allocator->realloc_possible = true;
  allocator->aligned_possible = false;
  // set functions
  allocator->allocator_fn = std_allocator_fn;
  allocator->deallocator_fn = std_deallocator_fn;
  allocator->reallocator_fn = std_reallocator_fn;
  allocator->destroy_allocator_fn = std_destroy_allocator_fn;
  // no support for aligned
  allocator->aligned_allocator_fn = NULL;
}
