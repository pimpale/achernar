#include "stdallocator.h"

#include <stdbool.h>

#include "allocator.h"

static void* std_allocator_fn(void* backing, size_t size) {
  return malloc(size);
}

static void std_deallocator_fn(void* backing, void* ptr) {
  free(ptr);
}

static void* std_reallocator_fn(void* backing, void* ptr, size_t size) {
  return realloc(ptr, size);
}

static void std_destroy_allocator_fn(void* backing) {
  //nothing
}

void std_a_create(Allocator* allocator) {
  allocator->allocator_backing = NULL;
  allocator->realloc_possible = true;
  allocator->allocator_fn = std_allocator_fn;
  allocator->deallocator_fn = std_deallocator_fn;
  allocator->reallocator_fn = std_reallocator_fn;
  allocator->destroy_allocator_fn = std_destroy_allocator_fn;
}
