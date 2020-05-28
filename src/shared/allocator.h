#ifndef ALLOCATOR_H
#define ALLOCATOR_H

#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

// Dynamic allocator for fun

typedef struct Allocator_s {
  // is realloc valid?
  bool realloc_possible;

  // Opaque pointer to reallocator backend
  void* allocator_backing;

  // void* allocate(void* backing, size_t size) 
  void *(*allocator_fn)(void*, size_t);
  // void* deallocate(void* backing, void* ptr) 
  void (*deallocator_fn)(void*, void*);
  // void* realloc(void* backing, void* ptr, size_t size) 
  void *(*reallocator_fn)(void*, void*, size_t);
  // void* destroy_allocator(void* backing)
  void (*destroy_allocator_fn)(void*);
} Allocator;

// hopefully inlined function to call an allocator (sadly still a tad of overhead)
static inline void* a_alloc(Allocator* a, size_t size) {
  return a->allocator_fn(a->allocator_backing, size);
}

// REQUIRES: `a` is a valid pointer to an Allocator
// REQUIRES: `ptr` is a memory location returned by a previous call to `a`
// GUARANTEES: `ptr` will be freed if possible, with memory returning to the OS
static inline void a_dealloc(Allocator* a, void* ptr) {
  a->deallocator_fn(a->allocator_backing, ptr);
}

static inline void* a_realloc(Allocator* a, void* ptr, size_t size) {
  assert(a->realloc_possible);
  return a->reallocator_fn(a->allocator_backing, ptr, size);
}

// REQUIRES: `a` is a valid pointer to an Allocator
// GUARANTEES: `a` is no longer a valid pointer to an Allocator
// GUARANTEES: all memory held by `a` is in an UNDEFINED state
// GUARANTEES: depending on the allocator backing, memory may or may not be freed
static inline void a_destroy(Allocator *a) {
  a->destroy_allocator_fn(a->allocator_backing);
}

// REQUIRES: `a` is a valid pointer to an Allocator
// GUARANTEES: returns if `a` supports reallocating pointers
static inline bool a_realloc_possible(Allocator* a) {
  return a->realloc_possible;
}

#endif
