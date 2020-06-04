#include "std_allocator.h"

#include <stdbool.h>

#include "allocator.h"
#include "utils.h"

typedef struct {
  void *ptr;
  bool valid;
} AllocEntry;

typedef struct StdAllocator_s {
  AllocEntry *ptrs;
  size_t ptrs_len;
  size_t ptrs_cap;
} StdAllocator;

static size_t push_entry(StdAllocator *b, void *entry) {
  // The system has a null zero range
  assert(b->ptrs_cap > 0);
  // Ensure that there is enough room for the allocation
  if (b->ptrs_len + 1 >= b->ptrs_cap) {
    b->ptrs_cap = b->ptrs_cap * 2;
    b->ptrs = realloc(b->ptrs, b->ptrs_cap * sizeof(AllocEntry));
  }
  // Add to the top of the stack
  size_t index = b->ptrs_len;
  b->ptrs_len++;
  b->ptrs[index] = (AllocEntry){
      .valid = true,
      .ptr = entry,
  };
  // Increment the length pointer
  return index;
}

static inline AllocId std_allocate(StdAllocator *backing, size_t size) {
  if (size == 0) {
    return (AllocId){.id = 0, .valid = true};
  }
  void *ptr = malloc(size);
  size_t index = push_entry(backing, ptr);
  return (AllocId){.id = index, .valid = true};
}

static inline void std_deallocate(StdAllocator *backing, AllocId id) {
  assert(id.valid);
  AllocEntry *ae = &backing->ptrs[id.id];
  ae->valid = false;
  free(ae->ptr);
}

// TODO errors??
static inline AllocId std_reallocate(StdAllocator *backing, AllocId id,
                                     size_t size) {
  if (size == 0) {
    std_deallocate(backing, id);
    return (AllocId){.id = 0, .valid = true};
  } else if (id.id == 0) {
    return std_allocate(backing, size);
  }

  AllocEntry *ae = &backing->ptrs[id.id];
  void *ret = realloc(ae->ptr, size);
  ae->ptr = ret;
  assert(ae->ptr != NULL);
  return (AllocId){.id = id.id, .valid = true};
}

static inline void *std_get(StdAllocator *backing, AllocId id) {
  assert(id.id < backing->ptrs_len);
  assert(id.valid);
  return backing->ptrs[id.id].ptr;
}

static inline StdAllocator *std_create() {
  StdAllocator *sa = malloc(sizeof(StdAllocator));

  // allocate once to form the standard allocator
  sa->ptrs = malloc(2 * sizeof(AllocEntry));
  sa->ptrs_cap = 2;
  sa->ptrs_len = 1;
  // the null pointer
  sa->ptrs[0] = (AllocEntry){.ptr = NULL, .valid = true};
  return sa;
}

static inline void std_destroy(StdAllocator *backing) {
  // free all unfreed things
  for (size_t i = 0; i < backing->ptrs_len; i++) {
    if (backing->ptrs[i].valid) {
      free(backing->ptrs[i].ptr);
    }
  }
  // free the array
  free(backing->ptrs);
  // free the allocator itself
  free(backing);
}

// Shim methods
static AllocId std_allocator_fn(void *backing, size_t size) {
  return std_allocate((StdAllocator *)backing, size);
}
static AllocId std_allocator_flags_fn(void *backing, size_t size,
                                      AllocatorFlags flags) {
  UNUSED(flags);
  return std_allocate(backing, size);
}

static void std_deallocator_fn(void *backing, AllocId id) {
  std_deallocate((StdAllocator *)backing, id);
}

// normalize realloc behavior
static AllocId std_reallocator_fn(void *backing, AllocId id, size_t size) {
  return std_reallocate((StdAllocator *)backing, id, size);
}

static void *std_get_fn(void *backing, AllocId id) {
  return std_get((StdAllocator *)backing, id);
}

static void std_destroy_allocator_fn(void *backing) {
  std_destroy((StdAllocator *)backing);
}

// Allocator constant struct variable
Allocator std_allocator(void) {
  return (Allocator){// just use the system malloc and free funcs
                     .default_flags = A_NOFLAGS,
                     .supported_flags = A_REALLOCABLE | A_NO_CLEANUP_ON_DESTROY,
                     // create the allocator
                     .allocator_backing = std_create(),
                     // Set functions
                     .allocator_fn = std_allocator_fn,
                     .allocator_flags_fn = std_allocator_flags_fn,
                     .deallocator_fn = std_deallocator_fn,
                     .reallocator_fn = std_reallocator_fn,
                     .get_fn = std_get_fn,
                     .destroy_allocator_fn = std_destroy_allocator_fn};
}
