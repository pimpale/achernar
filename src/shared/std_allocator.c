#include "std_allocator.h"

#include <stdbool.h>

#include "allocator.h"
#include "utils.h"

typedef struct {
  void* ptr;
  bool valid;
} AllocEntry;

typedef struct StdAllocator_s {
  AllocEntry* ptrs;
  size_t ptrs_len;
  size_t ptrs_cap;
} StdAllocator;

static size_t push_entry(StdAllocator* b,void* entry) {
  // The system has a null zero range
  assert(b->ptrs_cap > 0);
  // Ensure that there is enough room for the allocation 
  if(b->ptrs_len + 1 >= b->ptrs_cap) {
    b->ptrs = realloc(b->ptrs, b->ptrs_cap*2*sizeof(AllocEntry));
  }
  // Add to the top of the stack
  size_t index = b->ptrs_len;
  b->ptrs[index] = (AllocEntry){
      .valid=true,
      .ptr=entry,
  };
  // Increment the length pointer
  b->ptrs_len++;
  return index;
}

static inline MemHandle std_allocate(StdAllocator* backing, size_t size) {
  if (size == 0) {
    return (MemHandle) {.id={.id=0, .valid=false}, .ptr=NULL};
  }
  void* ptr = malloc(size);
  size_t index = push_entry(backing, ptr);
  return (MemHandle) {.id={.id=index,.valid=true}, .ptr=ptr};
}

static inline void std_deallocate(StdAllocator* backing, AllocId id) {
  assert(id.valid);
  AllocEntry *ae = &backing->ptrs[id.id];
  ae->valid=false;
  free(ae->ptr);
  ae->ptr= NULL;
}

static inline MemHandle std_reallocate(StdAllocator*backing, AllocId id, size_t size) {
  if (size == 0) {
    std_deallocate(backing, id);
    return (MemHandle) {.id={.id=0, .valid=false}, .ptr=NULL};
  }

  AllocEntry *ae = &backing->ptrs[id.id];
  void* ret = realloc(ae->ptr, size);
  assert(ret != NULL);
  ae->ptr = ret;
  return (MemHandle) {.id=id, .ptr=ret};
}

static inline StdAllocator* std_create() {
  StdAllocator* sa = malloc(sizeof(StdAllocator));

  // allocate once to form the standard allocator
  sa->ptrs = malloc(sizeof(AllocEntry));
  sa->ptrs_cap = 1;
  sa->ptrs_len = 0;
  return sa;
}

static inline void std_destroy(StdAllocator *backing) {
  // free all unfreed things
  for(size_t i = 0; i < backing->ptrs_len; i++) {
    if(backing->ptrs[i].valid) {
        free(backing->ptrs[i].ptr);
    }
  }
  // free the array
  free(backing->ptrs);
  // free the allocator itself
  free(backing);
}

// Shim methods
static MemHandle std_allocator_fn(void *backing, size_t size) {
    return std_allocate((StdAllocator*)backing, size);
}
static MemHandle std_allocator_flags_fn(void *backing, size_t size,
                                    AllocatorFlags flags) {
  return std_allocate(backing, size);
}

static void std_deallocator_fn(void *backing, AllocId id) {
    std_deallocate((StdAllocator*)backing, id);
}

// normalize realloc behavior
static MemHandle std_reallocator_fn(void *backing, AllocId id, size_t size) {
    return std_reallocate((StdAllocator*)backing, id, size);
}

static void std_destroy_allocator_fn(void *backing) {
    std_destroy((StdAllocator*)backing);
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
                     .destroy_allocator_fn = std_destroy_allocator_fn};
}
