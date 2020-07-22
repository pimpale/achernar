#include "com_allocator_os_noleak.h"
#include "com_assert.h"

typedef struct {
  void *ptr;
  bool valid;
} AllocEntry;

typedef struct StdAllocator_s {
  // this is essentially a vector of 
  AllocEntry *ptrs;
  usize ptrs_len;
  usize ptrs_cap;
} StdAllocator;

static usize push_entry(StdAllocator *b, void *entry) {
  // The system has a null zero range
  com_assert_m(b->ptrs_cap > 0, "internal allocator vector is corrupt");
  // Ensure that there is enough room for the allocation
  if (b->ptrs_len + 1 >= b->ptrs_cap) {
    b->ptrs_cap = b->ptrs_cap * 2;
    b->ptrs = realloc(b->ptrs, b->ptrs_cap * sizeof(AllocEntry));
  }
  // Add to the top of the stack
  usize index = b->ptrs_len;
  b->ptrs_len++;
  b->ptrs[index] = (AllocEntry){
      .valid = true,
      .ptr = entry,
  };
  // Increment the length pointer
  return index;
}

static inline com_allocator_Handle std_allocate(StdAllocator *backing, usize size) {
  if (size == 0) {
    return (com_allocator_Handle){._id = 0, .valid = true};
  }
  void *ptr = malloc(size);
  usize index = push_entry(backing, ptr);
  return (com_allocator_Handle){._id = index, .valid = true};
}

static inline void std_deallocate(StdAllocator *backing, com_allocator_Handle id) {
  com_assert_m(id.valid, "handle id is invalid");
  AllocEntry *ae = &backing->ptrs[id._id];
  ae->valid = false;
  free(ae->ptr);
}

// TODO errors??
static inline com_allocator_Handle std_reallocate(StdAllocator *backing, com_allocator_Handle id,
                                     usize size) {
  if (size == 0) {
    std_deallocate(backing, id);
    return (com_allocator_Handle){._id = 0, .valid = true};
  } else if (id._id == 0) {
    return std_allocate(backing, size);
  }

  AllocEntry *ae = &backing->ptrs[id._id];
  void *ret = realloc(ae->ptr, size);
  if(ret == NULL) {
    // realloc failed
    return (com_allocator_Handle) {.valid=false};
  }
  ae->ptr = ret;
  return (com_allocator_Handle){.id = id.id, .valid = true};
}

static inline void *std_get(StdAllocator *backing, com_allocator_Handle id) {
  com_assert_m(id.valid, "id is invalid");
  com_assert_m(id._id < backing->ptrs_len, "_id is out of bounds");
  return backing->ptrs[id._id].ptr;
}

static inline StdAllocator *std_create() {
  StdAllocator *sa = malloc(sizeof(StdAllocator));
  if(sa != NULL) {
    void* ptr = malloc(2 * sizeof(AllocEntry));
    if(ptr != NULL) {
      // allocate once to form the standard allocator
      sa->ptrs = ptr;
      sa->ptrs_cap = 2;
      sa->ptrs_len = 1;
      // the null pointer
      sa->ptrs[0] = (AllocEntry){.ptr = NULL, .valid = true};
    } else {
        // otherwise GCC complains
        assert(ptr != NULL);
    }
  } else {
    // helpful assertion to diagnose
    assert(sa != NULL);
  }
  return sa;
}

static inline void std_destroy(StdAllocator *backing) {
  // free all unfreed things
  for (usize i = 0; i < backing->ptrs_len; i++) {
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
static com_allocator_Handle std_allocator_fn(void *backing, usize size) {
  return std_allocate((StdAllocator *)backing, size);
}
static com_allocator_Handle std_allocator_flags_fn(void *backing, usize size,
                                       attr_UNUSED com_allocatorAllocatorFlags flags) {
  return std_allocate((StdAllocator*)backing, size);
}

static void std_deallocator_fn(void *backing, com_allocator_Handle id) {
  std_deallocate((StdAllocator *)backing, id);
}

// normalize realloc behavior
static com_allocator_Handle std_reallocator_fn(void *backing, com_allocator_Handle id, usize size) {
  return std_reallocate((StdAllocator *)backing, id, size);
}

static void *std_get_fn(void *backing, com_allocator_Handle id) {
  return std_get((StdAllocator *)backing, id);
}

static void std_destroy_allocator_fn(void *backing) {
  std_destroy((StdAllocator *)backing);
}

// Allocator constant struct variable
com_Allocator std_allocator(void) {
  return (com_Allocator){// just use the system malloc and free funcs
                     .default_flags = com_allocator_NOLEAK,
                     .supported_flags = com_allocator_NOLEAK | com_allocator_REALLOCABLE,
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
