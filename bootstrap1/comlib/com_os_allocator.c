#include "com_os_allocator.h"
#include "com_assert.h"

typedef struct {
  void *ptr;
  bool valid;
  com_allocator_HandleData data;
} AllocEntry;

typedef struct StdAllocator_s {
  // this is essentially a vector of
  AllocEntry *ptrs;
  usize ptrs_len;
  usize ptrs_cap;
} StdAllocator;

static usize push_entry(StdAllocator *b, void *entry, com_allocator_HandleData data) {
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
      .data = data,
  };
  // Increment the length pointer
  return index;
}

static inline StdAllocator *std_create() {
  StdAllocator *sa = malloc(sizeof(StdAllocator));
  com_assert_m(sa != NULL, "failed to allocate StdAllocator object");

  void *ptr = malloc(2 * sizeof(AllocEntry));
  com_assert_m(ptr != NULL, "failed to allocate internal StdAllocator vector");

  // allocate once to form the standard allocator
  sa->ptrs = ptr;
  sa->ptrs_cap = 2;
  sa->ptrs_len = 1;
  // the null pointer
  sa->ptrs[0] = (AllocEntry){.ptr = NULL, .valid = true};
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
static com_allocator_Handle std_allocator_fn(const com_Allocator *allocator,
                                             com_allocator_HandleData data) {
  StdAllocator *backing = allocator->_backing;

  if (data.len == 0) {
    // return the dummy null entry
    return (com_allocator_Handle){._id = 0, .valid = true};
  }

  void *ptr = malloc(data.len);
  usize index = push_entry(backing, ptr, data);
  return (com_allocator_Handle){._id = index, .valid = true};
}

static void std_deallocator_fn(com_allocator_Handle id) {
  StdAllocator *backing = id._allocator->_backing;
  com_assert_m(id._id < backing->ptrs_len, "_id is out of bounds");
  AllocEntry *ae = &backing->ptrs[id._id];
  ae->valid = false;
  free(ae->ptr);
}

// normalize realloc behavior
static com_allocator_Handle std_reallocator_fn(com_allocator_Handle id,
                                               usize size) {
  StdAllocator *backing = id._allocator->_backing;

  // deallocate if we have resized to zero bytes
  if (size == 0) {
    std_deallocator_fn(id);
    return (com_allocator_Handle){._id = 0, .valid = true};
  } 

	// if it was a dummy pointer then we just allocate
  if (id._id == 0) {
    return std_allocator_fn(
        id._allocator,
        (com_allocator_HandleData){
            .len = size, 
            .flags = com_allocator_defaults(id._allocator)
        });
  }

  AllocEntry *ae = &backing->ptrs[id._id];
  void *ret = realloc(ae->ptr, size);
  if (ret == NULL) {
    // realloc failed, old handle is good though
    return (com_allocator_Handle){.valid = false};
  }

	// update alloc entry
  ae->ptr = ret;
  ae->data.len = size;

	// return valid handle
  return (com_allocator_Handle){._id = id._id, .valid = true};
}

static void *std_get_fn(const com_allocator_Handle id) {
  StdAllocator *backing = id._allocator->_backing;
  com_assert_m(id._id < backing->ptrs_len, "_id is out of bounds");
  return backing->ptrs[id._id].ptr;
}

static com_allocator_HandleData 
std_query_fn(const com_allocator_Handle id) {
  StdAllocator *backing = id._allocator->_backing;
  com_assert_m(id._id < backing->ptrs_len, "_id is out of bounds");
  return backing->ptrs[id._id].data;
}

static void std_destroy_allocator_fn(com_Allocator *allocator) {
  std_destroy((StdAllocator *)allocator->_backing);
}

// Allocator constant struct variable
com_Allocator std_allocator(void) {
  return (com_Allocator){._valid = true,
                         ._default_flags = com_allocator_NOLEAK,
                         ._supported_flags =
                             com_allocator_NOLEAK | com_allocator_REALLOCABLE,
                         // create the allocator
                         ._backing = std_create(),
                         // Set functions
                         ._allocator_fn = std_allocator_fn,
                         ._deallocator_fn = std_deallocator_fn,
                         ._reallocator_fn = std_reallocator_fn,
                         ._get_fn = std_get_fn,
                         ._query_fn = std_query_fn,
                         ._destroy_allocator_fn = std_destroy_allocator_fn};
}
