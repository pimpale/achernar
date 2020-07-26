#include "com_allocator_os.h"
#include "com_os_mem_alloc.h"
#include "com_assert.h"

typedef struct {
  void *ptr;
  bool valid;
  com_allocator_HandleData data;
} AllocEntry;

typedef struct Stdallocator_s {
  // this is essentially a vector of
  AllocEntry *ptrs;
  usize ptrs_len;
  usize ptrs_cap;
} Stdallocator;

static usize push_entry(Stdallocator *b, void *entry, com_allocator_HandleData data) {
  // The system has a null zero range
  com_assert_m(b->ptrs_cap > 0, "internal allocator vector is corrupt");
  // Ensure that there is enough room for the allocation
  if (b->ptrs_len + 1 >= b->ptrs_cap) {
    b->ptrs_cap = b->ptrs_cap * 2;
    b->ptrs = com_os_mem_realloc(b->ptrs, b->ptrs_cap * sizeof(AllocEntry));
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

static Stdallocator *std_create() {
  Stdallocator *sa = com_os_mem_alloc(sizeof(Stdallocator));
  com_assert_m(sa != NULL, "failed to allocate Stdallocator object");

  void *ptr = com_os_mem_alloc(2 * sizeof(AllocEntry));
  com_assert_m(ptr != NULL, "failed to allocate internal Stdallocator vector");

  // allocate once to form the standard allocator
  sa->ptrs = ptr;
  sa->ptrs_cap = 2;
  sa->ptrs_len = 1;
  // the null pointer
  sa->ptrs[0] = (AllocEntry){.ptr = NULL, .valid = true};
  return sa;
}

static  void std_destroy(Stdallocator *backing) {
  // com_os_mem_dealloc all uncom_os_mem_deallocd things
  for (usize i = 0; i < backing->ptrs_len; i++) {
    if (backing->ptrs[i].valid) {
      com_os_mem_dealloc(backing->ptrs[i].ptr);
    }
  }
  // com_os_mem_dealloc the array
  com_os_mem_dealloc(backing->ptrs);
  // com_os_mem_dealloc the allocator itself
  com_os_mem_dealloc(backing);
}

// Shim methods
static com_allocator_Handle std_allocator_fn(const com_allocator *allocator,
                                             com_allocator_HandleData data) {
  Stdallocator *backing = allocator->_backing;

  if (data.len == 0) {
    // return the dummy null entry
    return (com_allocator_Handle){._id = 0, .valid = true};
  }

  void *ptr = com_os_mem_alloc(data.len);
  usize index = push_entry(backing, ptr, data);
  return (com_allocator_Handle){._id = index, .valid = true};
}

static void std_deallocator_fn(com_allocator_Handle id) {
  Stdallocator *backing = id._allocator->_backing;
  com_assert_m(id._id < backing->ptrs_len, "_id is out of bounds");
  AllocEntry *ae = &backing->ptrs[id._id];
  ae->valid = false;
  com_os_mem_dealloc(ae->ptr);
}

// normalize realloc behavior
static com_allocator_Handle std_reallocator_fn(com_allocator_Handle id,
                                               usize size) {
  Stdallocator *backing = id._allocator->_backing;

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
  void *ret = com_os_mem_realloc(ae->ptr, size);
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
  Stdallocator *backing = id._allocator->_backing;
  com_assert_m(id._id < backing->ptrs_len, "_id is out of bounds");
  return backing->ptrs[id._id].ptr;
}

static com_allocator_HandleData 
std_query_fn(const com_allocator_Handle id) {
  Stdallocator *backing = id._allocator->_backing;
  com_assert_m(id._id < backing->ptrs_len, "_id is out of bounds");
  return backing->ptrs[id._id].data;
}

static void std_destroy_allocator_fn(com_allocator *allocator) {
  std_destroy((Stdallocator *)allocator->_backing);
}

// allocator constant struct variable
com_allocator std_allocator(void) {
  return (com_allocator){._valid = true,
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
