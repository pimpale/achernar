#include <stdlib.h>
#include <string.h>

#include "arena_allocator.h"
#include "error.h"
#include "std_allocator.h"
#include "utils.h"
#include "vector.h"

#define DEFAULT_PAGE_SIZE 4095

typedef struct ArenaPage_s {
  void *data;      // pointer to page
  size_t length;   // amount of page currently used
  size_t capacity; // The capacity of this page
} ArenaPage;

/// Initializes a new empty arena page with the given size
/// REQUIRES: `mem` is a pointer to at least sizeof(ArenaPage) bytes of
///            valid memory
/// REQUIRES: `alignment` is a power of two
/// REQUIRES: `capacity` is a multiple of `alignment`
/// GUARANTEES: `mem` has been initialized to a valid ArenaPage
/// GUARANTEES: the ArenaPage has a capacity of at least `capacity`
/// GUARANTEES: returns `mem`
/// GUARANTEES: the ArenaPage's memory will be a multiple of `alignment`
static ArenaPage *createArenaPage(ArenaPage *mem, size_t capacity,
                                  size_t alignment);

/// Releases all memory associated with this arena page
/// REQUIRES: `app` is  pointer to a valid ArenaPage
/// REQUIRES: no pointers to the memory held by `app` are in use
/// GUARANTEES: all memory held by `app` is deallocated
/// GUARANTEES: `app` is no longer a valid ArenaPage
/// GUARANTEES: returns `app`
static ArenaPage *destroyArenaPage(ArenaPage *app);

/// True if there is sufficient space in this arena page for `len`
/// bytes of new memory.
/// REQUIRES: `app` is pointer to a valid ArenaPage
/// GUARANTEES: returns true if allocating `len` bytes from `app` would
///             not cause an overflow
static bool canFitArenaPage(ArenaPage *app, size_t len);

/// Allocates `len` bytes of data from `app`
/// REQUIRES: `app` is a pointer to a valid ArenaPage
/// REQUIRES: allocating `len` bytes from `app` would not cause an overflow
///           (this may be checked using `canFitArenaPage`)
/// GUARANTEES: returns a pointer to exactly `len` bytes of data allocated
///             from `app`
static void *allocArenaPage(ArenaPage *app, size_t len);

ArenaPage *createArenaPage(ArenaPage *mem, size_t capacity, size_t alignment) {
  // TODO replace aligned_alloc with malloc
  mem->data = aligned_alloc(alignment, capacity);
  mem->capacity = capacity;
  mem->length = 0;
  return mem;
}

// TODO write description
ArenaPage *fromMemoryArenaPage(ArenaPage *mem, void* ptr) {
    mem->data = ptr;
    mem->capacity = 0;
    mem->length = 0;
    return mem;
}

ArenaPage *destroyArenaPage(ArenaPage *app) {
  free(app->data);
  app->capacity = 0;
  app->length = 0;
  return app;
}

bool canFitArenaPage(ArenaPage *app, size_t len) {
  return app->length + len < app->capacity;
}

void *allocArenaPage(ArenaPage *app, size_t len) {
  uint8_t *dest = app->data;
  dest += app->length;
  app->length += len;
  return dest;
}
typedef struct Arena_s {
  // indices for aligned pages
  // Vector<int64_t> (if the int64_t is negative, then it means that there are
  // no pages allocated for that alignment)
  Vector indices;
  // Vector<ArenaPage>
  Vector pages;
} Arena;

/// Creates a arena in a region of a memory
/// REQUIRES: `mem` is a pointer to at least sizeof(Arena) bytes
/// GUARANTEES: `mem` has been initialized to a valid Arena
/// GUARANTEES: return value is `mem`
static Arena *ar_create(Arena *mem) {
  // initialize vectors
  mem->pages = vec_createOptions(&std_allocator, 0, A_REALLOCABLE | A_NO_CLEANUP_ON_DESTROY);
  mem->indices = vec_createOptions(&std_allocator, 0, A_REALLOCABLE | A_NO_CLEANUP_ON_DESTROY);
  return mem;
}

/// Destroys the arena and frees all memory associated with it
/// REQUIRES: `ar` is a pointer to a valid Arena
/// GUARANTEES: `ar` is no longer a valid Arena
/// GUARANTEES: all memory held by `ar` is deallocated
static Arena *ar_destroy(Arena *ar) {
  for (size_t i = 0; i < VEC_LEN(&ar->pages, ArenaPage); i++) {
    destroyArenaPage(VEC_GET(&ar->pages, i, ArenaPage));
  }
  vec_destroy(&ar->pages);
  vec_destroy(&ar->indices);
  return ar;
}

/** allocate aligned memory
 * REQUIRES: `a` is a valid pointer to an Allocator
 * GUARANTEES: if `size` is 0, NULL will be returned
 * GUARANTEES: if allocation succeeds, a pointer to `size` bytes of contiguous
 * memory will be returned, this pointer will be a multiple of the
 * 2^alignment_power GUARANTEES: if allocation fails, NULL will be returned
 */
inline static void *ar_alloc_aligned(Arena *ar, size_t len,
                                     uint8_t alignment_power) {
  // get alignment
  size_t alignment = 1 << alignment_power;
  // ensure len is a multiple
  len = roundToMultipleofTwo(len, alignment);

  // if len is larger than the default page size, we create a page specially
  // allocated without touching the currently active page
  if (len >= DEFAULT_PAGE_SIZE) {
    ArenaPage *newPage = VEC_PUSH(&ar->pages, ArenaPage);
    createArenaPage(newPage, len, alignment);
    return allocArenaPage(newPage, len);
  }

  size_t len_indices = VEC_LEN(&ar->indices, int64_t);
  for (size_t i = len_indices; i <= alignment_power; i++) {
    // instantiate the array up to where we want
    *VEC_PUSH(&ar->indices, int64_t) = -1;
  }

  // pointer to index (could be -1)
  int64_t *index_ptr = VEC_GET(&ar->indices, alignment_power, int64_t);

  // if a page for this pointer doesn't yet exist, create it
  if (*index_ptr == -1) {
    ArenaPage *newPage = VEC_PUSH(&ar->pages, ArenaPage);
    createArenaPage(newPage, DEFAULT_PAGE_SIZE, alignment);
    *index_ptr = 0;
  }

  assert(*index_ptr >= 0);
  uint64_t index = (uint64_t) *index_ptr;
  ArenaPage *a = VEC_GET(&ar->pages, index, ArenaPage);

  if (!canFitArenaPage(a, len)) {
    a = VEC_PUSH(&ar->pages, ArenaPage);
    createArenaPage(a, DEFAULT_PAGE_SIZE, alignment);
    *index_ptr = VEC_LEN(&ar->pages, ArenaPage) - 1;
  }

  return allocArenaPage(a, len);
}

static void *ar_allocator_flags_fn(void *backing, size_t len, AllocatorFlags flags) {
  Arena *ar = backing;
  assert(ar != NULL);

  if (len == 0) {
    return NULL;
  }

  // If it's reallocable it CANNOT be aligned, as realloc does not guarantee
  // alignment solid TODO right here: write own aligned_realloc function to take
  // care of this limitation
  assert(!((flags & A_REALLOCABLE) &&
           (flags & A_ALIGN_4 || flags & A_ALIGN_8 || flags & A_ALIGN_16)));

  // If they want aligned memory, go to other method...
  if (flags & A_ALIGN_16) {
    return ar_alloc_aligned(backing, len, 4);
  } else if (flags & A_ALIGN_8) {
    return ar_alloc_aligned(backing, len, 3);
  } else if (flags & A_ALIGN_4) {
    return ar_alloc_aligned(backing, len, 2);
  } else if (flags & A_REALLOCABLE) {
    // If reallocable is requested, we need to give it its own page
    ArenaPage *newPage = VEC_PUSH(&ar->pages, ArenaPage);
    fromMemoryArenaPage(newPage, malloc(len));
    return allocArenaPage(newPage, len);
  } else {
    // If it's not reallocable, then we can allocate it out of the normal pool
    return ar_alloc_aligned(backing, len, 0);
  }
}

// Shim method
static void *ar_allocator_fn(void *backing, size_t len) {
  // TODO fast path?
  return ar_allocator_flags_fn(backing, len, A_NOFLAGS);
}

// no op (memory can only be freed once the whole arena is deallocated)
static void ar_deallocator_fn(void *backing, void *ptr) {
  UNUSED(backing);
  UNUSED(ptr);
}

// reallocates memory for memory allocated with A_REALLOCABLE bit
static void* ar_reallocator_fn(void* backing, void* ptr, size_t len) {
    UNUSED(backing);
  return realloc(ptr, len);
}

/// Releases resources associated with arena
static void ar_destroy_allocator_fn(void *backing) {
  Arena *a = (Arena *)backing;
  // destroy backing arena
  ar_destroy(a);
  // free memory holding arena
  free(backing);
}

Allocator arena_a_create() {
  Allocator allocator;
  // create arena as backing
  allocator.allocator_backing = malloc(sizeof(Arena));
  ar_create(allocator.allocator_backing);
  // realloc is disabled but aligned is enabled
  allocator.default_flags = 0;
  allocator.supported_flags =
      A_ALIGN_4 | A_ALIGN_8 | A_ALIGN_16 | A_REALLOCABLE;

  // set functions
  allocator.allocator_fn = ar_allocator_fn;
  allocator.allocator_flags_fn = ar_allocator_flags_fn;
  allocator.deallocator_fn = ar_deallocator_fn;
  allocator.destroy_allocator_fn = ar_destroy_allocator_fn;
  // ignore realloc
  allocator.reallocator_fn = ar_reallocator_fn;
  return allocator;
}
