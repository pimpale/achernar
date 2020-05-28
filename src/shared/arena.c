#include <stdlib.h>
#include <string.h>

#include "arena.h"
#include "stdallocator.h"
#include "error.h"
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
  mem->data = aligned_alloc(alignment, capacity);
  mem->capacity = capacity;
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

// returns `value` rounded up to the nearest multiple of `multiple`
// REQUIRES: multiple is a power of two
// GUARANTEES: return value is a multiple of `roundTo` and >= value
static inline size_t roundTo(size_t value, size_t roundTo) {
  return (value + (roundTo - 1)) & ~(roundTo - 1);
}

typedef struct Arena_s {
  Allocator vec_allocator;
  // indices for aligned pages 
  // Vector<int64_t> (if the int64_t is negative, then it means that there are no pages allocated for that alignment)
  Vector indices;
  // Vector<ArenaPage>
  Vector pages;
} Arena;


/// Creates a arena in a region of a memory
/// REQUIRES: `mem` is a pointer to at least sizeof(Arena) bytes
/// GUARANTEES: `mem` has been initialized to a valid Arena
/// GUARANTEES: return value is `mem`
Arena *ar_create(Arena *mem) {
  // create allocator
  std_a_create(&mem->vec_allocator);
  // initialize vectors
  vec_create(&mem->pages, &mem->vec_allocator);
  vec_create(&mem->indices, &mem->vec_allocator);
  return mem;
}

/// Destroys the arena and frees all memory associated with it
/// REQUIRES: `ar` is a pointer to a valid Arena
/// GUARANTEES: `ar` is no longer a valid Arena
/// GUARANTEES: all memory held by `ar` is deallocated
Arena *ar_destroy(Arena *ar) {
  for (size_t i = 0; i < VEC_LEN(&ar->pages, ArenaPage); i++) {
    destroyArenaPage(VEC_GET(&ar->pages, i, ArenaPage));
  }
  vec_destroy(&ar->pages);
  vec_destroy(&ar->indices);
  a_destroy(&ar->vec_allocator);
  return ar;
}


/// Allocates `len` bytes from `ar`, aligned to the specified allocation
/// REQUIRES: `ar` is a pointer to a valid Arena
/// REQUIRES: `alignment` is one of 1, 2, 4, 8, or 16
/// REQUIRES: `len` is a multiple of `alignment`
/// GUARANTEES: return contains pointer to valid section of memory `len` bytes
/// long GUARANTEES: if `len` is 0, no memory will be allocated, NULL will be
/// returned GUARANTEES: the returned pointer will be aligned to `alignment`
void *ar_a_alloc_aligned_fn(void *ar, size_t len, uint8_t alignment_power, bool* failed) {
  assert(ar != NULL);

  if (len == 0) {
    return NULL;

  }

  // if len is larger than the default page size, we create a page specially
  // allocated without touching the currently active page
  if (len >= DEFAULT_PAGE_SIZE) {
    ArenaPage *newPage = VEC_PUSH(&ar->pages, ArenaPage);
    createArenaPage(newPage, len, alignment);
    return allocArenaPage(newPage, len);
  }

  size_t *index_ptr;
  switch (alignment) {
  case 1: {
    index_ptr = &ar->current_1_index;
    break;
  }
  case 2: {
    index_ptr = &ar->current_2_index;
    break;
  }
  case 4: {
    index_ptr = &ar->current_4_index;
    break;
  }
  case 8: {
    index_ptr = &ar->current_8_index;
    break;
  }
  case 16: {
    index_ptr = &ar->current_16_index;
    break;
  }
  default: {
    INTERNAL_ERROR("alignment is not one of 1, 2, 4, 8, or 16");
    PANIC();
  }
  }

  ArenaPage *a = VEC_GET(&ar->pages, *index_ptr, ArenaPage);

  if (!canFitArenaPage(a, len)) {
    a = VEC_PUSH(&ar->pages, ArenaPage);
    size_t pageCapacity = DEFAULT_PAGE_SIZE;
    createArenaPage(a, pageCapacity, alignment);
    *index_ptr = VEC_LEN(&ar->pages, ArenaPage) - 1;
  }

  if(failed != NULL)
  {
      if(len != 0 && ret == NULL) {
          *failed = true;
      }else {
          *failed = false;
      }
  }
  return ret;

  return allocArenaPage(a, len);
}

/// Allocates `len` bytes from `ar`. This memory cannot be freed or reallocated
/// REQUIRES: `ar` is a pointer to a valid Arena
/// GUARANTEES: return contains pointer to valid section of memory `len` bytes long 
/// GUARANTEES: if `len` is 0, no memory will be allocated, NULL will be returned 
/// GUARANTEES: if len is greater than 4, the pointer returned is 8-byte aligned 
/// GUARANTEES: if len is greater than or equal to 2, the pointer returned is 4-byte aligned
void *ar_a_allocator_fn(void *backing, size_t len, bool *failed) {
  Arena* ar = backing;
  if (len > 4) {
    return ar_alloc_aligned(ar, , 3);
  } else if (len > 2) {
    return ar_alloc_aligned(ar, roundTo(len, 4), 2);
  } else {
    return ar_alloc_aligned(ar, len, 0);
  }
}

void ar_a_destroy_allocator_fn(void* backing) {
  Arena* a = (Arena*)backing;
  // destroy backing arena
  ar_destroy(a);
  // free memory holding arena
  free(backing);
}

void ar_a_create(Allocator* allocator) {
  // create arena as backing
  allocator->allocator_backing = malloc(sizeof(Arena));
  ar_create(allocator->allocator_backing);
  // realloc is disabled but aligned is enabled
  allocator->realloc_possible = false;
  allocator->aligned_possible = true;

  // set functions


}
