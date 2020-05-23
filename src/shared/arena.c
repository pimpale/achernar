#include <stdlib.h>
#include <string.h>

#include "arena.h"
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

Arena *createArena(Arena *mem) {
  // initialize vector
  createVector(&mem->pages);
  // Push pages
  createArenaPage(VEC_PUSH(&mem->pages, ArenaPage), DEFAULT_PAGE_SIZE, 1);
  createArenaPage(VEC_PUSH(&mem->pages, ArenaPage), DEFAULT_PAGE_SIZE, 2);
  createArenaPage(VEC_PUSH(&mem->pages, ArenaPage), DEFAULT_PAGE_SIZE, 4);
  createArenaPage(VEC_PUSH(&mem->pages, ArenaPage), DEFAULT_PAGE_SIZE, 8);
  createArenaPage(VEC_PUSH(&mem->pages, ArenaPage), DEFAULT_PAGE_SIZE, 16);
  mem->current_1_index = 0;
  mem->current_2_index = 1;
  mem->current_4_index = 2;
  mem->current_8_index = 3;
  mem->current_16_index = 4;
  return mem;
}

Arena *destroyArena(Arena *ar) {
  for (size_t i = 0; i < VEC_LEN(&ar->pages, ArenaPage); i++) {
    destroyArenaPage(VEC_GET(&ar->pages, i, ArenaPage));
  }
  destroyVector(&ar->pages);
  return ar;
}

void *allocAlignedArena(Arena *ar, size_t len, size_t alignment) {
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

  return allocArenaPage(a, len);
}

void *allocArena(Arena *ar, size_t len) {
  if (len > 4) {
    return allocAlignedArena(ar, roundTo(len, 8), 8);
  } else if (len > 2) {
    return allocAlignedArena(ar, roundTo(len, 4), 4);
  } else {
    return allocAlignedArena(ar, len, 1);
  }
}

void *manageMemArena(Arena *ar, void *ptr) {
  if (ptr == NULL) {
    return NULL;
  }

  *VEC_PUSH(&ar->pages, ArenaPage) = (ArenaPage){
      .capacity = 0,
      .length = 0,
      .data = ptr,
  };
  return ptr;
}

char *internArena(Arena *ar, char *str) {
  if (str == NULL) {
    return NULL;
  } else {
    return strcpy(allocAlignedArena(ar, strlen(str) + 1, 1), str);
  }
}
