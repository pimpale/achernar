#include <stdlib.h>
#include <string.h>

#include "arena.h"
#include "error.h"
#include "vector.h"

#define DEFAULT_PAGE_SIZE 1024

typedef struct ArenaPage_s {
  void *data;      // pointer to page
  size_t length;   // amount of page currently used
  size_t capacity; // The capacity of this page
} ArenaPage;

/// Initializes a new empty arena page with the given size
/// REQUIRES: `mem` is a pointer to at least sizeof(ArenaPage) bytes of
///            valid memory
/// GUARANTEES: `mem` has been initialized to a valid ArenaPage
/// GUARANTEES: the ArenaPage has a capacity of at least `capacity`
/// GUARANTEES: returns `mem`
static ArenaPage *createArenaPage(ArenaPage *mem, size_t capacity);

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

ArenaPage *createArenaPage(ArenaPage *mem, size_t capacity) {
  mem->data = malloc(capacity);
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

Arena *createArena(Arena *mem) {
  // initialize vector
  createVector(&mem->pages);
  // Push page
  createArenaPage(VEC_PUSH(&mem->pages, ArenaPage), DEFAULT_PAGE_SIZE);
  mem->current_index = 0;
  return mem;
}

Arena *destroyArena(Arena *ar) {
  for (size_t i = 0; i < VEC_LEN(&ar->pages, ArenaPage); i++) {
    destroyArenaPage(VEC_GET(&ar->pages, i, ArenaPage));
  }
  destroyVector(&ar->pages);
  return ar;
}

void* allocAlignedArena(Arena* ar, size_t len, size_t alignment) {
  if (len == 0) {
    return NULL;
  }

  // get the last page in our list
  size_t numPages = VEC_LEN(&ar->pages, ArenaPage);

  ArenaPage *a;
  if (numPages == 0) {
    // allocate new ArenaPage with max(len, DEFAULT_PAGE_SIZE) size
    a = VEC_PUSH(&ar->pages, ArenaPage);
    size_t pageCapacity = DEFAULT_PAGE_SIZE > len ? DEFAULT_PAGE_SIZE : len;
    createArenaPage(a, pageCapacity);
  } else {
    // get the last vector element
    size_t lastIndex = numPages - 1;
    a = VEC_GET(&ar->pages, lastIndex, ArenaPage);
    if (!canFitArenaPage(a, len)) {
      a = VEC_PUSH(&ar->pages, ArenaPage);
      size_t pageCapacity = DEFAULT_PAGE_SIZE > len ? DEFAULT_PAGE_SIZE : len;
      createArenaPage(a, pageCapacity);
    }
  }
  return allocArenaPage(a, len);
}

void *allocArena(Arena *ar, size_t len) {
  size_t alignment = 0;
  if(len > 8) {
    alignment = 8;
  } else if(len > 2) {
    alignment = 4;
  } else {
    alignment = 0;
  }
}

void manageMemArena(Arena *ar, void *ptr, size_t len) {
  *VEC_PUSH(&ar->pages, ArenaPage) = (ArenaPage){
      .capacity = len,
      .length = len,
      .data = ptr,
  };
}
