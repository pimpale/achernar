#include <stddef.h>
#include <string.h>

#include "vector.h"
#include "arena.h"

Arena *createArena(Arena* mem) {
  return createVector((Vector*)mem);
}

Arena *destroyArena(Arena* r) {
  return destroyVector((Vector*) r);
}

void *allocArena(Arena* r, size_t size) {
  return pushVector((Vector*)r, size);
}

char *intern(const char* str, Arena* r) {
  // Allocates enough memory for the string, then copies the string over
  // Finally returns pointer
  return strcpy(allocArena(r, strlen(str)+1), str);
}
