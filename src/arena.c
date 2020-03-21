#include <stddef.h>
#include <string.h>

#include "vector.h"
#include "arena.h"

Arena *createArena(Arena* mem) {
  return createVector((Vector*)mem);
}

Arena *destroyArena(Arena* a) {
  return destroyVector((Vector*) a);
}

void *allocArena(Arena* a, size_t size) {
  return pushVector((Vector*)a, size);
}

char *intern(const char* str, Arena* a) {
  // Allocates enough memory for the string, then copies the string over
  // Finally returns pointer
  return strcpy(allocArena(a, strlen(str)+1), str);
}
