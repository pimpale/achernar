#include <stddef.h>
#include <string.h>

#include "vector.h"
#include "linalloc.h"

LinearAllocator *createLinearAllocator(LinearAllocator* mem) {
  return createVector((Vector*)mem);
}

LinearAllocator *destroyLinearAllocator(LinearAllocator* r) {
  return destroyVector((Vector*) r);
}

void *linalloc(LinearAllocator* r, size_t size) {
  return pushVector((Vector*)r, size);
}

char *rstrdup(const char* str, LinearAllocator* r) {
  // Allocates enough memory for the string, then copies the string over
  // Finally returns pointer
  return strcpy(linalloc(r, strlen(str)+1), str);
}
