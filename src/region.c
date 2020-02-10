#include <stddef.h>
#include <string.h>

#include "region.h"
#include "vector.h"

Region *createRegion(Region* mem) {
  return createVector((Vector*)mem);
}

Region *destroyRegion(Region* r) {
  return destroyVector((Vector*) r);
}

void *ralloc(Region* r, size_t size) {
  return pushVector((Vector*)r, size);
}

char *rstrdup(const char* str, Region* r) {
  // Allocates enough memory for the string, then copies the string over
  // Finally returns pointer
  return strcpy(ralloc(r, strlen(str)+1), str);
}
