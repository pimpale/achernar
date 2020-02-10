#ifndef REGION_H
#define REGION_H

#include "vector.h"

/* This header includes region aware duplications of common functions */

typedef vector_s Region;

/// Creates a region of memory which can be allocated from and then destroyed all at once
/// GUARANTEES: mem points to valid memory
/// GUARANTEES: return value contains pointer to valid Region
Region *createRegion(Region* mem);

/// Destroys the memory region and frees everything within it
/// REQUIRES: r is a pointer to a valid region
/// GUARANTEES: r is no longer a valid region
/// GUARANTEES: memory held by the region is deallocated
Region *destroyRegion(Region* r);

/// Allocates size bytes from region r. This memory cannot be freed or reallocated
/// REQUIRES: r is a pointer to a valid region
/// GUARANTEES: return contains pointer to valid section of memory with len size
/// GUARANTEES: if len is 0, no memory will be allocated
/// GUARANTEES: the return value will be located directly after the last byte allocated
void *ralloc(Region* r, size_t size);

// Duplicates a string into a region of memory
/// REQUIRES: r is a pointer to a valid region
/// REQUIRES: str is a pointer to a valid null terminated string
/// GUARANTEES: returns a pointer to a duplicated string allocated within the vector
char *rstrdup(const char* str, Region* r);

// Macros to help work with vectors

/// Push a single element to the the top of the region
#define REGION_PUSH(region, type) VEC_PUSH((Vector*)region, type)
/// Returns the current last pointer to the top of the region
#define REGION_TOP(region) ralloc(region, 0)

#endif
