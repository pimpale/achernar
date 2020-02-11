#ifndef LINALLOC_H
#define LINALLOC_H

#include "vector.h"

/* This header includes linear allocator aware duplications of common functions */

typedef vector_s LinearAllocator;

/// Creates a linear allocator in a region of a memory
/// GUARANTEES: mem points to valid memory
/// GUARANTEES: return value contains pointer to valid LinearAllocator
LinearAllocator *createLinearAllocator(LinearAllocator* mem);

/// Destroys the linear allocator and frees all memory associated with it
/// REQUIRES: la is a pointer to a valid linear allocator
/// GUARANTEES: la is no longer a valid linear allocator
/// GUARANTEES: memory held by the linear allocator is deallocated
LinearAllocator *destroyLinearAllocator(LinearAllocator* la);

/// Allocates size bytes from linear allocator la. This memory cannot be freed or reallocated
/// REQUIRES: la is a pointer to a valid linear allocator
/// GUARANTEES: return contains pointer to valid section of memory with len size
/// GUARANTEES: if len is 0, no memory will be allocated
/// GUARANTEES: the return value will be located directly after the last byte allocated
void *linalloc(LinearAllocator* la, size_t size);

// Duplicates a string using memory allocated from la
/// REQUIRES: la is a pointer to a valid linear allocator
/// REQUIRES: str is a pointer to a valid null terminated string
/// GUARANTEES: returns a pointer to a duplicated string allocated within the vector
char *rstrdup(const char* str, LinearAllocator* la);

// Macros to help work with vectors

/// Push a single element to the the top of the linear allocator
#define L_ALLOC_PUSH(la, type) VEC_PUSH((Vector*)la, type)

#endif
