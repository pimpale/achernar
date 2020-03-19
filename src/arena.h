#ifndef ARENA_H_
#define ARENA_H_

#include "vector.h"

/* This header includes arena aware duplications of common functions */

typedef struct vector_s Arena;

/// Creates a arena in a region of a memory
/// GUARANTEES: mem points to valid memory
/// GUARANTEES: return value contains pointer to valid Arena
Arena *createArena(Arena* mem);

/// Destroys the arena and frees all memory associated with it
/// REQUIRES: la is a pointer to a valid arena
/// GUARANTEES: la is no longer a valid arena
/// GUARANTEES: memory held by the arena is deallocated
Arena *destroyArena(Arena* la);

/// Allocates size bytes from arena la. This memory cannot be freed or reallocated
/// REQUIRES: la is a pointer to a valid arena
/// GUARANTEES: return contains pointer to valid section of memory with len size
/// GUARANTEES: if len is 0, no memory will be allocated
/// GUARANTEES: the return value will be located directly after the last byte allocated
void *allocArena(Arena* la, size_t size);

// Duplicates the string using memory allocated from la
/// REQUIRES: la is a pointer to a valid arena
/// REQUIRES: str is a pointer to a valid null terminated string
/// GUARANTEES: returns a pointer to a duplicated string allocated within the vector
char *intern(const char* str, Arena* la);
#endif
