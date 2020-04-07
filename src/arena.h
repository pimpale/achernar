#ifndef ARENA_H_
#define ARENA_H_

#include <stddef.h>

#include "vector.h"
#include "string.h"

typedef struct Arena_s {
  Vector pages;
} Arena;

/// Creates a arena in a region of a memory
/// REQUIRES: `mem` is a pointer to at least sizeof(Arena) bytes
/// GUARANTEES: `mem` has been initialized to a valid Arena
/// GUARANTEES: return value is `mem`
Arena *createArena(Arena* mem);

/// Destroys the arena and frees all memory associated with it
/// REQUIRES: `ar` is a pointer to a valid arena
/// GUARANTEES: `ar` is no longer a valid arena
/// GUARANTEES: all memory held by `ar` is deallocated
Arena *destroyArena(Arena* ar);

/// Allocates `len` bytes from `ar`. This memory cannot be freed or reallocated
/// REQUIRES: `ar` is a pointer to a valid Arena
/// GUARANTEES: return contains pointer to valid section of memory `len` bytes long
/// GUARANTEES: if `len` is 0, no memory will be allocated
void *allocArena(Arena* ar, size_t len);

// Utility Macros

// Duplicates the string using memory allocated from `ar`
/// REQUIRES: `ar` is a pointer to a valid Arena
/// REQUIRES: `str` is a pointer to a valid null terminated string
/// GUARANTEES: returns a pointer to a duplicated string allocated within the vector
#define INTERN(str, ar) strcpy(allocArena(ar, strlen(str)+1), str)

#endif
