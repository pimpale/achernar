#ifndef ARENA_H_
#define ARENA_H_

#include <stddef.h>

#include "vector.h"
#include "string.h"

typedef struct Arena_s {
  size_t current_1_index;
  size_t current_2_index;
  size_t current_4_index;
  size_t current_8_index;
  Vector pages;
} Arena;

/// Creates a arena in a region of a memory
/// REQUIRES: `mem` is a pointer to at least sizeof(Arena) bytes
/// GUARANTEES: `mem` has been initialized to a valid Arena
/// GUARANTEES: return value is `mem`
Arena *createArena(Arena* mem);

/// Destroys the arena and frees all memory associated with it
/// REQUIRES: `ar` is a pointer to a valid Arena
/// GUARANTEES: `ar` is no longer a valid Arena
/// GUARANTEES: all memory held by `ar` is deallocated
Arena *destroyArena(Arena* ar);

/// Allocates `len` bytes from `ar`. This memory cannot be freed or reallocated
/// REQUIRES: `ar` is a pointer to a valid Arena
/// GUARANTEES: return contains pointer to valid section of memory `len` bytes long
/// GUARANTEES: if `len` is 0, no memory will be allocated, NULL will be returned
/// GUARANTEES: if len is greater than 4, the pointer returned is 8-byte aligned
/// GUARANTEES: if len is greater than or equal to 2, the pointer returned is 4-byte aligned
void *allocArena(Arena* ar, size_t len);

/// Allocates `len` bytes from `ar`, aligned to the specified allocation
/// REQUIRES: `ar` is a pointer to a valid Arena
/// REQUIRES: `alignment` is one of 1, 2, 4, or 8
/// REQUIRES: `len` is a multiple of `alignment`
/// GUARANTEES: return contains pointer to valid section of memory `len` bytes long
/// GUARANTEES: if `len` is 0, no memory will be allocated, NULL will be returned
/// GUARANTEES: the returned pointer will be aligned to `alignment`
void *allocAlignedArena(Arena* ar, size_t len, size_t alignment);


/// Hands control of malloc'd memory at `ptr` to Arena `ar`.
/// This allows the memory to be deallocated when the arena is deallocated
/// REQUIRES: `ar` is a pointer to a valid Arena
/// REQUIRES: `ptr` is a valid ptr that was allocated using malloc
/// REQUIRES: `ptr` points to the beginning of the allocated area
/// REQUIRES: `ptr` will not be deallocated or reallocated
/// GUARANTEES: `ptr` will still be a valid pointer to the data
/// GUARANTEES: All contents of `ptr` will be held constant
/// GUARANTEES: `ptr` will be freed when `ar` is `destroyArena` is called
/// GUARANTEES: returns `ptr`
void* manageMemArena(Arena* ar, void* ptr);

// Utility Macros

// Duplicates the string using memory allocated from `ar`
/// REQUIRES: `ar` is a pointer to a valid Arena
/// REQUIRES: `str` is a pointer to a valid null terminated string
/// GUARANTEES: returns a pointer to a duplicated string allocated within the vector
#define INTERN(str, ar) strcpy(allocAlignedArena(ar, strlen(str)+1, 1), str)

#endif
