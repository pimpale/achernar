#ifndef COM_MEM
#define COM_MEM

#include "com_define.h"

/// sets the `len` bytes located at `ptr` to the value 0
/// REQUIRES: `ptr` is a valid pointer
/// REQUIRES: `len` is the number of bytes to set to 0
/// GUARANTEES: `len` bytes of memory located at `ptr` will be set to 0
void com_mem_zero(void* ptr, const usize len);

/// utility macro setting a pointer's object to zero
/// REQUIRES: `ptr` is a valid pointer to a struct or a fixed size array
/// GUARANTEES: all bytes of the struct located at `ptr` will be set to zero
#define com_mem_zero_obj_m(ptr) com_mem_zero((ptr), sizeof(ptr))

/// utility macro setting n structs of a given size to zero
/// REQUIRES: `ptr` is a valid pointer the beginning of an array of `n` elements of type `t`
/// GUARANTEES: all bytes of `ptr` will be set to zero
#define com_mem_zero_arr_m(ptr, n, type) com_mem_zero((ptr), (n)*sizeof(type))


/// sets the `len` bytes located at `ptr` to the value `byte`
/// REQUIRES: `ptr` is a valid pointer
/// REQUIRES: `len`  is the number of bytes to set to 0
/// REQUIRES: `byte` is the value to overwrite each byte with
/// GUARANTEES: `len` bytes of memory located at `ptr` will be set to 0
void com_mem_set(void* ptr, const usize len, const u8 byte);

/// copies `n` bytes of memory from `src` to `dest` (even if they overlap)
/// REQUIRES: `dest` is a valid pointer to at least `n` bytes of memory
/// REQUIRES: `src` is a valid pointer to at least `n` bytes of memory
/// REQUIRES: `n` is a digit representing the number of bytes to copy from `src` to `dest`
/// GUARANTEES: the first `n` bytes at `dest` are identical to the first `n` bytes at `src`
void com_mem_move(void* dest, const void* src, usize n);

#endif

