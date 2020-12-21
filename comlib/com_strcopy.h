#ifndef COM_STRCOPY
#define COM_STRCOPY

#include "com_define.h"
#include "com_str.h"
#include "com_allocator.h"

// returns a copy of `src` allocated with `handle`
// REQUIRES: `src` is a valid `com_str`
// REQUIRES: `handle` is a valid `com_allocator_Handle` 
// REQUIRES: `handle` has at least src->len bytes of data
// GUARANTEES: returns a valid `com_str_mut` whose contents are exactly the same as `src`
// GUARANTEES: the length of the returned value is `src.len`
com_str_mut com_strcopy(com_str src, com_allocator_Handle handle);

#endif

