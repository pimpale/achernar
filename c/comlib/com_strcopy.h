#ifndef COM_STRCOPY
#define COM_STRCOPY

#include "com_define.h"
#include "com_str.h"
#include "com_allocator.h"

// returns a copy of `src` allocated from `a` where allocation has opts default + noleak
// it is necessary to have noleak for memory safety, since this method never exposes the handle
// internally calls `com_strcopy_to_handle`
// REQUIRES: `a` is a valid pointer to an allocator
// REQUIRES: `a` supports com_allocator_NOLEAK
// REQUIRES: `src` is a valid com_str
// GUARANTEES: returns a valid `com_str_mut` whose contents are exactly the same as `src`
// GUARANTEES: the length of the returned value is `src.len`
com_str_mut com_strcopy_noleak(com_str src, com_allocator* a);

// returns a copy of `src` allocated with `handle`
// REQUIRES: `src` is a valid `com_str`
// REQUIRES: `handle` is a valid `com_allocator_Handle` 
// REQUIRES: `handle` has at least src->len bytes of data
// GUARANTEES: returns a valid `com_str_mut` whose contents are exactly the same as `src`
// GUARANTEES: the length of the returned value is `src.len`
com_str_mut com_strcopy_to_handle(com_str src, com_allocator_Handle handle);

#endif

