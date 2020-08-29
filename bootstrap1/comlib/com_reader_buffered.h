#ifndef COM_READER_BUFFERED
#define COM_READER_BUFFERED

// provides methods to buffer reading and peek ahead

#include "com_define.h"
#include "com_reader.h"
#include "com_allocator.h"

// creates a new com_bufreader
// REQUIRES: `r` is a valid pointer to a valid com_reader
// REQUIRES: `a` is a valid pointer to a valid com_allocator
com_reader com_reader_buffered(com_reader* r, com_allocator *a);

#endif

