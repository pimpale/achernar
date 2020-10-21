#ifndef COM_READER_STR_H
#define COM_READER_STR_H

// this offers an implementation of a reader based on an underlying com_str (fixed len buffer)

#include "com_reader.h"
#include "com_str.h"

typedef struct {
    // backing string
    com_str* _str;
    // current index into the string
    usize _index;
} com_reader_str_backing;

/**
 * Constructs a com_reader which will read from `source` using the offset. puts metadata into backing;
 * REQUIRES: `source` is a valid pointer to a valid `com_str` that will be read from
 * REQUIRES: `offset` represents the index at which to begin reading from `source` 
 * REQUIRES: `offset` < `source->len`
 * REQUIRES: `backing` is valid pointer to memory that will be initialized with the backing data for this reader
 * REQUIRES: `backing` must stay at the same memory address for the duration of this reader
 * GUARANTEES: the reader cursor will start at `offset` bytes after the `source->data`
 * GUARANTEES: the reader will not alter the length of the string or allocate any memory whatsoever
 * GUARANTEES: the backing will not be altered
 * GUARANTEES: the reader will support `com_reader_BYTES_LIMITED`
 */
com_reader com_reader_str_create(com_str* source, usize offset, com_reader_str_backing *backing);

#endif

