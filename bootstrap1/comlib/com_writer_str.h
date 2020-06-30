#ifndef COM_WRITER_STR_H
#define COM_WRITER_STR_H

// this offers an implementation of a writer based on an underlying com_str (fixed len buffer)

#include "com_writer.h"
#include "com_str.h"

typedef struct {
    // backing string
    com_str* _str;
    // current index into the string
    usize _index;
} com_writer_str_backing;

/**
 * Constructs a com_writer which will write to `destination` using the offset. puts metadata into backing;
 * REQUIRES: `destination` is a valid pointer to a valid `com_str` that will be written to
 * REQUIRES: `offset` represents the index at which to begin writing into `destination` 
 * REQUIRES: `offset` < `destination->len`
 * REQUIRES: `backing` is valid pointer to memory that will be initialized with the backing data for this writer
 * REQUIRES: `backing` must stay at the same memory address for the duration of this writer
 * GUARANTEES: the writer cursor will start at `offset` bytes after the `destination->data`
 * GUARANTEES: the writer will not alter the length of the string or allocate any memory whatsoever
 * GUARANTEES: the backing will be overwritten
 * GUARANTEES: the writer will support `com_writer_BYTES_LIMITED`
 */
com_writer com_writer_str_create(com_str* destination, usize offset, com_writer_str_backing *backing);

#endif

