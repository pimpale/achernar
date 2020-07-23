#ifndef COM_READER_FN_H
#define COM_READER_FN_H

#include "com_define.h"
#include "com_reader.h"

// creates a reader out of a single function that can push a com_str and return the length
com_reader com_reader_fn_create(com_str (*read_fn)(u8* buffer, usize buflen));

#endif
