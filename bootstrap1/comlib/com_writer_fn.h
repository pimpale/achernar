#ifndef COM_WRITER_FN_H
#define COM_WRITER_FN_H

#include "com_define.h"
#include "com_writer.h"

// creates a writer out of a single function that can push a com_str and return the length
com_writer com_writer_fn_create(usize (*write_fn)(const com_str data));

#endif 

