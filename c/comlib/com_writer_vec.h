#ifndef COM_WRITER_VEC
#define COM_WRITER_VEC

// this offers an implementation of a writer based on an underlying vector

#include "com_writer.h"
#include "com_vec.h"

com_writer com_writer_vec_create(com_vec* backing);

#endif
