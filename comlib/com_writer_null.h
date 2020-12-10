#ifndef COM_WRITER_NULL_H
#define COM_WRITER_NULL_H

// the /dev/null of writers

#include "com_define.h"
#include "com_writer.h"

// returns a writer that does not remember data
com_writer com_writer_null(void);

#endif

