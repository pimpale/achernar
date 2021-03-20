#ifndef COM_OS_IOSTREAM
#define COM_OS_IOSTREAM

#include "com_define.h"
#include "com_writer.h"
#include "com_reader.h"

// provides readers and writers for the standard input, standard output, and standard error

com_writer com_os_iostream_out(void);
com_writer com_os_iostream_err(void);
com_reader com_os_iostream_in(void);

#endif

