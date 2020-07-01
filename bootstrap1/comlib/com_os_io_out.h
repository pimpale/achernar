#ifndef COM_OS_IO_OUT
#define COM_OS_IO_OUT

// intrinsic OS methods related to manipulating out streams
// functionality may be implemented internally using com_os_io_fs

#include "com_writer.h"

com_writer com_os_io_out_writer_create(void);
com_writer com_os_io_err_writer_create(void);

#endif // COM_OS_IO_OUT
