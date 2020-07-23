#include "com_writer_os_errstream.h"
#include "com_writer_fn.h"
#include "com_os_iostream.h"

com_writer com_writer_os_errstream_create(void) {
	return com_writer_fn_create(com_os_iostream_write_err);
}
