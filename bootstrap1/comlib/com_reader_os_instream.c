#include "com_reader_os_instream.h"
#include "com_os_iostream.h"
#include "com_reader_fn.h"

com_reader com_reader_os_instream_create(void) {
	return com_reader_fn_create(com_os_iostream_read_in);
}
