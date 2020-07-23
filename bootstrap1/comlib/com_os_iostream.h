#ifndef COM_OS_IOSTREAM_H
#define COM_OS_IOSTREAM_H

#include "com_define.h"
#include "com_str.h"

// intrinsic methods relating to manipulating the standard io streams (which may be stuff like files/ports) depending on platform
// (may be internally linked to com os fs)

usize com_os_iostream_write_err(const com_str str);
usize com_os_iostream_write_out(const com_str str);
com_str com_os_iostream_read_in(u8* buffer, usize buflen);

#endif

