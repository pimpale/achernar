#include "com_os_io_out.h"

#include 

bool append_u8_fn(const com_writer *w, const u8 data) {
    putc(data);
}
