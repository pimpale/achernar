#ifndef COM_OS_IO_H
#define COM_OS_IO_H

#include "com_define.h"
#include "com_str.h"

typedef enum {
  // can't read or write, illegal
  com_os_io_ACCESS_NONE = 0,
  // open to read from it
  com_os_io_ACCESS_R = 1<<1,
  // open to write/modify to it
  com_os_io_ACCESS_W = 1<<2,
  // will create the file if it doesnt already exist (by default will fail with an error message)
  com_os_io_OPEN_CREATE = 1<<3,
  // only write to end, all repositioning will be ignored
  com_os_io_APPEND_ENABLE = 1<<4,
  com_os_io_BINARY_MODE = 1<<5
} com_io_AccessFlag;

typedef u32 com_os_io_Flags;

typedef struct {
  bool valid;
  u32 _id;
} com_os_io_Handle; 

com_os_io_Handle com_os_io_open(com_str identifier, com_os_io_Flags flags);

// TODO figure out the semantics of moving and editing in a file properly
// For now, we're only going to support appending and reading from the end of stdin, stdout, and stderr
// def will fix later

// flushes all writes to the file
void com_os_io_flush(com_os_io_Handle handle);

// inserts one u8 byte at the location of the file cursor
void com_os_io_write_u8(com_os_io_Handle handle, u8 data);

// inserts the whole string at the location of the file cursor
void com_os_io_write_str(com_os_io_Handle handle, const com_str data);

// reads at most `data.len` bytes into data and returns the number of bytes
// REQUIRES: `buffer` is a valid pointer to at least `buffer_len` u8s
// GUARANTEES: up to `buffer_len` bytes will be read, halting on EOF or 
usize com_os_io_read_str(com_os_io_Handle handle, com_str* data);
usize com_os_io_read_u8(com_os_io_Handle handle, u8* data);

// closes file handle
void com_os_io_close(com_os_io_Handle handle);

#endif
