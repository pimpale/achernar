#ifndef COM_WRITER_H
#define COM_WRITER_H

#include "com_define.h"
#include "com_str.h"

typedef enum {
  // no flags for this writer
  com_writer_FLAGS_NONE = 0,
  // Means that unlimited writing is not permitted
  // There is a fixed amount of space to write to
  // If a writer has this flag enabled, it must be able to query how many bytes can still be written
  com_writer_LIMITED  = 1<<1,
  // whether the  writer is buffered (uses an internal buffer to speed up writing to the underlying resource)
  // Solely memory based writers donn't need this flag, because there's no need to `lush` them
  // If this flag is enabled, the writer must support the `flush` operation
  com_writer_BUFFERED = 1 << 2,
} com_writer_Flag;

typedef u32 com_writer_Flags;

typedef struct {
    bool valid;
} com_writer_WriteResult;


// forward declare writer struct
typedef struct com_writer_s com_writer;

typedef struct com_writer_s {
    bool _valid;

    // flags used for this instance of the writer
    com_writer_Flags _flags;

    // opaque pointer to the backing of this writer
    void* _backing; 

    // allows you to write to the writer
    usize (*_append_str_fn)(const com_writer*, const com_str data);
    com_writer_WriteResult (*_append_u8_fn)(const com_writer*, const u8 data);

    // query how many bytes are available in the underlying resource
    usize (*_query_fn)(const com_writer*);

    // flush to underlying resource
    void  (*_flush_fn)(const com_writer*);

    // destroy writer wrapper
    void (*_destroy_fn)(com_writer*);
} com_writer;


/// flags that are enabled by default for an writer (cannot be disabled)
/// REQUIRES: `w` is a valid pointer to a com_writer
/// GUARANTEES: returns flags supported by default by `w`
com_writer_Flags com_writer_flags(const com_writer *w);

///  appends `data.len` bytes of `data` to the writer `w`
/// REQUIRES: `data` is a valid com_str
/// REQUIRES: `w` is a valid pointer to a valid com_writer
/// REQUIRES: if the underlying resource supports `com_writer_LIMITED`, then `com_writer_query(w)` >= `data.len`
/// GUARANTEES: the behaviour of this method is identical to repeatedly calling `append_u8`
/// GUARANTEES: will stop writing on an the first error encountered
/// GUARANTEES: returns the number of bytes successfully written
usize com_writer_append_str(const com_writer* w, const com_str data);


///  appends `u8` to the writer `w`
/// REQUIRES: `w` is a valid pointer to a valid com_writer
/// REQUIRES: if the underlying resource supports `com_writer_LIMITED` then `com_writer_query(w)` > 0
/// GUARANTEES: writes `data` to `w`
/// GUARANTEES: if the operation succeeds, will return true
/// GUARANTEES: if the operation fails, will return false
com_writer_WriteResult com_writer_append_u8(const com_writer* w, const u8 data); 

///  query how many bytes are available in the underlying resource (if applicable)
/// REQUIRES: `w` is a valid pointer pointing to a valid `com_writer`
/// REQUIRES: `w` must support `com_writer_LIMITED`
/// GUARANTEES: returns how many more bytes may safely be written to the writer
usize com_writer_query(const com_writer *w);

///  flush all buffered changes to the underlying resource (if applicable)
/// REQUIRES: `w` is a valid pointer pointing to a valid `com_writer`
/// REQUIRES: `w` must support `com_writer_BUFFERED`
/// GUARANTEES: all writes made will be flushed from the buffer and into the underlying resource
void com_writer_flush(const com_writer *w);

///  destroys the writer
/// REQUIRES: `w` is a valid pointer pointing to a valid `com_writer`
/// GUARANTEES: `w` is no longer a valid `com_writer`
void com_writer_destroy(com_writer* w);

#endif
