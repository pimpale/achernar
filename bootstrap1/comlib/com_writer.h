#ifndef COM_WRITER_H
#define COM_WRITER_H

#include "com_define.h"
#include "com_str.h"

typedef enum {
  // no flags for this writer
  com_writer_FLAGS_NONE = 0,
  // can get how many bytes can still be written
  com_writer_BYTES_LIMITED  = 1<<1,
} com_writer_Flag;

typedef u32 com_writer_Flags;

// forward declare writer struct
typedef struct com_writer_s com_writer;

typedef struct com_writer_s {
    bool _valid;
    // default and supported flags for this writer
    com_writer_Flags _default_flags;
    com_writer_Flags _supported_flags;

    // flags used for this instance of the writer
    com_writer_Flags _used_flags;

    // opaque pointer to the backing of this writer
    void* _backing; 

    // allows you to write to the writer
    usize (*_append_str_fn)(const com_writer*, const com_str data);
    bool (*_append_u8_fn)(const com_writer*, const u8 data);

    // query how many bytes are available in the underlying resource
    usize (*_query_fn)(const com_writer*);

    // destroy writer wrapper
    void (*_destroy_fn)(com_writer*);
} com_writer;


/** flags that are enabled by default for an writer (cannot be disabled)
 * REQUIRES: `w` is a valid pointer to a com_writer
 * GUARANTEES: returns flags supported by default by `w`
 */
com_writer_Flags com_writer_defaults(const com_writer *w);

/** flags that are valid for `w` (may be enabled)
 * REQUIRES: `w` is a valid pointer to a com_writer
 * GUARANTEES: returns flags supported by default by `w`
 */
com_writer_Flags com_writer_supports(const com_writer *w);


/** appends `data.len` bytes of `data` to the writer `w`
 * REQUIRES: `data` is a valid com_str
 * REQUIRES: `w` is a valid pointer to a valid com_writer
 * REQUIRES: if the underlying resource supports `com_writer_BYTES_LIMITED`, then `com_writer_query(w)` >= `data.len`
 * GUARANTEES: the behaviour of this method is identical to repeatedly calling `append_u8`
 * GUARANTEES: will stop writing on an the first error encountered
 * GUARANTEES: returns the number of bytes successfully written
 */
usize com_writer_append_str(const com_writer* w, const com_str data);


/** appends `u8` to the writer `w`
 * REQUIRES: `w` is a valid pointer to a valid com_writer
 * REQUIRES: if the underlying resource supports `com_writer_BYTES_LIMITED` then `com_writer_query(w)` > 0
 * GUARANTEES: writes `data` to `w`
 * GUARANTEES: if the operation succeeds, will return true
 * GUARANTEES: if the operation fails, will return false
 */
bool com_writer_append_u8(const com_writer* w, const u8 data); 

/** query how many bytes are available in the underlying resource (if applicable)
 * REQUIRES: `w` is a valid pointer pointing to a valid `com_writer`
 * REQUIRES: `w` must support `com_writer_BYTES_LIMITED`
 * GUARANTEES: returns how many more bytes may safely be written to the writer
 */
usize com_writer_query(const com_writer *w);

/** destroys the writer
 * REQUIRES: `w` is a valid pointer pointing to a valid `com_writer`
 * GUARANTEES: `w` is no longer a valid `com_writer`
 */
void com_writer_destroy(com_writer* w);

#endif
