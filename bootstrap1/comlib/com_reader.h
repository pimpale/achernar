#ifndef COM_READER_H
#define COM_READER_H

#include "com_define.h"
#include "com_str.h"

typedef enum {
  // no flags for this reader
  com_reader_FLAGS_NONE = 0,
  // there are a limited number of bytes that may be read from this
  com_reader_LIMITED = 1 << 1,
} com_reader_Flag;

typedef u32 com_reader_Flags;

// forward declare reader struct
typedef struct com_reader_s com_reader;

typedef struct com_reader_s {
    bool _valid;
    com_reader_Flags _flags;

    // opaque pointer to the backing of this reader
    void* _backing;

    // allows you to read from the reader
    usize (*_read_str_fn)(const com_reader*, com_str* data);
    bool (*_read_u8_fn)(const com_reader*, u8* data);

    // query how many bytes are available in the underlying resource
    usize (*_query_fn)(const com_reader*);

    // resync resource
    void  (*_resync_fn)(const com_reader*);

    // destroy reader wrapper
    void (*_destroy_fn)(com_reader*);
} com_reader;

/** flags that are valid for `r`
 * REQUIRES: `r` is a valid pointer to a com_reader
 * GUARANTEES: returns flags supported by default by `r`
 */
com_reader_Flags com_reader_supports(const com_reader *r);


/** reads at most `data.len` bytes of `r` into `data.data` and returns the number of bytes read
 * REQUIRES: `data` is a valid com_str
 * REQUIRES: `r` is a valid pointer to a valid com_reader
 * GUARANTEES: the behaviour of this method is identical to repeatedly calling `read_u8`
 * GUARANTEES: will stop reading on an the first error encountered
 * GUARANTEES: returns the number of bytes successfully read
 */
usize com_reader_read_str(const com_reader* r, com_str* data);


/** reads u8 from the reader `r` and sets `data`
 * REQUIRES: `r` is a valid pointer to a valid com_reader
 * GUARANTEES: reads `data` to `r`
 * GUARANTEES: if the operation succeeds, will return true
 * GUARANTEES: if the operation fails, will return false
 */
bool com_reader_read_u8(const com_reader* r,  u8* data); 

/** query hor many bytes are available in the underlying resource (if applicable)
 * REQUIRES: `r` is a valid pointer pointing to a valid `com_reader`
 * REQUIRES: `r` must support `com_reader_LIMITED`
 * GUARANTEES: returns how many more bytes may safely be read from the reader
 */
usize com_reader_query(const com_reader *r);

/** destroys the reader
 * REQUIRES: `r` is a valid pointer pointing to a valid `com_reader`
 * GUARANTEES: `r` is no longer a valid `com_reader`
 */
void com_reader_destroy(com_reader* r);

#endif

