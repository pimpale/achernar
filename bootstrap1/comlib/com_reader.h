#ifndef COM_READER_H
#define COM_READER_H

#include "com_define.h"
#include "com_streamposition.h"
#include "com_str.h"

typedef enum {
  // no flags for this reader
  com_reader_FLAGS_NONE = 0,
  // there are a limited number of bytes that may be read from this
  com_reader_LIMITED = 1 << 1,
  // you can query n number of bytes into the future <use queue>
  com_reader_BUFFERED = 1 << 2,
} com_reader_Flag;

typedef u32 com_reader_Flags;

// forward declare reader struct
typedef struct com_reader_s com_reader;

typedef struct {
  bool valid;
  u8 value;
} com_reader_ReadResult;

typedef struct com_reader_s {
    bool _valid;
    com_reader_Flags _flags;

    // opaque pointer to the backing of this reader
    void* _backing;

    // allows you to read from the reader
    usize (*_read_str_fn)(const com_reader*, com_str* data);
    com_reader_ReadResult (*_read_u8_fn)(const com_reader*);

    // allows you to peek forward any number of bytes (if supported)
    com_reader_ReadResult (*_peek_u8_fn)(const com_reader*, usize n);

    // query stream position
    com_streamposition_LnCol (*_position_fn)(const com_reader*);

    // query how many bytes are available in the underlying resource
    usize (*_query_fn)(const com_reader*);

    // destroy reader wrapper
    void (*_destroy_fn)(com_reader*);
} com_reader;

///  flags that are valid for `r`
/// REQUIRES: `r` is a valid pointer to a com_reader
/// GUARANTEES: returns flags supported by default by `r`
com_reader_Flags com_reader_supports(const com_reader *r);


///  reads at most `data.len` bytes of `r` into `data.data` and returns the number of bytes read
/// REQUIRES: `data` is a valid com_str
/// REQUIRES: `r` is a valid pointer to a valid com_reader
/// GUARANTEES: the behavior of this method is identical to repeatedly calling `read_u8`
/// GUARANTEES: will stop reading on an the first error encountered
/// GUARANTEES: returns the number of bytes successfully read
usize com_reader_read_str(const com_reader* r, com_str* data);

///  reads u8 from the reader `r`
/// REQUIRES: `r` is a valid pointer to a valid com_reader
/// GUARANTEES: if `r` supports `com_reader_LIMITED`, any reads beyond the length will not succeed
/// GUARANTEES: if the operation succeeds, will return true and the next value of the reader
/// GUARANTEES: if the operation fails, will return false
com_reader_ReadResult com_reader_read_u8(const com_reader* r); 

///  reads u8 from the reader `r` and discards the result
/// REQUIRES: `r` is a valid dpointer to a valid `com_reader`
/// GUARANTEES: if `r` supports `com_reader_LIMITED` any reads beyond the length will not succeed
void com_reader_drop_u8(const com_reader* r);

///  peeks the `n`th u8 from the reader `r`
/// REQUIRES: `r` is a valid pointer to a valid com_reader
/// REQUIRES: `r` must support `com_reader_BUFFERED`
/// GUARANTEES: if `r` supports `com_reader_LIMITED`, any reads beyond the length will not succeed
/// GUARANTEES: if the operation succeeds, will return valid and the nth char of the reader
/// GUARANTEES: if the operation fails, will return invalid
com_reader_ReadResult com_reader_peek_u8(const com_reader* r, usize n); 

///  query how many bytes are available in the underlying resource (if applicable)
/// REQUIRES: `r` is a valid pointer pointing to a valid `com_reader`
/// REQUIRES: `r` must support `com_reader_LIMITED`
/// GUARANTEES: returns how many more bytes may safely be read from the reader
usize com_reader_query(const com_reader *r);

///  query the current locaation of the reader 
/// REQUIRES: `r` is a valid pointer pointing to a valid `com_reader`
/// GUARANTEES: returns a valid com_streamposition_LnCol representing the location of the cursor
com_streamposition_LnCol com_reader_position(const com_reader *r);

///  destroys the reader
/// REQUIRES: `r` is a valid pointer pointing to a valid `com_reader`
/// GUARANTEES: `r` is no longer a valid `com_reader`
void com_reader_destroy(com_reader* r);

#endif

