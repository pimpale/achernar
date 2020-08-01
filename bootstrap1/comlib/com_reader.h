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
  com_reader_POSITION = 1 << 3,
} com_reader_Flag;

typedef u32 com_reader_Flags;

// forward declare reader struct
typedef struct com_reader_s com_reader;

typedef struct {
  bool valid;
  u8 value;
} com_reader_ReadU8Result;

typedef struct {
  bool valid;
  com_str_mut value;
} com_reader_ReadStrResult;

typedef struct com_reader_s {
    bool _valid;
    com_reader_Flags _flags;

    // opaque pointer to the backing of this reader
    void* _backing;

    // allows you to read from the reader
    com_reader_ReadStrResult(*_read_str_fn)(const com_reader*, com_str_mut buffer);
    com_reader_ReadU8Result (*_read_u8_fn)(const com_reader*);

    // allows you to peek forward any number of bytes (if supported)
    com_reader_ReadU8Result(*_peek_u8_fn)(const com_reader*, usize n);

    // query stream position (if supported)
    com_streamposition_LnCol (*_position_fn)(const com_reader*);

    // query how many bytes are available in the underlying resource
    u64 (*_query_fn)(const com_reader*);

    // destroy reader wrapper
    void (*_destroy_fn)(com_reader*);
} com_reader;

///  flags that are valid for `r`
/// REQUIRES: `r` is a valid pointer to a com_reader
/// GUARANTEES: returns flags supported by default by `r`
com_reader_Flags com_reader_flags(const com_reader *r);

///  reads at most `buflen` bytes of `r` into `buffer` and returns a com_str
/// REQUIRES: `buffer` is a valid pointer to at least `buflen` u8s
/// REQUIRES: `r` is a valid pointer to a valid com_reader
/// GUARANTEES: the behavior of this method is identical to repeatedly calling `read_u8`
/// GUARANTEES: will stop reading on an the first error encountered
/// GUARANTEES: if the buffer provided was completely filled with no errors encountered, .valid=true
/// GUARANTEES: .value= a com_str with .value=buffer and .length=how many bytes were able to read successfully
com_reader_ReadStrResult com_reader_read_str(const com_reader* r, com_str_mut buffer);

///  reads u8 from the reader `r`
/// REQUIRES: `r` is a valid pointer to a valid com_reader
/// GUARANTEES: if `r` supports `com_reader_LIMITED`, any reads beyond the length will not succeed
/// GUARANTEES: if the operation succeeds, will return .valid=true and the .value=next value of the reader
/// GUARANTEES: if the operation fails, will return .valid=false and .value=undefined
com_reader_ReadU8Result com_reader_read_u8(const com_reader* r); 

///  reads u8 from the reader `r` and discards the result
/// REQUIRES: `r` is a valid dpointer to a valid `com_reader`
/// GUARANTEES: if `r` supports `com_reader_LIMITED` any reads beyond the length will not succeed
void com_reader_drop_u8(const com_reader* r);

///  peeks the `n`th u8 from the reader `r`
/// REQUIRES: `r` is a valid pointer to a valid com_reader
/// REQUIRES: `r` must support `com_reader_BUFFERED`
/// GUARANTEES: if `r` supports `com_reader_LIMITED`, any reads beyond the length will not succeed
/// GUARANTEES: if the operation succeeds, will return .valid=true and .value=the nth char of the reader
/// GUARANTEES: if the operation fails, will return .valid=false and .value=undefined
com_reader_ReadU8Result com_reader_peek_u8(const com_reader* r, usize n); 

///  query how many bytes are available in the underlying resource (if applicable)
/// REQUIRES: `r` is a valid pointer pointing to a valid `com_reader`
/// REQUIRES: `r` must support `com_reader_LIMITED`
/// GUARANTEES: returns how many more bytes may safely be read from the reader
u64 com_reader_query(const com_reader *r);

///  query the current locaation of the reader (if supported) 
/// REQUIRES: `r` is a valid pointer pointing to a valid `com_reader`
/// REQUIRES: `r` supports com_reader_POSITION
/// GUARANTEES: returns a valid com_streamposition_LnCol representing the location of the cursor
com_streamposition_LnCol com_reader_position(const com_reader *r);

///  destroys the reader
/// REQUIRES: `r` is a valid pointer pointing to a valid `com_reader`
/// GUARANTEES: `r` is no longer a valid `com_reader`
void com_reader_destroy(com_reader* r);

#endif

