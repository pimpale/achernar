#ifndef COM_SCAN_H
#define COM_SCAN_H

#include "com_define.h"
#include "com_reader.h"
#include "com_writer.h"

// TODO add more scan options for integers and doubles

typedef struct {
    bool successful;
    // how many bytes were written to destination
    u64 written;
    // how many bytes were read from source
    u64 read;
} com_scan_UntilResult;

/// read until `c` is encountered in the stream
/// REQUIRES: `destination` is a valid pointer to a valid `com_writer`
/// REQUIRES: `source` is a valid pointer to a valid `com_reader`
/// GUARANTEES: will not write `c` into the destination
/// GUARANTEES: if a read from source or a write to destination fails, will stop
/// GUARANTEES: is not atomic
/// GUARANTEES: reads until `c` 
/// or min(flags(source) & limited ? source.limit : usize_max, flags(destination) & limited ? destination.limit : usize_max)
/// GUARANTEES: returns successful if no unexpected errors encountered
com_scan_UntilResult com_scan_until(com_writer* destination, com_reader* source, u8 c);

typedef enum {
  com_scan_CheckedStrExpectedDoubleQuote,
  com_scan_CheckedStrInvalidControlChar,
  com_scan_CheckedStrInvalidUnicodeSpecifier,
} com_scan_CheckedStrErrorKind;

// Result of reading a checked str
typedef struct {
    // always defined
    u64 written;
    // always defined
    u64 read;
    // if the read was (no unexpected errors)
    bool successful;
    // only defined if !successful
    com_scan_CheckedStrErrorKind error;
} com_scan_CheckedStrResult;

/// Parses n "characters" (each of which may be more than an actual u8 because of unicode)
/// REQUIRES: `destination` is a valid pointer to a valid com_writer
/// REQUIRES: `source` is a valid pointer to a valid com_reader
/// REQUIRES: `n` is the number of character to read
/// GUARANTEES: will read `n` "characters" from reader
/// GUARANTEES: will write the unescaped string to destination
/// GUARANTEES: if a syntax error is encountered, will immediately halt reading
/// GUARANTEES: this operation is not atomic
/// GUARANTEES: follows JSON syntax
com_scan_CheckedStrResult com_scan_checked_str(com_writer* destination, com_reader *source, usize n);

/// parses checked string until syntax error or unescaped close double quote
/// REQUIRES: `destination` is a valid pointer to a valid com_writer
/// REQUIRES: `source` is a valid pointer to a valid com_reader
/// GUARANTEES: will read "characters" from reader until an unescaped `"` is encountered
/// GUARANTEES: will write the unescaped string to destination
/// GUARANTEES: if a syntax error is encountered, will immediately halt reading
/// GUARANTEES: this operation is not atomic
/// GUARANTEES: follows JSON syntax
com_scan_CheckedStrResult com_scan_checked_str_until_quote(com_writer* destination, com_reader *source);

#endif

