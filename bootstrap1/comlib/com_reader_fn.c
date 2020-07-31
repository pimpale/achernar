#include "com_reader_fn.h"
#include "com_assert.h"

static com_reader_ReadStrResult read_str_fn(const com_reader *w, com_str buffer) {
	com_str (*read_fn)(u8* buffer, usize buflen) = w->_backing;

  com_str ret = read_fn(buffer.data, buffer.len);
  return (com_reader_ReadStrResult) {
      .valid=ret.len == buffer.len,
      .value=ret, 
     };
}

static com_reader_ReadU8Result read_u8_fn(const com_reader *w) {
	com_str (*read_fn)(u8* buffer, usize buflen) = w->_backing;
  // calls the function with a 1 long string
  u8 c;
	com_str ret = read_fn(&c, 1);
  return (com_reader_ReadU8Result) {
      .valid=ret.len == 1,
      .value=ret.data[0], 
     };
}

static u64 attr_NORETURN query_fn(attr_UNUSED const com_reader* w) {
    com_assert_unreachable_m("fn reader does not support querying for length");
}

static com_reader_ReadU8Result attr_NORETURN peek_u8_fn(attr_UNUSED const com_reader* w, attr_UNUSED usize n) {
    com_assert_unreachable_m("fn reader does not support peeking");
}

static com_streamposition_LnCol attr_NORETURN position_fn(const com_reader* w) {
    com_assert_unreachable_m("fn reader does not support querying position");
}

static void destroy_fn(com_reader* w) {
    w->_valid = false;
}

com_reader com_reader_fn_create(com_str (*read_fn)(u8* buffer, usize buflen)) {
  return (com_reader) {
      ._valid = true,
      ._flags= com_reader_FLAGS_NONE,
      ._backing = read_fn,
      ._read_str_fn = read_str_fn,
      ._read_u8_fn = read_u8_fn,
      ._query_fn = query_fn,
      ._position_fn = position_fn,
      ._peek_u8_fn= peek_u8_fn,
      ._destroy_fn = destroy_fn
  };
}

