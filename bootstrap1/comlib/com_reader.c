#include "com_reader.h"

#include "com_assert.h"

com_reader_Flags com_reader_flags(const com_reader *r) {
  com_assert_m(r->_valid, "reader is invalid");
  return r->_flags;
}

com_reader_ReadStrResult com_reader_read_str(const com_reader* r, com_str_mut buffer) {
  com_assert_m(r->_valid, "reader is invalid");
  return r->_read_str_fn(r, buffer);
}

com_reader_ReadU8Result com_reader_read_u8(const com_reader* r) {
  com_assert_m(r->_valid, "reader is invalid");
  return r->_read_u8_fn(r);
}

void com_reader_drop_u8(const com_reader *r) {
  com_reader_read_u8(r);
}

com_reader_ReadU8Result com_reader_peek_u8(const com_reader* r, usize n) {
  com_assert_m(r->_valid, "reader is invalid");
  com_assert_m(com_reader_flags(r) & com_reader_BUFFERED, "reader doesn't support peeking forward");
  return r->_peek_u8_fn(r, n);
}

com_loc_Span com_reader_peek_span_u8(const com_reader* r, usize n) {
  com_assert_m(r->_valid, "reader is invalid");
  com_assert_m(com_reader_flags(r) & com_reader_BUFFERED, "reader doesn't support peeking forward");
  com_assert_m(com_reader_flags(r) & com_reader_POSITION, "reader doesn't support querying position");
  return r->_peek_span_u8_fn(r, n);
}

u64 com_reader_query(const com_reader *r) {
  com_assert_m(r->_valid, "reader is invalid");
  com_assert_m(com_reader_flags(r) & com_reader_LIMITED, "reader doesn't support querying remaining length");
  return r->_query_fn(r);
}

com_loc_LnCol com_reader_position(const com_reader *r) {
  com_assert_m(r->_valid, "reader is invalid");
  com_assert_m(com_reader_flags(r) & com_reader_POSITION, "reader doesn't support querying position");
  return r->_position_fn(r);
}

void com_reader_destroy(com_reader *r) {
  com_assert_m(r->_valid, "reader is invalid");
  r->_destroy_fn(r);
  com_assert_m(!r->_valid, "reader is still valid after destroy");
}
