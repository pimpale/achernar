#include "com_reader.h"

#include "com_assert.h"

com_reader_Flags com_reader_flags(const com_reader *w) {
  com_assert_m(w->_valid, "reader is invalid");
  return w->_flags;
}

com_reader_ReadStrResult com_reader_read_str(const com_reader* w, com_str_mut buffer) {
  com_assert_m(w->_valid, "reader is invalid");
  return w->_read_str_fn(w, buffer);
}

com_reader_ReadU8Result com_reader_read_u8(const com_reader* w) {
  com_assert_m(w->_valid, "reader is invalid");
  return w->_read_u8_fn(w);
}

void com_reader_drop_u8(const com_reader *w) {
  com_reader_read_u8(w);
}

com_reader_ReadU8Result com_reader_peek_u8(const com_reader* w, usize n) {
  com_assert_m(w->_valid, "reader is invalid");
  com_assert_m(com_reader_flags(w) & com_reader_BUFFERED, "reader doesn't support peeking forward");
  return w->_peek_u8_fn(w, n);
}

u64 com_reader_query(const com_reader *w) {
  com_assert_m(w->_valid, "reader is invalid");
  com_assert_m(com_reader_flags(w) & com_reader_LIMITED, "reader doesn't support querying remaining length");
  return w->_query_fn(w);
}

com_loc_LnCol com_reader_position(const com_reader *r) {
  com_assert_m(r->_valid, "reader is invalid");
  com_assert_m(com_reader_flags(r) & com_reader_POSITION, "reader doesn't support querying position");
  return r->_position_fn(r);
}


void com_reader_destroy(com_reader *w) {
  com_assert_m(w->_valid, "reader is invalid");
  w->_destroy_fn(w);
  com_assert_m(!w->_valid, "reader is still valid after destroy");
}
