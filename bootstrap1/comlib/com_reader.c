#include "com_reader.h"

#include "com_assert.h"

com_reader_Flags com_reader_flags(const com_reader *w) {
  com_assert_m(w->_valid, "reader is invalid");
  return w->_flags;
}

usize com_reader_read_str(const com_reader* w, com_str* data) {
  com_assert_m(w->_valid, "reader is invalid");
  return w->_read_str_fn(w, data);
}

bool com_reader_read_u8(const com_reader* w, u8* data) {
  com_assert_m(w->_valid, "reader is invalid");
  return w->_read_u8_fn(w, data);
}

bool com_reader_drop_u8(const com_reader *w) {
  u8 c;
  return com_reader_read_u8(w, &c);
}

bool com_reader_peek_u8(const com_reader* w, usize n, u8* data) {
  com_assert_m(w->_valid, "reader is invalid");
  com_assert_m(com_reader_flags(w) & com_reader_BUFFERED, "reader doesn't support peeking forward");
  return w->_peek_u8_fn(w, n, data);
}

usize com_reader_query(const com_reader *w) {
  com_assert_m(w->_valid, "reader is invalid");
  com_assert_m(com_reader_flags(w) & com_reader_LIMITED, "reader doesn't support querying remaining length");
  return w->_query_fn(w);
}

com_streamposition_LnCol com_reader_position(const com_reader *r) {
  com_assert_m(r->_valid, "reader is invalid");
  return r->_position_fn(r);
}


void com_reader_destroy(com_reader *w) {
  com_assert_m(w->_valid, "reader is invalid");
  w->_destroy_fn(w);
  com_assert_m(!w->_valid, "reader is still valid after destroy");
}

