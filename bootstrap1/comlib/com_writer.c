#include "com_writer.h"
#include "com_assert.h"

com_writer_Flags com_writer_flags(const com_writer *w) {
  com_assert_m(w->_valid, "writer is invalid");
  return w->_flags;
}

usize com_writer_append_str(const com_writer* w, const com_str data) {
  com_assert_m(w->_valid, "writer is invalid");
  return w->_append_str_fn(w, data);
}

bool com_writer_append_u8(const com_writer* w, const u8 data) {
  com_assert_m(w->_valid, "writer is invalid");
  return w->_append_u8_fn(w, data);
}

usize com_writer_query(const com_writer *w) {
  com_assert_m(w->_valid, "writer is invalid");
  com_assert_m(com_writer_flags(w) & com_writer_LIMITED, "writer doesn't support querying remaining length");
  return w->_query_fn(w);
}

void com_writer_flush(const com_writer *w) {
  com_assert_m(w->_valid, "writer is invalid");
  com_assert_m(com_writer_flags(w) & com_writer_BUFFERED, "writer doesn't support flushing");
  w->_flush_fn(w);
}

void com_writer_destroy(com_writer *w) {
  com_assert_m(w->_valid, "writer is invalid");
  w->_destroy_fn(w);
  com_assert_m(!w->_valid, "writer is still valid after destroy");
}
