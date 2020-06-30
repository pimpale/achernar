#include "com_writer.h"
#include "com_assert.h"

com_writer_Flags com_writer_defaults(const com_writer *w) {
  com_assert_m(w->_valid, "writer is invalid");
  return w->_default_flags;
}

com_writer_Flags com_writer_supports(const com_writer *w) {
  com_assert_m(w->_valid, "writer is invalid");
  return w->_supported_flags;
}

com_writer_Flags com_writer_used(const com_writer *w) {
  com_assert_m(w->_valid, "writer is invalid");
  return w->_used_flags;
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
  com_assert_m(com_writer_used(w) & com_writer_BYTES_LIMITED, "writer doesn't support querying remaining length");
  return w->_query_fn(w);
}

void com_writer_destroy(com_writer *w) {
  com_assert_m(w->_valid, "writer is invalid");
  w->_destroy_fn(w);
  com_assert_m(!w->_valid, "writer is still valid after destroy");
}
