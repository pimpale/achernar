#include "com_writer_str.h"
#include "com_mem.h"
#include "com_imath.h"
#include "com_assert.h"

usize query_fn(const com_writer *w) {
  com_writer_str_backing *backing = w->_backing;
  com_assert_m(backing->_index <= backing->_str->len, "buffer has already overflown");
  return backing->_str->len - backing->_index;
}

bool append_u8_fn(const com_writer *w, const u8 data) {
  // get backing
  com_writer_str_backing *backing = w->_backing;

  // check write ok
  if(query_fn(w) == 0) {
    return false;
  }

  // update data
  backing->_str->data[backing->_index] = data;
  backing->_index++;
  return true;
}

usize append_str_fn(const com_writer *w, const com_str data) {
  // get backing
  com_writer_str_backing *backing = w->_backing;

  // check write ok
  usize readlen = com_imath_usize_min(query_fn(w), data.len);

  com_assert_m(query_fn(w) >= data.len, "buffer overflow");

  // update data
  com_str* dest = backing->_str;
  com_mem_move(&dest->data[backing->_index], data.data, readlen);
  backing->_index += readlen;

  return readlen;
}

void attr_NORETURN flush_fn(attr_UNUSED const com_writer *w) {
  com_assert_unreachable_m("str_writer does not support flushing");
}

void destroy_fn(com_writer *w) {
  w->_valid = false;
}


com_writer com_writer_str_create(com_str* destination, usize offset, com_writer_str_backing *backing) {
  *backing = (com_writer_str_backing) {
      ._str = destination,
      ._index = offset,
  };
  return (com_writer) {
      ._valid = true,
      ._default_flags = com_writer_LIMITED,
      ._supported_flags = com_writer_LIMITED,
      ._used_flags = com_writer_LIMITED,
      ._backing = backing,
      ._append_str_fn = append_str_fn,
      ._append_u8_fn = append_u8_fn,
      ._query_fn = query_fn,
      ._flush_fn = flush_fn,
      ._destroy_fn = destroy_fn
  };
}
