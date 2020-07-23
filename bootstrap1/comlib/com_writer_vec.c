#include "com_writer_vec.h"
#include "com_assert.h"
#include "com_mem.h"

com_writer_WriteResult append_u8_fn(const com_writer *w, const u8 data) {
  com_vec *v = w->_backing;
  *com_vec_push_m(v, u8) = data;
  return (com_writer_WriteResult){.valid = true, .written = 1};
}

com_writer_WriteResult append_str_fn(const com_writer *w, const com_str data) {
  com_vec *v = w->_backing;
  com_mem_move(com_vec_push(v, data.len), data.data, data.len);
  return (com_writer_WriteResult){.valid = true, .written = data.len};
}

usize attr_NORETURN query_fn(const com_writer *w) {
  com_assert_unreachable_m("vec writer does not support querying for length");
}

void attr_NORETURN flush_fn(const com_writer *w) {
  com_assert_unreachable_m("vec writer does not support flushing");
}

void destroy_fn(com_writer *w) { w->_valid = false; }

com_writer com_writer_vec_create(com_vec *backing) {
  return (com_writer){._valid = true,
                      ._flags = com_writer_FLAGS_NONE,
                      ._backing = backing,
                      ._append_str_fn = append_str_fn,
                      ._append_u8_fn = append_u8_fn,
                      ._query_fn = query_fn,
                      ._flush_fn = flush_fn,
                      ._destroy_fn = destroy_fn};
}
