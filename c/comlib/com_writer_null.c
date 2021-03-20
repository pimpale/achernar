#include "com_writer_null.h"
#include "com_assert.h"

static usize attr_NORETURN query_fn(attr_UNUSED const com_writer *w) {
  com_assert_unreachable_m("null_writer does not support querying");
}

static com_writer_WriteResult append_u8_fn(attr_UNUSED const com_writer *w, attr_UNUSED const u8 data) {
  return (com_writer_WriteResult){.valid = true, .written = 1};
}

static com_writer_WriteResult append_str_fn(attr_UNUSED const com_writer *w, const com_str data) {
  return (com_writer_WriteResult){.valid = true, .written = data.len};
}

static void attr_NORETURN flush_fn(attr_UNUSED const com_writer *w) {
  com_assert_unreachable_m("null_writer does not support flushing");
}

static void destroy_fn(com_writer *w) { w->_valid = false; }

com_writer com_writer_null(void) {
  return (com_writer){._valid = true,
                      ._flags = com_writer_FLAGS_NONE,
                      ._backing = NULL,
                      ._append_str_fn = append_str_fn,
                      ._append_u8_fn = append_u8_fn,
                      ._query_fn = query_fn,
                      ._flush_fn = flush_fn,
                      ._destroy_fn = destroy_fn};
}
