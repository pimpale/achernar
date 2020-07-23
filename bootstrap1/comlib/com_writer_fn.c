#include "com_writer_fn.h"
#include "com_assert.h"


com_writer_WriteResult append_str_fn(const com_writer *w, const com_str data) {
	usize (*write_fn)(const com_str data) = w->_backing;

    usize ret = write_fn(data);
  return (com_writer_WriteResult) {
      .valid = ret==data.len, 
      .written=ret
     };
}

com_writer_WriteResult append_u8_fn(const com_writer *w, u8 data) {
  // calls the function with a 1 long string
	return append_str_fn(w, com_str_create(&data, 1));
}

usize attr_NORETURN query_fn(const com_writer* w) {
    com_assert_unreachable_m("fn writer does not support querying for length");
}

void attr_NORETURN flush_fn(const com_writer* w) {
    com_assert_unreachable_m("fn writer does not support flushing");
}


void destroy_fn(com_writer* w) {
    w->_valid = false;
}

com_writer com_writer_fn_create(usize (*write_fn)(const com_str data)) {
  return (com_writer) {
      ._valid = true,
      ._flags= com_writer_FLAGS_NONE,
      ._backing = write_fn,
      ._append_str_fn = append_str_fn,
      ._append_u8_fn = append_u8_fn,
      ._query_fn = query_fn,
      ._flush_fn = flush_fn,
      ._destroy_fn = destroy_fn
  };
}
