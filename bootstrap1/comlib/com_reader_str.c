#include "com_reader_str.h"
#include "com_assert.h"
#include "com_imath.h"
#include "com_mem.h"

static usize query_fn(const com_reader *w) {
  com_reader_str_backing *backing = w->_backing;
  com_assert_m(backing->_index <= backing->_str->len,
               "buffer has already overflown");
  return backing->_str->len - backing->_index;
}

static com_reader_ReadU8Result read_u8_fn(const com_reader *w) {
  // get backing
  com_reader_str_backing *backing = w->_backing;

  // check write ok
  if (query_fn(w) == 0) {
    return (com_reader_ReadU8Result){.valid = false};
  }

  return (com_reader_ReadU8Result){
    .valid = true, 
    .value = backing->_str->data[backing->_index++]
  };
}

static com_reader_ReadResult read_str_fn(const com_reader *w, const com_str data) {
  // get backing
  com_reader_str_backing *backing = w->_backing;

  // check write ok
  usize writelen = com_imath_usize_min(query_fn(w), data.len);

  com_assert_m(query_fn(w) >= data.len, "buffer overflow");

  // update data
  com_str *dest = backing->_str;
  com_mem_move(&dest->data[backing->_index], data.data, writelen);
  backing->_index += writelen;
  // success if we were able to write all bytes
  return (com_reader_ReadResult){.valid = writelen == data.len,
                                  .written = writelen};
}

static void attr_NORETURN flush_fn(attr_UNUSED const com_reader *w) {
  com_assert_unreachable_m("str_reader does not support flushing");
}

static void destroy_fn(com_reader *w) { w->_valid = false; }

com_reader com_reader_str_create(com_str *destination, usize offset,
                                 com_reader_str_backing *backing) {
  *backing = (com_reader_str_backing){
      ._str = destination,
      ._index = offset,
  };
  return (com_reader){._valid = true,
                      ._flags = com_reader_LIMITED,
                      ._backing = backing,
                      ._read_str_fn = read_str_fn,
                      ._read_u8_fn = read_u8_fn,
                      ._query_fn = query_fn,
                      ._flush_fn = flush_fn,
                      ._destroy_fn = destroy_fn};
}
