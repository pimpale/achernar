#include "com_reader_str.h"
#include "com_assert.h"
#include "com_imath.h"
#include "com_mem.h"

static usize internal_query_fn(const com_reader *w) {
  com_reader_str_backing *backing = w->_backing;
  com_assert_m(backing->_index <= backing->_str->len,
               "buffer has already overflown");
  return backing->_str->len - backing->_index;
}

static com_reader_ReadU8Result internal_u8_fn(const com_reader *w) {
  // get backing
  com_reader_str_backing *backing = w->_backing;

  // check read ok
  if (internal_query_fn(w) == 0) {
    return (com_reader_ReadU8Result){.valid = false};
  }

  return (com_reader_ReadU8Result){
      .valid = true, .value = backing->_str->data[backing->_index++]};
}

static com_reader_ReadStrResult internal_str_fn(const com_reader *w,
                                                com_str_mut buffer) {
  // get backing
  com_reader_str_backing *backing = w->_backing;

  // check read ok
  usize readlen = com_imath_usize_min(internal_query_fn(w), buffer.len);

  // update data
  com_str *dest = backing->_str;
  com_mem_move(buffer.data, &dest->data[backing->_index], readlen);
  backing->_index += readlen;
  // success if we were able to write all bytes
  return (com_reader_ReadStrResult){
      .valid = readlen == buffer.len,
      .value = (com_str_mut){.data = buffer.data, .len = readlen}};
}

// TODO implement these
static com_reader_ReadU8Result attr_NORETURN
internal_peek_u8_fn(attr_UNUSED const com_reader *w, attr_UNUSED usize n) {
  com_assert_unreachable_m("str reader does not support peeking");
}

static com_loc_Span attr_NORETURN
internal_peek_span_u8_fn(attr_UNUSED const com_reader *w) {
  com_assert_unreachable_m("str reader does not peeking span");
}

static com_loc_LnCol attr_NORETURN
internal_position_fn(attr_UNUSED const com_reader *w) {
  com_assert_unreachable_m("str reader does not support querying position");
}

static void internal_destroy_fn(com_reader *w) { w->_valid = false; }

com_reader com_reader_str_create(com_str *destination, usize offset,
                                 com_reader_str_backing *backing) {
  *backing = (com_reader_str_backing){
      ._str = destination,
      ._index = offset,
  };

  return (com_reader){._valid = true,
                      ._flags = com_reader_LIMITED,
                      ._backing = backing,
                      ._read_str_fn = internal_str_fn,
                      ._read_u8_fn = internal_u8_fn,
                      ._query_fn = internal_query_fn,
                      ._peek_span_u8_fn = internal_peek_span_u8_fn,
                      ._position_fn = internal_position_fn,
                      ._peek_u8_fn = internal_peek_u8_fn,
                      ._destroy_fn = internal_destroy_fn};
}
