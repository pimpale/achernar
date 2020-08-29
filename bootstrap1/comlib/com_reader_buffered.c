#include "com_reader_buffered.h"
#include "com_assert.h"
#include "com_loc.h"
#include "com_queue.h"

static com_loc_LnCol lex_incrementLn(com_loc_LnCol position) {
  return com_loc_lncol_m(com_loc_ln_m(position.ln.val + 1), com_loc_col_m(1));
}

static com_loc_LnCol lex_incrementCol(com_loc_LnCol position) {
  return com_loc_lncol_m(position.ln, com_loc_col_m(position.col.val + 1));
}

typedef struct {
  com_queue queue;
  com_reader *reader;
  com_loc_LnCol loc;
} BufferBacking;

// read raw from the backing reader
static com_reader_ReadU8Result internal_raw(const com_reader *w) {
  BufferBacking *backing = w->_backing;
  com_reader_ReadU8Result ret = com_reader_read_u8(backing->reader);
  return ret;
}

static com_reader_ReadU8Result internal_u8_fn(const com_reader *w) {
  BufferBacking *backing = w->_backing;

  com_reader_ReadU8Result ret;
  if (com_queue_len_m(&backing->queue, u8) > 0) {
    com_queue_pop_m(&backing->queue, &ret, com_reader_ReadU8Result);
  } else {
    ret = internal_raw(w);
  }
  if (ret.valid) {
    u8 c = ret.value;
    if (c == '\n') {
      backing->loc = lex_incrementLn(backing->loc);
    } else {
      backing->loc = lex_incrementCol(backing->loc);
    }
  }
  return ret;
}

static com_reader_ReadStrResult internal_str_fn(const com_reader *w,
                                                com_str_mut buffer) {
  usize i;
  bool valid = true;

  for (i = 0; i < buffer.len; i++) {
    com_reader_ReadU8Result ret = internal_u8_fn(w);
    if (!ret.valid) {
      valid = false;
      break;
    }
    buffer.data[i] = ret.value;
  }

  return (com_reader_ReadStrResult){
      .valid = valid,
      .value = (com_str_mut){.data = buffer.data, .len = i},
  };
}

static com_loc_LnCol internal_position_fn(const com_reader *w) {
  BufferBacking *backing = w->_backing;
  return backing->loc;
}

static com_reader_ReadU8Result internal_peek_u8_fn(const com_reader *w,
                                                   usize n) {
  BufferBacking *backing = w->_backing;

  com_assert_m(n > 0, "n is not 1 or more");
  // the for loop is only invoked if n > the max queue size
  for (usize i = com_queue_len_m(&backing->queue, com_reader_ReadU8Result);
       i < n; i++) {
    // enqueue the result
    *com_queue_push_m(&backing->queue, com_reader_ReadU8Result) =
        internal_raw(w);
  }

  // gives us the result n away from the beginning of the queue
  return *com_queue_get_m(
      &backing->queue,
      com_queue_len_m(&backing->queue, com_reader_ReadU8Result) - n,
      com_reader_ReadU8Result);
}

static com_loc_Span internal_peek_span_fn(const com_reader *w) {
  com_loc_LnCol loc = internal_position_fn(w);
  com_loc_LnCol nloc = loc;
  com_reader_ReadU8Result ret = internal_peek_u8_fn(w, 1);
  if (ret.valid && ret.value == '\n') {
    nloc = lex_incrementLn(loc);
  } else {
    nloc = lex_incrementCol(loc);
  }

  return com_loc_span_m(loc, nloc);
}

static u64 attr_NORETURN internal_query_fn(attr_UNUSED const com_reader *w) {
  com_assert_unreachable_m("reader does not support querying for length");
}

static void internal_destroy_fn(com_reader *w) {
  BufferBacking *b = w->_backing;
  com_queue_destroy(&b->queue);
  w->_valid = false;
}

com_reader com_reader_buffered(com_reader *r, com_allocator *a) {
  BufferBacking *b = com_allocator_handle_get(com_allocator_alloc(
      a, (com_allocator_HandleData){.len = sizeof(BufferBacking),
                                    .flags = com_allocator_defaults(a)}));

  *b = (BufferBacking){
      .reader = r,
      .queue = com_queue_create(com_vec_create(com_allocator_alloc(
          a, (com_allocator_HandleData){.len = 10,
                                        .flags = com_allocator_defaults(a) |
                                                 com_allocator_REALLOCABLE})))};

  return (com_reader){._valid = true,
                      ._flags = com_reader_BUFFERED | com_reader_POSITION,
                      ._backing = b,
                      ._read_str_fn = internal_str_fn,
                      ._read_u8_fn = internal_u8_fn,
                      ._query_fn = internal_query_fn,
                      ._peek_span_u8_fn = internal_peek_span_fn,
                      ._position_fn = internal_position_fn,
                      ._peek_u8_fn = internal_peek_u8_fn,
                      ._destroy_fn = internal_destroy_fn};
}
