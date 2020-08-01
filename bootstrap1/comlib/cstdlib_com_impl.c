#include "com_assert.h"
#include "com_define.h"
#include "com_os_exit.h"
#include "com_os_iostream.h"
#include "com_os_mem_alloc.h"
#include "com_os_time.h"

// now include the c standard library functions
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// com_os_exit

void attr_NORETURN com_os_exit_panic() { abort(); }

void attr_NORETURN com_os_exit(i32 code) { exit(code); }

// com_os_mem_alloc

void *com_os_mem_alloc(usize size) { return malloc(size); }

void com_os_mem_dealloc(void *ptr) { free(ptr); }

void *com_os_mem_realloc(void *src, usize nlen) { return realloc(src, nlen); }

// com_os_iostream

usize com_os_iostream_write_err(const com_str str) {
  return fwrite(str.data, sizeof(u8), str.len, stderr);
}

usize com_os_iostream_write_out(const com_str str) {
  return fwrite(str.data, sizeof(u8), str.len, stdout);
}

// com_os_time
u64 com_os_time_monotonic() { return (u64)clock(); }

u64 com_os_time_unix() { return (u64)time(NULL); }

// IOSTREAM READER

static com_reader_ReadStrResult file_read_str_fn(const com_reader *w,
                                                 com_str_mut buffer) {
  FILE *file = w->_backing;
  usize read = fread(buffer.data, 1, buffer.len, file);
  return (com_reader_ReadStrResult){
      .valid = read == buffer.len,
      .value = (com_str_mut){.data = buffer.data, .len = read},
  };
}

static com_reader_ReadU8Result file_read_u8_fn(const com_reader *w) {
  FILE *file = w->_backing;
  // calls the function with a 1 long string
  int c = fgetc(file);
  if (c == EOF) {
    return (com_reader_ReadU8Result){.valid = false};
  } else {
    return (com_reader_ReadU8Result){.valid = true, .value = (u8)c};
  }
}

static u64 attr_NORETURN file_read_query_fn(attr_UNUSED const com_reader *w) {
  com_assert_unreachable_m("file reader does not support querying for length");
}

static com_reader_ReadU8Result attr_NORETURN
file_read_peek_u8_fn(attr_UNUSED const com_reader *w, attr_UNUSED usize n) {
  com_assert_unreachable_m("file reader does not support peeking");
}

static com_streamposition_LnCol attr_NORETURN 
file_read_position_fn(attr_UNUSED const com_reader *w) {
  com_assert_unreachable_m("file reader does not support querying position");
}

static void file_read_destroy_fn(com_reader *w) { w->_valid = false; }

static com_reader file_read_create(FILE* file) {
  return (com_reader){._valid = true,
                      ._flags = com_reader_FLAGS_NONE,
                      ._backing = file,
                      ._read_str_fn = file_read_str_fn,
                      ._read_u8_fn = file_read_u8_fn,
                      ._query_fn = file_read_query_fn,
                      ._position_fn = file_read_position_fn,
                      ._peek_u8_fn = file_read_peek_u8_fn,
                      ._destroy_fn = file_read_destroy_fn};
}

// IOSTREAM WRITER

static com_writer_WriteResult file_append_str_fn(const com_writer *w, com_str data) {
	FILE* file = w->_backing;
  usize ret = fwrite(data.data, 1, data.len, file);
  return (com_writer_WriteResult) {
      .valid = ret==data.len, 
      .written=ret
  };
}

static com_writer_WriteResult file_append_u8_fn(const com_writer *w, u8 data) {
	FILE* file = w->_backing;
	int ret = fputc(data, file);
	bool successful = ret !=EOF;
	return (com_writer_WriteResult) {
    	.valid = successful,
    	.written=successful? 1 : 0
	};
}

static usize attr_NORETURN file_append_query_fn(attr_UNUSED const com_writer* w) {
    com_assert_unreachable_m("fn writer does not support querying for length");
}

static void attr_NORETURN file_append_flush_fn(attr_UNUSED const com_writer* w) {
    com_assert_unreachable_m("fn writer does not support flushing");
}

static void file_append_destroy_fn(com_writer* w) {
    w->_valid = false;
}

static com_writer file_append_create(FILE* file) {
  return (com_writer) {
      ._valid = true,
      ._flags= com_writer_FLAGS_NONE,
      ._backing = file,
      ._append_str_fn = file_append_str_fn,
      ._append_u8_fn = file_append_u8_fn,
      ._query_fn = file_append_query_fn,
      ._flush_fn = file_append_flush_fn,
      ._destroy_fn = file_append_destroy_fn
  };
}

// Actual readers & writers

com_reader com_os_iostream_in(void) {
	return file_read_create(stdin);
}

com_writer com_os_iostream_out(void) {
	return file_append_create(stdout);
}

com_writer com_os_iostream_err(void) {
	return file_append_create(stderr);
}
