#include "com_assert.h"
#include "com_define.h"
#include "com_os_allocator.h"
#include "com_os_exit.h"
#include "com_os_iostream.h"
#include "com_os_time.h"

// now include the c standard library functions
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// com_os_exit

void attr_NORETURN com_os_exit_panic() { abort(); }

void attr_NORETURN com_os_exit(i32 code) { exit(code); }

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

static com_loc_LnCol attr_NORETURN 
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

// com_os_iostream

com_reader com_os_iostream_in(void) {
	return file_read_create(stdin);
}

com_writer com_os_iostream_out(void) {
	return file_append_create(stdout);
}

com_writer com_os_iostream_err(void) {
	return file_append_create(stderr);
}


// MEMORY

typedef struct {
  void *ptr;
  bool valid;
  com_allocator_HandleData data;
} AllocEntry;

typedef struct Stdallocator_s {
  // this is essentially a vector of
  AllocEntry *ptrs;
  usize ptrs_len;
  usize ptrs_cap;
} Stdallocator;

static usize push_entry(Stdallocator *b, void *entry, com_allocator_HandleData data) {
  // The system has a null zero range
  com_assert_m(b->ptrs_cap > 0, "internal allocator vector is corrupt");
  // Ensure that there is enough room for the allocation
  if (b->ptrs_len + 1 >= b->ptrs_cap) {
    b->ptrs_cap = b->ptrs_cap * 2;
    b->ptrs = realloc(b->ptrs, b->ptrs_cap * sizeof(AllocEntry));
  }
  // Add to the top of the stack
  usize index = b->ptrs_len;
  b->ptrs_len++;
  b->ptrs[index] = (AllocEntry){
      .valid = true,
      .ptr = entry,
      .data = data,
  };
  // Increment the length pointer
  return index;
}

static Stdallocator *std_create() {
  Stdallocator *sa = malloc(sizeof(Stdallocator));
  com_assert_m(sa != NULL, "failed to allocate Stdallocator object");

  void *ptr = malloc(2 * sizeof(AllocEntry));
  com_assert_m(ptr != NULL, "failed to allocate internal Stdallocator vector");

  // allocate once to form the standard allocator
  sa->ptrs = ptr;
  sa->ptrs_cap = 2;
  sa->ptrs_len = 1;
  // the null pointer
  sa->ptrs[0] = (AllocEntry){.ptr = NULL, .valid = true};
  return sa;
}

static  void std_destroy(Stdallocator *backing) {
  // com_os_mem_dealloc all uncom_os_mem_deallocd things
  for (usize i = 0; i < backing->ptrs_len; i++) {
    if (backing->ptrs[i].valid) {
      free(backing->ptrs[i].ptr);
    }
  }
  // com_os_mem_dealloc the array
  free(backing->ptrs);
  // com_os_mem_dealloc the allocator itself
  free(backing);
}

// Shim methods
static com_allocator_Handle std_allocator_fn(const com_allocator *allocator,
                                             com_allocator_HandleData data) {
  Stdallocator *backing = allocator->_backing;

  if (data.len == 0) {
    // return the dummy null entry
    return (com_allocator_Handle){._id = 0, .valid = true};
  }

  void *ptr = malloc(data.len);
  usize index = push_entry(backing, ptr, data);
  return (com_allocator_Handle){._id = index, .valid = true};
}

static void std_deallocator_fn(com_allocator_Handle id) {
  Stdallocator *backing = id._allocator->_backing;
  com_assert_m(id._id < backing->ptrs_len, "_id is out of bounds");
  AllocEntry *ae = &backing->ptrs[id._id];
  ae->valid = false;
  free(ae->ptr);
}

// normalize realloc behavior
static com_allocator_Handle std_reallocator_fn(com_allocator_Handle id,
                                               usize size) {
  Stdallocator *backing = id._allocator->_backing;

  // deallocate if we have resized to zero bytes
  if (size == 0) {
    std_deallocator_fn(id);
    return (com_allocator_Handle){._id = 0, .valid = true};
  } 

	// if it was a dummy pointer then we just allocate
  if (id._id == 0) {
    return std_allocator_fn(
        id._allocator,
        (com_allocator_HandleData){
            .len = size, 
            .flags = com_allocator_defaults(id._allocator)
        });
  }

  AllocEntry *ae = &backing->ptrs[id._id];
  void *ret = realloc(ae->ptr, size);
  if (ret == NULL) {
    // realloc failed, old handle is good though
    return (com_allocator_Handle){.valid = false};
  }

	// update alloc entry
  ae->ptr = ret;
  ae->data.len = size;

	// return valid handle
  return (com_allocator_Handle){._id = id._id, .valid = true};
}

static void *std_get_fn(const com_allocator_Handle id) {
  Stdallocator *backing = id._allocator->_backing;
  com_assert_m(id._id < backing->ptrs_len, "_id is out of bounds");
  return backing->ptrs[id._id].ptr;
}

static com_allocator_HandleData 
std_query_fn(const com_allocator_Handle id) {
  Stdallocator *backing = id._allocator->_backing;
  com_assert_m(id._id < backing->ptrs_len, "_id is out of bounds");
  return backing->ptrs[id._id].data;
}

static void std_destroy_allocator_fn(com_allocator *allocator) {
  std_destroy((Stdallocator *)allocator->_backing);
}

// allocator constant struct variable
com_allocator com_os_allocator(void) {
  return (com_allocator){._valid = true,
                         ._default_flags = com_allocator_NOLEAK,
                         ._supported_flags =
                             com_allocator_NOLEAK | com_allocator_REALLOCABLE,
                         // create the allocator
                         ._backing = std_create(),
                         // Set functions
                         ._allocator_fn = std_allocator_fn,
                         ._deallocator_fn = std_deallocator_fn,
                         ._reallocator_fn = std_reallocator_fn,
                         ._get_fn = std_get_fn,
                         ._query_fn = std_query_fn,
                         ._destroy_allocator_fn = std_destroy_allocator_fn};
}
