#include "com_allocator_passthrough.h"

#include "com_assert.h"

static com_allocator_Handle 
handle_allocator_fn(const com_Allocator* allocator, com_allocator_HandleData data) {

  com_allocator_passthrough_Backing *backing = allocator->_allocator_backing;

  // do checks
  com_assert_m(backing->_allocator_valid, "allocator is invalid");
  com_assert_m(backing->_input_used, "already allocated from this passthrough allocator");
  com_assert_m(backing->_input_valid, "input is already invalid somehow, despite the input not being used (corruption?)");
  com_assert_m(data.len <= backing->_original_input_len, "asked for more memory than originally provided to this passthrough allocator");

  // set used
  backing->_input_used = true;
  backing->_input_len = data.len;

  // return handle
  return (com_allocator_Handle) {
      ._allocator = allocator,
      // id doesn't really matter
      ._id = 0,
      .valid = true
  };
}

void handle_deallocator_fn(const com_allocator_Handle handle) {
  // get allocator and backing
  const com_Allocator* allocator = handle._allocator;
  com_allocator_passthrough_Backing *backing = allocator->_allocator_backing;

  // perform checks
  com_assert_m(backing->_allocator_valid, "allocator is invalid");
  com_assert_m(!backing->_input_used, "haven't yet allocated from this passthrough allocator, so impossible to dealloc yet");
  com_assert_m(backing->_input_valid, "already deallocated handle");

  // invalidate input
  backing->_input_valid = false;
}


static com_allocator_Handle
handle_reallocator_fn(com_allocator_Handle handle, usize len) {

  // get allocator and backing
  const com_Allocator* allocator = handle._allocator;
  com_allocator_passthrough_Backing *backing = allocator->_allocator_backing;

  // check allocator is valid 
  com_assert_m(backing->_allocator_valid, "allocator is invalid");

  // check that we've allocated an id yet
  com_assert_m(backing->_input_used, "haven't allocated a handle yet");

  // check that we haven't deallocated the pointer
  com_assert_m(backing->_input_valid, "already deallocated handle");

  // if it's less memory than we were originally provided, it's OK to reallocate
  if (len <= backing->_original_input_len) {
    // set the new len
    backing->_input_len = len;
    // return the same handle
    return handle;
  } else {
    // return a invalid handle if asked for more memory than originally provided
    return (com_allocator_Handle){
        ._allocator = allocator,
        // don't really care what the ID is
        ._id = 0,
        // reallocations will always fail
        .valid = false,
    };
  }
}

static void* handle_get_fn(const com_allocator_Handle handle) {
  // get allocator and backing
  const com_Allocator* allocator = handle._allocator;
  com_allocator_passthrough_Backing *backing = allocator->_allocator_backing;

  // perform checks
  com_assert_m(backing->_allocator_valid, "allocator is invalid");
  com_assert_m(!backing->_input_used, "haven't yet allocated from this passthrough allocator, so impossible to get yet");
  com_assert_m(backing->_input_valid, "already deallocated handle");

  return backing->_input_ptr;
}

static void
destroy_fn(com_Allocator *allocator) {
  com_allocator_passthrough_Backing *backing = allocator->_allocator_backing;

  com_assert_m(backing->_allocator_valid, "allocator is already invalid");
  backing->_allocator_valid = false;
}

com_Allocator
com_allocator_passthrough(void *ptr, usize len,
                          com_allocator_passthrough_Backing *backing_storage) {
  *backing_storage =
      (com_allocator_passthrough_Backing){._used = false,
                                          ._input_ptr = ptr,
                                          ._original_input_len = len,
                                          ._input_len = len};
  return (com_Allocator) {
    // user is responsible for cleaning up data
    ._default_flags = com_allocator_Persistent,
    ._supported_flags = com_allocator_Persistent,
    ._allocator_backing = backing_storage,
  }
