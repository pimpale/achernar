#include "com_allocator_passthrough.h"

#include "com_assert.h"

static com_allocator_Handle handle_allocator_fn(const com_allocator *allocator,
                                                com_allocator_HandleData data) {

  com_allocator_passthrough_Backing *backing = allocator->_backing;

  // do checks
  com_assert_m(backing->_input_used,
               "already allocated from this passthrough allocator");
  com_assert_m(data.len <= backing->_original_input_len,
               "asked for more memory than originally provided to this "
               "passthrough allocator");

  // set used
  backing->_input_used = true;
  backing->_input_len = data.len;

  // return handle
  return (com_allocator_Handle){._allocator = allocator,
                                // id doesn't really matter
                                ._id = 0,
                                .valid = true};
}

static void handle_deallocator_fn(com_allocator_Handle handle) {
  // get allocator and backing
  const com_allocator *allocator = handle._allocator;
  com_allocator_passthrough_Backing *backing = allocator->_backing;

  // perform checks
  com_assert_m(!backing->_input_used,
               "haven't yet allocated from this passthrough allocator, so "
               "impossible to dealloc yet");

  backing->_input_used=false;
}

static com_allocator_Handle handle_reallocator_fn(com_allocator_Handle handle,
                                                  usize len) {

  // get allocator and backing
  const com_allocator *allocator = handle._allocator;
  com_allocator_passthrough_Backing *backing = allocator->_backing;

  // check that we've allocated an id yet
  com_assert_m(backing->_input_used, "haven't allocated a handle yet");

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

static void *handle_get_fn(const com_allocator_Handle handle) {
  // get allocator and backing
  const com_allocator *allocator = handle._allocator;
  com_allocator_passthrough_Backing *backing = allocator->_backing;

  // perform checks
  com_assert_m(!backing->_input_used,
               "haven't yet allocated from this passthrough allocator, so "
               "impossible to get yet");

  return backing->_input_ptr;
}

static com_allocator_HandleData
handle_query_fn(const com_allocator_Handle handle) {
  // get allocator and backing
  const com_allocator *allocator = handle._allocator;
  com_allocator_passthrough_Backing *backing = allocator->_backing;

  // perform checks
  com_assert_m(!backing->_input_used,
               "haven't yet allocated from this passthrough allocator, so "
               "impossible to get yet");

  return (com_allocator_HandleData){.len = backing->_input_len,
                                    .flags = backing->_input_flags};
}

static void destroy_fn(com_allocator *allocator) {
  allocator->_valid = false;
}

com_allocator
com_allocator_passthrough(void *ptr, usize len,
                          com_allocator_passthrough_Backing *backing_storage) {
  *backing_storage =
      (com_allocator_passthrough_Backing){._input_used = false,
                                          ._input_ptr = ptr,
                                          ._original_input_len = len,
                                          ._input_len = len};
  return (com_allocator){
      // user is responsible for cleaning up data
      ._valid = true,
      ._default_flags = com_allocator_REALLOCABLE,
      ._supported_flags = com_allocator_REALLOCABLE,
      ._backing = backing_storage,
      ._allocator_fn = handle_allocator_fn,
      ._deallocator_fn = handle_deallocator_fn,
      ._reallocator_fn = handle_reallocator_fn,
      ._get_fn = handle_get_fn,
      ._query_fn = handle_query_fn,
      ._destroy_allocator_fn = destroy_fn};
}
