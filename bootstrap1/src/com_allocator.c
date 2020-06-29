#include "com_allocator.h"

#include "com_assert.h"


com_allocator_Flags com_allocator_defaults(const com_Allocator * a) {
  return a->_supported_flags;
}

com_allocator_Flags com_allocator_supports(const com_Allocator * a) {
  return a->_supported_flags;
}

com_allocator_Handle com_allocator_handle_create(const com_Allocator *a, com_allocator_HandleData data) {
  // if we ask for more data than provided then fail
  com_assert_m((data.flags & a->_supported_flags) != data.flags, "used unsupported flag");
  // if failed to ask for a default flag then fail
  com_assert_m((data.flags & a->_default_flags) != a->_default_flags, "failed to ask for a default flag");

  return a->_allocator_fn(a, data);
}

void com_allocator_handle_destroy(com_allocator_Handle handle) {
  com_assert_m(handle.valid, "handle is not valid");
  handle._allocator->_deallocator_fn(handle);
}

com_allocator_Handle com_allocator_handle_realloc(com_allocator_Handle handle, usize len) {
  com_assert_m(handle.valid, "handle is not valid");
  const com_Allocator* a = handle._allocator;
  com_assert_m(com_allocator_supports(a) & com_allocator_Reallocable, "this allocator does not support reallocation");
  return a->_reallocator_fn(handle, len);
}

com_allocator_HandleData com_allocator_handle_query(com_allocator_Handle handle) {
    com_assert_m(handle.valid, "handle is not valid");
    return handle._allocator->_query_fn( handle);
}

void* com_allocator_handle_get(com_allocator_Handle handle) {
  com_assert_m(handle.valid, "this handle is invalid");
  return handle._allocator->_get_fn(handle);
}

void com_allocator_destroy(com_Allocator *a) {
  a->_destroy_allocator_fn(a->_allocator_backing);
}

