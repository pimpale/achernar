#ifndef COM_QUEUE_H
#define COM_QUEUE_H

// creates a circular buffer based queue based on a vector

#include "com_define.h"
#include "com_allocator.h"

typedef struct {
  usize _length;
  usize _capacity;
  usize _length_to_shrink;
  com_allocator_Handle _handle;
  void *_data;
} com_queue;

// Creates a com_queue from a preinitialized vector
/// REQUIRES: `vector` is a valid vector
/// GUARANTEES: `vector` is no longer valid
/// GUARANTEES: returns a valid com_queue
/// GUARANTEES: returned com_queue uses the same memory as `vector`
/// GUARANTEES: returned com_queue has the same length as `vector`
com_queue com_queue_create(com_vec vector);

// com_Enqueues an element with `len` bytes of memory.
/// REQUIRES: `queue` is a valid pointer to a com_queue
/// GUARANTEES: until a subsequent operation to `queue`,
///             the returned pointer will point to `len` bytes of memory
void *queue_push(com_queue *queue, usize len);

// com_Dequeues an element with `len` bytes of memory
// If `data` is not NULL, the removed memory will be copied to `data`
/// REQUIRES: `queue` is a valid pointer to a com_queue
/// REQUIRES: `len` is less than or equal to the length of `queue`
/// REQUIRES: `data` is either null or a pointer to `len` bytes of valid memory
/// GUARANTEES: if `data` is NULL, the element will be lost
/// GUARANTEES: if `data` is not NULL, the `len` bytes from the element will be copied to `data`
/// GUARANTEES: `queue`'s length is decreased by `len` bytes
void com_queue_pop(com_queue *queue, void* data, usize len);

// Peeks at the next element with `len` bytes
/// REQUIRES: `queue` is a valid pointer to a com_queue
/// REQUIRES: len is less than or equal to the length of `queue`
/// GUARANTEES: a pointer will be returned to the last `len` bytes of `queue`
/// GUARANTEES: this pointer is valid till the next operation on `queue`
void *com_queue_peek(com_queue *queue, usize len); 

// returns a pointer to the loc'th byte of com_queue
/// REQUIRES: `queue` is a valid pointer to a com_queue
/// REQUIRES: `loc` is less than `queue`'s length
/// GUARANTEES: a pointer will be returned to the `loc`'th byte of `queue`
/// GUARANTEES: this pointer will be valid till the next operation on `queue`
void *com_queue_get(com_queue *queue, usize loc);

// destroys the com_queue
/// REQUIRES: `queue` is a valid pointer to a com_queue
/// GUARANTEES: `queue`, and any pointers to its elements, are no longer valid
/// GUARANTEES: memory held by `queue` will be released
void com_queue_destroy(com_queue *com_queue);

// Returns the length of the com_queue's data in bytes.
/// REQUIRES: `queue` is a valid pointer to a com_queue
/// GUARANTEES: returned value is length of com_queue's data in bytes.
usize com_queue_length(const com_queue *queue);

// Macros to help work with com_queues

#define com_queue_push_m(com_queue, type) ((type *)com_queue_push((com_queue), sizeof(type)))
#define com_queue_peek_m(com_queue, type) ((type *)com_queue_peek((com_queue), sizeof(type)))
#define com_queue_get_m(com_queue, n, type) ((type *)com_queue_get((com_queue), (n)*sizeof(type)))
#define com_queue_pop_m(com_queue, data, type) (com_queue_pop((com_queue),(data), sizeof(type)))
#define com_queue_len_m(com_queue, type) (com_queue_length((com_queue)) / sizeof(type))


#endif
