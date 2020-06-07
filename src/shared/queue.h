#ifndef QUEUE_H
#define QUEUE_H

#include "vector.h"

// 
typedef struct Queue_s {
  Vector backing;
  int64_t offset;
} Queue;

// Creates a queue from a preinitialized vector
/// REQUIRES: `vector` is a valid vector
/// GUARANTEES: `vector` is no longer valid
/// GUARANTEES: returns a valid Queue
/// GUARANTEES: returned queue uses the same memory as `vector`
/// GUARANTEES: returned queue has the same length as `vector`
Queue queue_create(Vector vector);

// Enqueues an element with `len` bytes of memory.
/// REQUIRES: `queue` is a valid pointer to a Queue
/// GUARANTEES: until a subsequent operation to `queue`,
///             the returned pointer will point to `len` bytes of memory
void* queue_push(Queue* queue, size_t len);

// Dequeues an element with `len` bytes of memory
// If `data` is not NULL, the removed memory will be copied to `data`
/// REQUIRES: `queue` is a valid pointer to a Queue
/// REQUIRES: `data` is either null or a pointer to `len` bytes of valid memory
/// GUARANTEES: if `data` is NULL, the element will be lost
/// GUARANTEES: if `data` is not NULL, the `len` bytes from the element will be copied to `data`
/// GUARANTEES: `queue`'s length is decreased by `len` bytes
void queue_pop(Queue* queue, void* data, size_t len);

// frees all memory associated with this queue
/// REQUIRES: `queue` is a valid pointer to a Queue
/// GUARANTEES: the returned vector has as many elements as the queue
/// GUARANTEES: `queue` is no longer valid
void queue_destroy(Queue* queue);

// Returns the length of the queue's data in bytes.
/// REQUIRES: `queue` is a valid pointer to a Queue
/// GUARANTEES: returned value is length of queue's data in bytes.
size_t queue_length(const Queue* queue);

#endif
