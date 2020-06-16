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
/// REQUIRES: `len` is less than or equal to the length of `queue`
/// REQUIRES: `data` is either null or a pointer to `len` bytes of valid memory
/// GUARANTEES: if `data` is NULL, the element will be lost
/// GUARANTEES: if `data` is not NULL, the `len` bytes from the element will be copied to `data`
/// GUARANTEES: `queue`'s length is decreased by `len` bytes
void queue_pop(Queue* queue, void* data, size_t len);

// Peeks at the next element with `len` bytes
/// REQUIRES: `queue` is a valid pointer to a ~ueue
/// REQUIRES: len is less than or equal to the length of `queue`
/// GUARANTEES: a pointer will be returned to the last `len` bytes of `queue`
/// GUARANTEES: this pointer is valid till the next operation on `queue`
void* queue_peek(Queue* queue, size_t len); 

// returns a pointer to the loc'th byte of queue
/// REQUIRES: `queue` is a valid pointer to a Queue
/// REQUIRES: `loc` is less than `queue`'s length
/// GUARANTEES: a pointer will be returned to the `loc`'th byte of `queue`
/// GUARANTEES: this pointer will be valid till the next operation on `queue`
void* queue_get(Queue* queue, size_t loc);

// releases the backing memory of the queue as a vector
/// REQUIRES: `queue` is a valid pointer to a Queue
/// GUARANTEES: the returned vector has the same length as `queue`
/// GUARANTEES: `queue`, and any pointers to its elements, are no longer valid
/// GUARANTEES: the contents of `queue` will be the pushed elements in order of their insertion
Vector queue_release(Queue* queue);

// destroys the queue
/// REQUIRES: `queue` is a valid pointer to a Queue
/// GUARANTEES: `queue`, and any pointers to its elements, are no longer valid
/// GUARANTEES: memory held by `queue` will be released
void queue_destroy(Queue *queue);

// Returns the length of the queue's data in bytes.
/// REQUIRES: `queue` is a valid pointer to a Queue
/// GUARANTEES: returned value is length of queue's data in bytes.
size_t queue_length(const Queue* queue);

// Macros to help work with queues

#define QUEUE_PUSH(queue, type) ((type *)queue_push((queue), sizeof(type)))
#define QUEUE_PEEK(queue, type) ((type *)queue_peek((queue), sizeof(type)))
#define QUEUE_GET(queue, n, type) ((type *)queue_get((queue), (n)*sizeof(type)))
#define QUEUE_POP(queue, data, type) (queue_pop((queue),(data), sizeof(type)))
#define QUEUE_LEN(queue, type) (queue_length((queue)) / sizeof(type))


#endif
