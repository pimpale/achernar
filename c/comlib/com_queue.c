#include "com_queue.h"
#include "com_vec.h"

#include "com_assert.h"

// TODO convert to circular buffer

com_queue com_queue_create(com_vec vector) {
  // it currently has zero offset
  return (com_queue) {
    ._backing = vector,
    ._offset= 0
  };
}

usize com_queue_length(const com_queue *queue) {
  com_assert_m(com_vec_length(&queue->_backing) >= queue->_offset, "queue is corrupt because the apparent length is less than 0");
  usize length = com_vec_length(&queue->_backing) - queue->_offset;
  return (usize) length;
}

void* com_queue_push(com_queue *queue, usize len) {
    if(queue->_offset < len) {
        usize expansion = com_vec_length(&queue->_backing)+len;
        com_vec_insert(&queue->_backing, 0, expansion);
        queue->_offset += expansion;
    }
    void* ptr = com_vec_get(&queue->_backing, (usize)queue->_offset - len);
    queue->_offset -= len;
    return ptr;
}

void com_queue_pop(com_queue* queue, void* data, usize len) {
  com_assert_m(len <= com_queue_length(queue), "queue underflow because trying to pop more than exists in the queue");
  com_vec_pop(&queue->_backing, data, len);
}

void *com_queue_peek(com_queue* queue, usize len) {
  com_assert_m(len <= com_queue_length(queue), "queue does not have enough elements to peek");
  return com_vec_get(&queue->_backing, com_vec_length(&queue->_backing) - len);
}

void * com_queue_get(com_queue* queue, usize loc) {
  com_assert_m(loc <= com_queue_length(queue), "queue out of bounds access");
  return com_vec_get(&queue->_backing, loc + (usize)queue->_offset);
}

com_vec com_queue_release(com_queue* queue) {
  // delete padding from beginning
  com_vec_remove(&queue->_backing, NULL, 0, (usize)queue->_offset);
  return queue->_backing;
}

void com_queue_destroy(com_queue *queue) {
  com_vec_destroy(&queue->_backing);
}

