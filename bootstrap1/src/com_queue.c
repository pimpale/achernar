#include "com_queue.h"
#include "com_vec.h"

#include "com_assert.h"

com_Queue queue_create(com_Vec vector) {
  // it currently has zer
  return (com_Queue) {
    ._backing = vector,
    ._offset= 0
  };
}

size_t queue_length(const com_Queue *queue) {
  int64_t length = (int64_t)com_vec_length(&queue->_backing) - queue->_offset;
  com_assert_m(length >= 0, "queue is corrupt because the apparent length is less than 0");
  return (size_t) length;
}

// 
void* queue_push(com_Queue *queue, size_t len) {
    if(queue->_offset - (int64_t)len < 0) {
        size_t expansion = com_vec_length(&queue->_backing)+len;
        com_vec_insert(&queue->_backing, 0, expansion);
        queue->_offset += expansion;
    }
    void* ptr = com_vec_get(&queue->_backing, (size_t)queue->_offset - len);
    queue->_offset -= len;
    return ptr;
}

void queue_pop(com_Queue* queue, void* data, size_t len) {
  com_assert_m(len <= queue_length(queue), "queue underflow because trying to pop more than exists in the queue");
  com_vec_pop(&queue->_backing, data, len);
}

void *queue_peek(com_Queue* queue, size_t len) {
  com_assert_m(len <= queue_length(queue), "queue does not have enough elements to peek");
  return com_vec_get(&queue->_backing, com_vec_length(&queue->_backing) - len);
}

void * queue_get(com_Queue* queue, size_t loc) {
  com_assert_m(loc <= queue_length(queue), "queue out of bounds access");
  return com_vec_get(&queue->_backing, loc + (size_t)queue->_offset);
}

com_Vec queue_release(com_Queue* queue) {
  // delete padding from beginning
  com_vec_remove(&queue->_backing, NULL, 0, (size_t)queue->_offset);
  return queue->_backing;
}

void queue_destroy(com_Queue *queue) {
  com_vec_destroy(&queue->_backing);
}

