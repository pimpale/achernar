#include "queue.h"
#include "vector.h"

Queue queue_create(Vector vector) {
  // it currently has zer
  return (Queue) {
    .backing = vector,
    .offset= 0
  };
}

size_t queue_length(const Queue *queue) {
  int64_t length = (int64_t)vec_length(&queue->backing) - queue->offset;
  assert(length >= 0);
  return (size_t) length;
}

// 
void* queue_push(Queue *queue, size_t len) {
    if(queue->offset - (int64_t)len < 0) {
        size_t expansion = vec_length(&queue->backing)+len;
        vec_insert(&queue->backing, 0, expansion);
        queue->offset += expansion;
    }
    void* ptr = vec_get(&queue->backing, (size_t)queue->offset - len);
    queue->offset -= len;
    return ptr;
}

void queue_pop(Queue* queue, void* data, size_t len) {
  assert(len <= queue_length(queue));
  vec_pop(&queue->backing, data, len);
}

void *queue_peek(Queue* queue, size_t len) {
  assert(len <= queue_length(queue));
  return vec_get(&queue->backing, vec_length(&queue->backing) - len);
}

void * queue_get(Queue* queue, size_t loc) {
  assert(loc <= queue_length(queue));
  return vec_get(&queue->backing, loc + (size_t)queue->offset);
}

Vector queue_release(Queue* queue) {
  // delete padding from beginning
  vec_remove(&queue->backing, NULL, 0, (size_t)queue->offset);
  return queue->backing;
}

void queue_destroy(Queue *queue) {
  vec_destroy(&queue->backing);
}

