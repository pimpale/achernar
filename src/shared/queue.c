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
  return vec_length(&queue->backing) - queue->offset;
}

// 
void* queue_push(Queue *queue, size_t len) {
    if(queue->offset - len < 0) {
        size_t expansion = vec_length(&queue->backing)+len;
        vec_insert(&queue->backing, 0, expansion);
        queue->offset += expansion;
    }
    void* ptr = vec_get(&queue->backing, queue->offset - len);
    queue->offset -= len;
    return ptr;
}

void queue_pop(Queue* queue, void* data, size_t len) {
  assert(len <= queue_length(queue));
  vec_pop(&queue->backing, data, len);
}

void queue_destroy(Queue* queue) {
  vec_destroy(&queue->backing);
}
