#include "halo_linear_buffer.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

void halo_linear_buffer_init(struct halo_linear_buffer* buffer, size_t retain_capacity) {
  memset(buffer, 0, sizeof(*buffer));
  buffer->retain_capacity = retain_capacity;
}

void halo_linear_buffer_destroy(struct halo_linear_buffer* buffer) {
  free(buffer->data);
  memset(buffer, 0, sizeof(*buffer));
}

int halo_linear_buffer_append(struct halo_linear_buffer* buffer, const void* data, size_t length) {
  size_t required;

  if (length == 0)
    return 0;
  if (length > SIZE_MAX - buffer->length)
    return -1;
  required = buffer->length + length;
  if (required > buffer->capacity) {
    size_t capacity = buffer->capacity;
    char* next;

    if (capacity == 0) {
      capacity = 4096;
      if (buffer->retain_capacity != 0 && buffer->retain_capacity < capacity)
        capacity = buffer->retain_capacity;
    }
    while (capacity < required) {
      if (capacity > SIZE_MAX / 2) {
        capacity = required;
        break;
      }
      capacity *= 2;
    }
    next = malloc(capacity);
    if (next == NULL)
      return -1;
    if (buffer->length != 0)
      memcpy(next, buffer->data + buffer->offset, buffer->length);
    free(buffer->data);
    buffer->data = next;
    buffer->capacity = capacity;
    buffer->offset = 0;
  } else if (length > buffer->capacity - buffer->offset - buffer->length) {
    memmove(buffer->data, buffer->data + buffer->offset, buffer->length);
    buffer->offset = 0;
  }

  memcpy(buffer->data + buffer->offset + buffer->length, data, length);
  buffer->length = required;
  return 0;
}

size_t halo_linear_buffer_read(struct halo_linear_buffer* buffer, void* data, size_t length) {
  if (length > buffer->length)
    length = buffer->length;
  if (length == 0)
    return 0;
  memcpy(data, buffer->data + buffer->offset, length);
  buffer->offset += length;
  buffer->length -= length;
  if (buffer->length == 0) {
    buffer->offset = 0;
    if (buffer->capacity > buffer->retain_capacity) {
      free(buffer->data);
      buffer->data = NULL;
      buffer->capacity = 0;
    }
  }
  return length;
}
