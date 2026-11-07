#ifndef OCAML_HALO_LINEAR_BUFFER_H
#define OCAML_HALO_LINEAR_BUFFER_H

#include <stddef.h>

/* Single-owner byte queue, independent of the HTTP/TLS runtime. Do not copy
 * an initialized buffer or access its storage directly. */
struct halo_linear_buffer {
  char* data;
  size_t capacity;
  size_t offset;
  size_t length;
  size_t retain_capacity;
};

/* Initializes without allocating. retain_capacity limits storage kept when
 * empty, not live data: larger bursts grow on demand and are freed on drain.
 * A zero-initialized buffer is valid and retains no storage. Initialize only
 * fresh or destroyed buffers; destroy a live buffer before reinitializing. */
void halo_linear_buffer_init(struct halo_linear_buffer* buffer, size_t retain_capacity);
void halo_linear_buffer_destroy(struct halo_linear_buffer* buffer);

/* Append copies bytes; returns -1 on allocation failure or size overflow,
 * leaving the queue unchanged. Consumed space is compacted only when the tail
 * has insufficient room. Source must contain length readable bytes and must
 * not alias the buffer's storage. */
int halo_linear_buffer_append(struct halo_linear_buffer* buffer, const void* data, size_t length);

/* Copies and consumes up to length bytes, returning the number copied.
 * Destination must have room for min(length, buffer->length) bytes and must
 * not alias the buffer's storage. Zero-length operations
 * accept NULL source/destination. */
size_t halo_linear_buffer_read(struct halo_linear_buffer* buffer, void* data, size_t length);

#endif
