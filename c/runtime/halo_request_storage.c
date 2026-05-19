#include "halo_runtime_internal.h"

#define HALO_REQUEST_STORAGE_DEFAULT_BLOCK_SIZE 1024

static size_t halo_request_storage_align_up(size_t value, size_t align) {
  size_t mask;

  if (align <= 1)
    return value;

  mask = align - 1;
  return (value + mask) & ~mask;
}

static struct halo_request_storage_block* halo_request_storage_block_new(size_t cap) {
  struct halo_request_storage_block* block;

  if (cap > SIZE_MAX - offsetof(struct halo_request_storage_block, data))
    return NULL;

  block = malloc(offsetof(struct halo_request_storage_block, data) + cap);
  if (block == NULL)
    return NULL;

  block->next = NULL;
  block->cap = cap;
  block->used = 0;
  return block;
}

void halo_request_storage_init(struct halo_request_storage* storage,
                               size_t initial_block_size) {
  if (storage == NULL)
    return;

  storage->head = NULL;
  storage->initial_block_size =
    initial_block_size == 0 ? HALO_REQUEST_STORAGE_DEFAULT_BLOCK_SIZE
                            : initial_block_size;
}

void* halo_request_storage_alloc(struct halo_request_storage* storage,
                                 size_t size,
                                 size_t align) {
  struct halo_request_storage_block* block;
  struct halo_request_storage_block* next_block;
  size_t offset;
  size_t required_cap;
  size_t next_cap;

  if (storage == NULL)
    return NULL;

  if (size == 0)
    size = 1;
  if (align == 0)
    align = 1;

  block = storage->head;
  if (block != NULL) {
    offset = halo_request_storage_align_up(block->used, align);
    if (offset <= block->cap && size <= block->cap - offset) {
      block->used = offset + size;
      return block->data + offset;
    }
  }

  if (size > SIZE_MAX - (align - 1))
    return NULL;
  required_cap = size + (align - 1);
  next_cap = storage->initial_block_size;
  if (block != NULL && block->cap > next_cap)
    next_cap = block->cap;
  while (next_cap < required_cap) {
    if (next_cap > SIZE_MAX / 2) {
      next_cap = required_cap;
      break;
    }
    next_cap *= 2;
  }

  next_block = halo_request_storage_block_new(next_cap);
  if (next_block == NULL)
    return NULL;

  next_block->next = block;
  storage->head = next_block;
  offset = halo_request_storage_align_up(next_block->used, align);
  next_block->used = offset + size;
  return next_block->data + offset;
}

char* halo_request_storage_strndup(struct halo_request_storage* storage,
                                   const char* src,
                                   size_t len) {
  char* copy;

  copy = halo_request_storage_alloc(storage, len + 1, _Alignof(char));
  if (copy == NULL)
    return NULL;

  if (len != 0 && src != NULL)
    memcpy(copy, src, len);
  copy[len] = '\0';
  return copy;
}

void halo_request_storage_reset(struct halo_request_storage* storage) {
  struct halo_request_storage_block* block;
  struct halo_request_storage_block* next;

  if (storage == NULL || storage->head == NULL)
    return;

  block = storage->head->next;
  while (block != NULL) {
    next = block->next;
    free(block);
    block = next;
  }

  storage->head->next = NULL;
  storage->head->used = 0;
}

void halo_request_storage_destroy(struct halo_request_storage* storage) {
  struct halo_request_storage_block* block;
  struct halo_request_storage_block* next;

  if (storage == NULL)
    return;

  block = storage->head;
  while (block != NULL) {
    next = block->next;
    free(block);
    block = next;
  }

  storage->head = NULL;
}
