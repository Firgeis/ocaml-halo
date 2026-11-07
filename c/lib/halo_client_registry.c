#include "halo_client_registry.h"

#include <stdlib.h>

struct halo_client_registry_entry {
  uint64_t id;
  struct halo_client* client;
  struct halo_client_registry_entry* next;
};

int halo_client_registry_init(struct halo_client_registry* registry, size_t bucket_count) {
  registry->buckets = NULL;
  registry->bucket_count = 0;
  if (bucket_count == 0 || bucket_count > SIZE_MAX / sizeof(*registry->buckets))
    return -1;
  registry->buckets = calloc(bucket_count, sizeof(*registry->buckets));
  if (registry->buckets == NULL)
    return -1;
  registry->bucket_count = bucket_count;
  return 0;
}

int halo_client_registry_insert(struct halo_client_registry* registry, uint64_t id,
                                struct halo_client* client) {
  struct halo_client_registry_entry* entry;
  size_t bucket;

  if (id == 0 || client == NULL || registry->buckets == NULL || registry->bucket_count == 0)
    return -1;
  bucket = id % registry->bucket_count;
  for (entry = registry->buckets[bucket]; entry != NULL; entry = entry->next) {
    if (entry->id == id)
      return -1;
  }
  entry = malloc(sizeof(*entry));
  if (entry == NULL)
    return -1;
  entry->id = id;
  entry->client = client;
  entry->next = registry->buckets[bucket];
  registry->buckets[bucket] = entry;
  return 0;
}

struct halo_client* halo_client_registry_lookup(const struct halo_client_registry* registry,
                                                uint64_t id) {
  struct halo_client_registry_entry* entry;

  if (id == 0 || registry->buckets == NULL || registry->bucket_count == 0)
    return NULL;
  for (entry = registry->buckets[id % registry->bucket_count]; entry != NULL; entry = entry->next) {
    if (entry->id == id)
      return entry->client;
  }
  return NULL;
}

struct halo_client* halo_client_registry_remove(struct halo_client_registry* registry,
                                                uint64_t id) {
  struct halo_client_registry_entry** cursor;

  if (id == 0 || registry->buckets == NULL || registry->bucket_count == 0)
    return NULL;
  cursor = &registry->buckets[id % registry->bucket_count];
  while (*cursor != NULL) {
    struct halo_client_registry_entry* entry = *cursor;
    if (entry->id == id) {
      struct halo_client* client = entry->client;
      *cursor = entry->next;
      free(entry);
      return client;
    }
    cursor = &entry->next;
  }
  return NULL;
}

void halo_client_registry_destroy(struct halo_client_registry* registry) {
  if (registry->buckets != NULL) {
    for (size_t i = 0; i < registry->bucket_count; i++) {
      struct halo_client_registry_entry* entry = registry->buckets[i];
      while (entry != NULL) {
        struct halo_client_registry_entry* next = entry->next;
        free(entry);
        entry = next;
      }
    }
    free(registry->buckets);
  }
  registry->buckets = NULL;
  registry->bucket_count = 0;
}
