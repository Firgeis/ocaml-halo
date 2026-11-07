#ifndef OCAML_HALO_CLIENT_REGISTRY_H
#define OCAML_HALO_CLIENT_REGISTRY_H

#include <stddef.h>
#include <stdint.h>

struct halo_client;
struct halo_client_registry_entry;

/* Owns its buckets and entries, but never the clients. Do not copy an
 * initialized registry. There is no internal locking: only the reactor may
 * access it during operation; initialization and destruction are outside
 * concurrent use. */
struct halo_client_registry {
  struct halo_client_registry_entry** buckets;
  size_t bucket_count;
};

/* Initialize a fresh or destroyed registry, not an already initialized one.
 * Returns -1 for zero buckets, allocation overflow, or allocation failure;
 * on failure the registry is empty and safe to destroy. Bucket count affects
 * lookup cost, not the number of entries that may be inserted. */
int halo_client_registry_init(struct halo_client_registry* registry, size_t bucket_count);

/* ID zero is reserved. Insert rejects zero, NULL clients, duplicate IDs,
 * uninitialized registries, and allocation failure, returning -1 without
 * changing the registry. Returns 0 on success. */
int halo_client_registry_insert(struct halo_client_registry* registry, uint64_t id,
                                struct halo_client* client);

/* Return NULL if the ID is zero or absent. Remove frees the entry only. */
struct halo_client* halo_client_registry_lookup(const struct halo_client_registry* registry,
                                                uint64_t id);
struct halo_client* halo_client_registry_remove(struct halo_client_registry* registry,
                                                uint64_t id);

/* Safe for zero-initialized, failed-initialization, and destroyed registries. */
void halo_client_registry_destroy(struct halo_client_registry* registry);

#endif
