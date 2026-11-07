#include "../c/lib/halo_client_registry.h"

#include <assert.h>
#include <stdint.h>

struct halo_client { int value; };

static void test_collisions_and_removal(void) {
  struct halo_client_registry registry = {0};
  struct halo_client clients[6] = {{1}, {2}, {3}, {4}, {5}, {6}};

  assert(halo_client_registry_lookup(&registry, 1) == NULL);
  assert(halo_client_registry_remove(&registry, 1) == NULL);
  assert(halo_client_registry_insert(&registry, 1, &clients[0]) == -1);
  assert(halo_client_registry_init(&registry, 1) == 0);
  for (uint64_t id = 1; id <= 6; id++)
    assert(halo_client_registry_insert(&registry, id, &clients[id - 1]) == 0);
  assert(halo_client_registry_insert(&registry, 0, &clients[0]) == -1);
  assert(halo_client_registry_insert(&registry, 7, NULL) == -1);
  assert(halo_client_registry_insert(&registry, 3, &clients[0]) == -1);
  assert(halo_client_registry_lookup(&registry, 3) == &clients[2]);
  assert(halo_client_registry_lookup(&registry, 0) == NULL);
  assert(halo_client_registry_remove(&registry, 0) == NULL);
  assert(halo_client_registry_lookup(&registry, 7) == NULL);
  assert(halo_client_registry_remove(&registry, 7) == NULL);

  /* Insertion is at the head: remove head, middle, then tail. */
  assert(halo_client_registry_remove(&registry, 6) == &clients[5]);
  assert(halo_client_registry_remove(&registry, 3) == &clients[2]);
  assert(halo_client_registry_remove(&registry, 1) == &clients[0]);
  assert(halo_client_registry_remove(&registry, 3) == NULL);
  assert(halo_client_registry_lookup(&registry, 2) == &clients[1]);
  assert(halo_client_registry_lookup(&registry, 4) == &clients[3]);
  assert(halo_client_registry_lookup(&registry, 5) == &clients[4]);
  assert(halo_client_registry_insert(&registry, 3, &clients[0]) == 0);
  assert(halo_client_registry_remove(&registry, 3) == &clients[0]);
  halo_client_registry_destroy(&registry);
  assert(registry.buckets == NULL && registry.bucket_count == 0);
  assert(clients[1].value == 2 && clients[3].value == 4 && clients[4].value == 5);
  halo_client_registry_destroy(&registry);
}

static void test_large_ids(void) {
  struct halo_client_registry registry;
  struct halo_client clients[4] = {{1}, {2}, {3}, {4}};
  const uint64_t ids[] = {1, UINT64_C(0x100000001), UINT64_MAX - 1, UINT64_MAX};

  assert(halo_client_registry_init(&registry, 7) == 0);
  for (size_t i = 0; i < 4; i++)
    assert(halo_client_registry_insert(&registry, ids[i], &clients[i]) == 0);
  for (size_t i = 0; i < 4; i++)
    assert(halo_client_registry_lookup(&registry, ids[i]) == &clients[i]);
  for (size_t i = 0; i < 4; i++)
    assert(halo_client_registry_remove(&registry, ids[i]) == &clients[i]);
  halo_client_registry_destroy(&registry);
}

int main(void) {
  test_collisions_and_removal();
  test_large_ids();
  return 0;
}
