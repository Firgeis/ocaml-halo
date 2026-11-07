#include "../c/lib/halo_client_registry.h"

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>

struct halo_client { int value; };

static size_t live_allocations;
static size_t allocation_calls;
static int fail_next_allocation;

static void* registry_test_malloc(size_t size) {
  void* result;
  allocation_calls++;
  if (fail_next_allocation) {
    fail_next_allocation = 0;
    return NULL;
  }
  result = malloc(size);
  if (result != NULL)
    live_allocations++;
  return result;
}

static void* registry_test_calloc(size_t count, size_t size) {
  void* result;
  allocation_calls++;
  if (fail_next_allocation) {
    fail_next_allocation = 0;
    return NULL;
  }
  result = calloc(count, size);
  if (result != NULL)
    live_allocations++;
  return result;
}

static void registry_test_free(void* pointer) {
  if (pointer != NULL) {
    assert(live_allocations > 0);
    live_allocations--;
  }
  free(pointer);
}

/* Inject allocation failures without adding allocator hooks to the API. */
#define malloc registry_test_malloc
#define calloc registry_test_calloc
#define free registry_test_free
#include "../c/lib/halo_client_registry.c"
#undef malloc
#undef calloc
#undef free

int main(void) {
  struct halo_client_registry registry = {0};
  struct halo_client clients[2] = {{1}, {2}};
  size_t calls;

  halo_client_registry_destroy(&registry);
  calls = allocation_calls;
  assert(halo_client_registry_init(&registry, 0) == -1);
  assert(halo_client_registry_init(&registry, SIZE_MAX) == -1);
  assert(allocation_calls == calls);
  assert(registry.buckets == NULL && registry.bucket_count == 0);
  halo_client_registry_destroy(&registry);

  fail_next_allocation = 1;
  assert(halo_client_registry_init(&registry, 2) == -1);
  assert(registry.buckets == NULL && registry.bucket_count == 0);
  assert(live_allocations == 0);
  halo_client_registry_destroy(&registry);
  assert(halo_client_registry_init(&registry, 1) == 0);
  fail_next_allocation = 1;
  assert(halo_client_registry_insert(&registry, 1, &clients[0]) == -1);
  assert(halo_client_registry_lookup(&registry, 1) == NULL);
  assert(live_allocations == 1);
  assert(halo_client_registry_insert(&registry, 1, &clients[0]) == 0);
  calls = allocation_calls;
  assert(halo_client_registry_insert(&registry, 1, &clients[1]) == -1);
  assert(allocation_calls == calls);
  fail_next_allocation = 1;
  assert(halo_client_registry_insert(&registry, 2, &clients[1]) == -1);
  assert(halo_client_registry_lookup(&registry, 1) == &clients[0]);
  assert(halo_client_registry_lookup(&registry, 2) == NULL);
  assert(live_allocations == 2);
  assert(halo_client_registry_insert(&registry, 2, &clients[1]) == 0);
  assert(halo_client_registry_remove(&registry, 1) == &clients[0]);
  halo_client_registry_destroy(&registry);
  assert(live_allocations == 0);
  halo_client_registry_destroy(&registry);

  /* Cleanup tolerates a partially initialized table with no buckets. */
  registry.bucket_count = 2;
  halo_client_registry_destroy(&registry);
  assert(registry.buckets == NULL && registry.bucket_count == 0);
  registry.buckets = registry_test_calloc(1, sizeof(*registry.buckets));
  assert(registry.buckets != NULL);
  halo_client_registry_destroy(&registry);
  assert(live_allocations == 0);
  return 0;
}
