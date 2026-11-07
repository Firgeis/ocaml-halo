/* Build: cc -O2 -std=c11 -D_POSIX_C_SOURCE=200809L scripts/client_registry_bench.c c/lib/halo_client_registry.c -o /tmp/client_registry_bench */
#include "../c/lib/halo_client_registry.h"
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

struct halo_client { int unused; };
struct old_entry { uint64_t id; struct halo_client* client; struct old_entry* next; };
static volatile uintptr_t sink;
static volatile uint64_t key;
static uint64_t now_ns(void) {
  struct timespec t;
  if (clock_gettime(CLOCK_MONOTONIC, &t) != 0) abort();
  return (uint64_t)t.tv_sec * UINT64_C(1000000000) + (uint64_t)t.tv_nsec;
}
static struct halo_client* old_lookup(struct old_entry* head, uint64_t id) {
  for (; head != NULL; head = head->next)
    if (head->id == id) return head->client;
  return NULL;
}
static struct halo_client* old_remove(struct old_entry** head, uint64_t id) {
  while (*head != NULL) {
    struct old_entry* entry = *head;
    if (entry->id == id) {
      struct halo_client* client = entry->client;
      *head = entry->next;
      free(entry);
      return client;
    }
    head = &entry->next;
  }
  return NULL;
}
int main(void) {
  const size_t counts[] = {256, 1024, 4096, 16384};
  const size_t repetitions = 20000;
  puts("active,operation,list_ns_per_op,bucket_ns_per_op");
  for (size_t s = 0; s < sizeof(counts)/sizeof(counts[0]); s++) {
    size_t n = counts[s];
    struct halo_client client = {0};
    struct old_entry* head = NULL;
    struct halo_client_registry registry = {0};
    if (halo_client_registry_init(&registry, n) != 0) abort();
    for (size_t i = 1; i <= n; i++) {
      struct old_entry* e = malloc(sizeof(*e));
      if (!e || halo_client_registry_insert(&registry, i, &client) != 0) abort();
      e->id = i; e->client = &client; e->next = head; head = e;
    }
    for (int mode = 0; mode < 3; mode++) {
      uint64_t start, list_time, table_time;
      key = mode == 0 ? 1 : n + 1;
      start = now_ns();
      for (size_t i = 0; i < repetitions; i++)
        sink ^= (uintptr_t)(mode == 2 ? old_remove(&head, key) : old_lookup(head, key));
      list_time = now_ns() - start;
      start = now_ns();
      for (size_t i = 0; i < repetitions; i++)
        sink ^= (uintptr_t)(mode == 2 ? halo_client_registry_remove(&registry, key) : halo_client_registry_lookup(&registry, key));
      table_time = now_ns() - start;
      printf("%zu,%s,%.1f,%.1f\n", n, mode == 0 ? "lookup_oldest" : mode == 1 ? "lookup_missing" : "remove_missing", (double)list_time/repetitions, (double)table_time/repetitions);
    }
    uint64_t start = now_ns();
    for (size_t i = 1; i <= n; i++) sink ^= (uintptr_t)old_remove(&head, i);
    uint64_t list_time = now_ns() - start;
    start = now_ns();
    for (size_t i = 1; i <= n; i++) sink ^= (uintptr_t)halo_client_registry_remove(&registry, i);
    uint64_t table_time = now_ns() - start;
    printf("%zu,remove_oldest_first,%.1f,%.1f\n", n, (double)list_time/n, (double)table_time/n);
    halo_client_registry_destroy(&registry);
  }
  return 0;
}
