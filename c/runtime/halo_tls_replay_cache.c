#include "halo_runtime_internal.h"

#include <pthread.h>

#define HALO_TLS_REPLAY_BUCKET_COUNT 16384U
#define HALO_TLS_REPLAY_ENTRY_CAPACITY 65536U
#define HALO_TLS_REPLAY_IDENTITY_BYTE_CAPACITY (64U * 1024U * 1024U)
#define HALO_NANOS_PER_SECOND UINT64_C(1000000000)

/* Accepted ticket identities are recorded across every Halo runtime in this
 * process. Exhausting either budget rejects new early data without evicting
 * live entries. */

struct halo_tls_replay_entry {
  uint8_t* identity;
  size_t identity_len;
  uint64_t hash;
  uint64_t expires_at_nanos;
  struct halo_tls_replay_entry* bucket_next;
  struct halo_tls_replay_entry* expiry_previous;
  struct halo_tls_replay_entry* expiry_next;
};

struct halo_tls_replay_cache {
  pthread_mutex_t mutex;
  struct halo_tls_replay_entry* buckets[HALO_TLS_REPLAY_BUCKET_COUNT];
  struct halo_tls_replay_entry* expiry_head;
  struct halo_tls_replay_entry* expiry_tail;
  size_t entry_count;
  size_t identity_bytes;
};

static struct halo_tls_replay_cache halo_tls_replay_cache = {
  .mutex = PTHREAD_MUTEX_INITIALIZER
};

static uint64_t halo_tls_replay_hash(const uint8_t* identity,
                                     size_t identity_len) {
  uint64_t hash;
  size_t index;

  hash = UINT64_C(14695981039346656037);
  for (index = 0; index < identity_len; index++) {
    hash ^= identity[index];
    hash *= UINT64_C(1099511628211);
  }
  return hash;
}

static uint64_t halo_tls_replay_expiry(uint64_t now_nanos, uint64_t ttl_secs) {
  uint64_t ttl_nanos;

  if (ttl_secs > UINT64_MAX / HALO_NANOS_PER_SECOND)
    return UINT64_MAX;
  ttl_nanos = ttl_secs * HALO_NANOS_PER_SECOND;
  if (UINT64_MAX - now_nanos < ttl_nanos)
    return UINT64_MAX;
  return now_nanos + ttl_nanos;
}

static int halo_tls_replay_now(uint64_t* now_nanos) {
  struct timespec now;
  uint64_t seconds;

  if (now_nanos == NULL || clock_gettime(CLOCK_MONOTONIC, &now) != 0
      || now.tv_sec < 0)
    return -1;
  seconds = (uint64_t) now.tv_sec;
  if (seconds > (UINT64_MAX - (uint64_t) now.tv_nsec)
                  / HALO_NANOS_PER_SECOND)
    return -1;
  *now_nanos = (seconds * HALO_NANOS_PER_SECOND) + (uint64_t) now.tv_nsec;
  return 0;
}

static void halo_tls_replay_entry_destroy(struct halo_tls_replay_entry* entry) {
  if (entry == NULL)
    return;
  free(entry->identity);
  free(entry);
}

static void halo_tls_replay_cache_remove_locked(
    struct halo_tls_replay_cache* cache,
    struct halo_tls_replay_entry* entry) {
  struct halo_tls_replay_entry** bucket;

  bucket = &cache->buckets[entry->hash % HALO_TLS_REPLAY_BUCKET_COUNT];
  while (*bucket != NULL && *bucket != entry)
    bucket = &(*bucket)->bucket_next;
  if (*bucket == entry)
    *bucket = entry->bucket_next;

  if (entry->expiry_previous == NULL)
    cache->expiry_head = entry->expiry_next;
  else
    entry->expiry_previous->expiry_next = entry->expiry_next;
  if (entry->expiry_next == NULL)
    cache->expiry_tail = entry->expiry_previous;
  else
    entry->expiry_next->expiry_previous = entry->expiry_previous;

  assert(cache->entry_count > 0);
  assert(cache->identity_bytes >= entry->identity_len);
  cache->entry_count -= 1;
  cache->identity_bytes -= entry->identity_len;
  halo_tls_replay_entry_destroy(entry);
}

static void halo_tls_replay_cache_evict_expired_locked(
    struct halo_tls_replay_cache* cache,
    uint64_t now_nanos) {
  while (cache->expiry_head != NULL
         && cache->expiry_head->expires_at_nanos <= now_nanos) {
    halo_tls_replay_cache_remove_locked(cache, cache->expiry_head);
  }
}

int halo_tls_replay_cache_claim(const uint8_t* identity,
                                size_t identity_len,
                                uint64_t ttl_secs) {
  struct halo_tls_replay_entry* entry;
  uint64_t hash;
  uint64_t now_nanos;
  size_t bucket_index;

  if (identity == NULL || identity_len == 0 || ttl_secs == 0)
    return -1;

  hash = halo_tls_replay_hash(identity, identity_len);
  if (halo_tls_replay_now(&now_nanos) != 0)
    return -1;

  pthread_mutex_lock(&halo_tls_replay_cache.mutex);
  halo_tls_replay_cache_evict_expired_locked(&halo_tls_replay_cache, now_nanos);

  bucket_index = hash % HALO_TLS_REPLAY_BUCKET_COUNT;
  for (entry = halo_tls_replay_cache.buckets[bucket_index];
       entry != NULL;
       entry = entry->bucket_next) {
    if (entry->hash == hash
        && entry->identity_len == identity_len
        && memcmp(entry->identity, identity, identity_len) == 0) {
      pthread_mutex_unlock(&halo_tls_replay_cache.mutex);
      return 0;
    }
  }

  if (halo_tls_replay_cache.entry_count >= HALO_TLS_REPLAY_ENTRY_CAPACITY
      || identity_len
           > HALO_TLS_REPLAY_IDENTITY_BYTE_CAPACITY
               - halo_tls_replay_cache.identity_bytes) {
    pthread_mutex_unlock(&halo_tls_replay_cache.mutex);
    return -1;
  }

  entry = malloc(sizeof(*entry));
  if (entry == NULL) {
    pthread_mutex_unlock(&halo_tls_replay_cache.mutex);
    return -1;
  }
  entry->identity = malloc(identity_len);
  if (entry->identity == NULL) {
    free(entry);
    pthread_mutex_unlock(&halo_tls_replay_cache.mutex);
    return -1;
  }

  memcpy(entry->identity, identity, identity_len);
  entry->identity_len = identity_len;
  entry->hash = hash;
  entry->expires_at_nanos = halo_tls_replay_expiry(now_nanos, ttl_secs);
  entry->bucket_next = halo_tls_replay_cache.buckets[bucket_index];

  halo_tls_replay_cache.buckets[bucket_index] = entry;
  entry->expiry_previous = halo_tls_replay_cache.expiry_tail;
  while (entry->expiry_previous != NULL
         && entry->expiry_previous->expires_at_nanos
              > entry->expires_at_nanos) {
    entry->expiry_previous = entry->expiry_previous->expiry_previous;
  }
  if (entry->expiry_previous == NULL) {
    entry->expiry_next = halo_tls_replay_cache.expiry_head;
    if (entry->expiry_next != NULL)
      entry->expiry_next->expiry_previous = entry;
    halo_tls_replay_cache.expiry_head = entry;
  }
  else {
    entry->expiry_next = entry->expiry_previous->expiry_next;
    entry->expiry_previous->expiry_next = entry;
    if (entry->expiry_next != NULL)
      entry->expiry_next->expiry_previous = entry;
  }
  if (entry->expiry_next == NULL)
    halo_tls_replay_cache.expiry_tail = entry;
  halo_tls_replay_cache.entry_count += 1;
  halo_tls_replay_cache.identity_bytes += identity_len;
  pthread_mutex_unlock(&halo_tls_replay_cache.mutex);
  return 1;
}

CAMLprim value ocaml_halo_tls_replay_cache_claim(value identity,
                                                  value ttl_secs) {
  CAMLparam2(identity, ttl_secs);
  int64_t ttl;
  int result;

  ttl = Int64_val(ttl_secs);
  if (ttl <= 0)
    CAMLreturn(Val_int(-1));
  result = halo_tls_replay_cache_claim(
    (const uint8_t*) String_val(identity),
    caml_string_length(identity),
    (uint64_t) ttl);
  CAMLreturn(Val_int(result));
}
