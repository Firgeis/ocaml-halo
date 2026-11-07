#include "../c/lib/halo_linear_buffer.h"

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

static size_t live_allocations;
static size_t allocation_calls;
static size_t requested_capacity;
static int fail_next_allocation;

static void* buffer_test_malloc(size_t size) {
  void* data;
  allocation_calls++;
  requested_capacity = size;
  if (fail_next_allocation) {
    fail_next_allocation = 0;
    return NULL;
  }
  data = malloc(size);
  if (data != NULL)
    live_allocations++;
  return data;
}

static void buffer_test_free(void* data) {
  if (data != NULL) {
    assert(live_allocations > 0);
    live_allocations--;
  }
  free(data);
}

/* Compile the implementation with a test allocator to force failures without
 * platform-specific linker flags or allocator hooks in the production API. */
#define malloc buffer_test_malloc
#define free buffer_test_free
#include "../c/lib/halo_linear_buffer.c"
#undef malloc
#undef free

static void test_allocation_failures(void) {
  struct halo_linear_buffer buffer;
  char* storage;
  char out[16];
  size_t calls;

  halo_linear_buffer_init(&buffer, 8);
  fail_next_allocation = 1;
  assert(halo_linear_buffer_append(&buffer, "a", 1) == -1);
  assert(buffer.data == NULL && buffer.capacity == 0);
  assert(buffer.offset == 0 && buffer.length == 0);
  assert(live_allocations == 0);

  assert(halo_linear_buffer_append(&buffer, "abcdefgh", 8) == 0);
  storage = buffer.data;
  assert(halo_linear_buffer_read(&buffer, out, 3) == 3);
  fail_next_allocation = 1;
  assert(halo_linear_buffer_append(&buffer, "ijkl", 4) == -1);
  assert(buffer.data == storage && buffer.capacity == 8);
  assert(buffer.offset == 3 && buffer.length == 5);
  assert(memcmp(buffer.data + buffer.offset, "defgh", 5) == 0);
  assert(live_allocations == 1);

  calls = allocation_calls;
  assert(halo_linear_buffer_append(&buffer, "x", SIZE_MAX) == -1);
  assert(allocation_calls == calls); /* Reject addition overflow before malloc. */
  assert(buffer.data == storage && buffer.offset == 3 && buffer.length == 5);
  assert(halo_linear_buffer_append(&buffer, "ijkl", 4) == 0);
  assert(live_allocations == 1); /* Old allocation freed after successful grow. */
  assert(halo_linear_buffer_read(&buffer, out, SIZE_MAX) == 9);
  assert(memcmp(out, "defghijkl", 9) == 0);
  assert(buffer.data == NULL && live_allocations == 0);

  /* Exercise the doubling-overflow branch without requesting a real huge
   * allocation. The failed append must leave the empty buffer reusable. */
  fail_next_allocation = 1;
  assert(halo_linear_buffer_append(&buffer, "x", SIZE_MAX) == -1);
  assert(requested_capacity == SIZE_MAX);
  assert(buffer.data == NULL && buffer.capacity == 0 && buffer.length == 0);
  assert(halo_linear_buffer_append(&buffer, "z", 1) == 0);
  halo_linear_buffer_destroy(&buffer); /* Destroy with unread data. */
  assert(live_allocations == 0);
  halo_linear_buffer_destroy(&buffer);
}

static void test_retention_boundaries(void) {
  const size_t limits[] = {0, 1, 4095, 4096, 4097, 65536};
  char input[65537], output[65538];
  memset(input, 'x', sizeof(input));
  for (size_t i = 0; i < sizeof(limits) / sizeof(limits[0]); i++) {
    struct halo_linear_buffer buffer;
    const size_t sizes[] = {1, limits[i], limits[i] + 1};
    halo_linear_buffer_init(&buffer, limits[i]);
    for (size_t j = 0; j < sizeof(sizes) / sizeof(sizes[0]); j++) {
      size_t amount = sizes[j];
      assert(halo_linear_buffer_append(&buffer, input, amount) == 0);
      memset(output, '!', sizeof(output));
      assert(halo_linear_buffer_read(&buffer, output, SIZE_MAX) == amount);
      assert(memcmp(output, input, amount) == 0 && output[amount] == '!');
      assert(buffer.length == 0 && buffer.offset == 0);
      assert(buffer.capacity <= limits[i]);
      assert((buffer.data == NULL) == (buffer.capacity == 0));
      assert(live_allocations == (buffer.data == NULL ? 0 : 1));
    }
    halo_linear_buffer_destroy(&buffer);
    assert(live_allocations == 0);
  }
}

int main(void) {
  test_allocation_failures();
  test_retention_boundaries();
  assert(live_allocations == 0);
  return 0;
}
