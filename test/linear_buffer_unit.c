#include "../c/lib/halo_linear_buffer.h"

#include <assert.h>
#include <stdint.h>
#include <string.h>

static void test_compaction_and_growth(void) {
  struct halo_linear_buffer buffer;
  char out[32];
  char* storage;
  halo_linear_buffer_init(&buffer, 8);
  assert(halo_linear_buffer_append(&buffer, "abcdef", 6) == 0);
  storage = buffer.data;
  assert(halo_linear_buffer_read(&buffer, out, 3) == 3);
  assert(memcmp(out, "abc", 3) == 0);
  assert(halo_linear_buffer_append(&buffer, "gh", 2) == 0);
  assert(buffer.data == storage && buffer.offset == 3); /* Tail space reused. */
  assert(halo_linear_buffer_read(&buffer, out, 2) == 2);
  assert(memcmp(out, "de", 2) == 0);
  assert(halo_linear_buffer_append(&buffer, "ijkl", 4) == 0);
  assert(buffer.data == storage && buffer.offset == 0); /* Compacted in place. */
  assert(halo_linear_buffer_append(&buffer, "mnop", 4) == 0);
  assert(buffer.capacity == 16);
  assert(halo_linear_buffer_read(&buffer, out, sizeof(out)) == 11);
  assert(memcmp(out, "fghijklmnop", 11) == 0);
  assert(buffer.data == NULL); /* Oversized storage released on drain. */

  assert(halo_linear_buffer_append(&buffer, "12345678", 8) == 0);
  assert(halo_linear_buffer_read(&buffer, out, 3) == 3);
  assert(halo_linear_buffer_append(&buffer, "abc", 3) == 0);
  assert(halo_linear_buffer_read(&buffer, out, 8) == 8);
  assert(memcmp(out, "45678abc", 8) == 0);
  storage = buffer.data;
  assert(buffer.capacity == 8 && buffer.length == 0);
  assert(halo_linear_buffer_append(&buffer, "x", 1) == 0);
  assert(buffer.data == storage);
  assert(halo_linear_buffer_append(&buffer, "x", SIZE_MAX) == -1);
  assert(buffer.data == storage && buffer.length == 1);
  assert(halo_linear_buffer_read(&buffer, out, 1) == 1 && out[0] == 'x');
  assert(halo_linear_buffer_append(&buffer, "abcdef", 6) == 0);
  assert(halo_linear_buffer_read(&buffer, out, 4) == 4);
  assert(halo_linear_buffer_append(&buffer, "x", SIZE_MAX) == -1);
  assert(buffer.offset == 4 && buffer.length == 2);
  assert(halo_linear_buffer_append(&buffer, "ghijklmno", 9) == 0);
  assert(halo_linear_buffer_read(&buffer, out, sizeof(out)) == 11);
  assert(memcmp(out, "efghijklmno", 11) == 0);
  halo_linear_buffer_destroy(&buffer);
  halo_linear_buffer_destroy(&buffer);
}

static uint32_t next_random(uint32_t* state) {
  *state = *state * 1664525u + 1013904223u;
  return *state;
}

static void test_queue_model(uint32_t seed) {
  struct halo_linear_buffer buffer;
  uint32_t random = seed;
  char model[8192], input[512], out[512];
  size_t length = 0;
  halo_linear_buffer_init(&buffer, 1024);
  for (int i = 0; i < 20000; i++) {
    size_t amount = next_random(&random) % sizeof(input);
    if (length < 4096 && (next_random(&random) & 4)) {
      for (size_t j = 0; j < amount; j++)
        input[j] = (char) next_random(&random);
      assert(halo_linear_buffer_append(&buffer, input, amount) == 0);
      memcpy(model + length, input, amount);
      length += amount;
    } else {
      if (amount > length)
        amount = length;
      assert(halo_linear_buffer_read(&buffer, out, amount) == amount);
      assert(memcmp(out, model, amount) == 0);
      length -= amount;
      memmove(model, model + amount, length);
    }
    assert(buffer.length == length);
    assert(buffer.offset <= buffer.capacity);
    assert(buffer.length <= buffer.capacity - buffer.offset);
    if (length != 0)
      assert(memcmp(buffer.data + buffer.offset, model, length) == 0);
  }
  halo_linear_buffer_destroy(&buffer);
}

int main(void) {
  struct halo_linear_buffer buffer = {0};
  char out;
  assert(halo_linear_buffer_append(&buffer, NULL, 0) == 0);
  assert(halo_linear_buffer_read(&buffer, NULL, 0) == 0);
  assert(halo_linear_buffer_read(&buffer, &out, 1) == 0);
  assert(halo_linear_buffer_append(&buffer, "a", 1) == 0);
  assert(halo_linear_buffer_read(&buffer, &out, 1) == 1 && out == 'a');
  assert(buffer.data == NULL && buffer.capacity == 0);
  halo_linear_buffer_destroy(&buffer);
  test_compaction_and_growth();
  for (uint32_t seed = 0; seed < 32; seed++)
    test_queue_model(seed);
  return 0;
}
