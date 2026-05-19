#include "halo_utils.h"

#include <string.h>
#include <time.h>

uint64_t halo_now_seconds(void) {
  time_t now;

  now = time(NULL);
  if (now < 0)
    return 0;
  return (uint64_t) now;
}

int halo_is_ows_char(char ch) {
  return ch == ' ' || ch == '\t';
}

char* halo_trim_in_place(char* text) {
  char* end;

  if (text == NULL)
    return NULL;

  while (*text == ' ' || *text == '\t' || *text == '\r' || *text == '\n')
    text += 1;

  end = text + strlen(text);
  while (end > text
         && (end[-1] == ' ' || end[-1] == '\t' || end[-1] == '\r'
             || end[-1] == '\n')) {
    end -= 1;
  }
  *end = '\0';
  return text;
}

static int halo_hex_nibble(char ch) {
  if (ch >= '0' && ch <= '9')
    return ch - '0';
  if (ch >= 'a' && ch <= 'f')
    return ch - 'a' + 10;
  if (ch >= 'A' && ch <= 'F')
    return ch - 'A' + 10;
  return -1;
}

int halo_decode_hex(const char* hex,
                    uint8_t* out,
                    size_t out_cap,
                    size_t* out_len) {
  size_t hex_len;
  size_t index;

  if (hex == NULL || out == NULL || out_len == NULL)
    return -1;

  hex_len = strlen(hex);
  if ((hex_len % 2) != 0 || (hex_len / 2) > out_cap)
    return -1;

  for (index = 0; index < hex_len; index += 2) {
    int high;
    int low;

    high = halo_hex_nibble(hex[index]);
    low = halo_hex_nibble(hex[index + 1]);
    if (high < 0 || low < 0)
      return -1;
    out[index / 2] = (uint8_t) ((high << 4) | low);
  }

  *out_len = hex_len / 2;
  return 0;
}
