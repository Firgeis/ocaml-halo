#ifndef OCAML_HALO_RUNTIME_UTILS_H
#define OCAML_HALO_RUNTIME_UTILS_H

#include <stdint.h>
#include <stdlib.h>

uint64_t halo_now_seconds(void);
int halo_is_ows_char(char ch);
char* halo_trim_in_place(char* text);
int halo_decode_hex(const char* hex,
                    uint8_t* out,
                    size_t out_cap,
                    size_t* out_len);

#endif
