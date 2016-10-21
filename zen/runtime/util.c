#include "value.h"

uint32_t
read_uint32(char *p) {
  return p[0] + (p[1]<<8) + (p[2]<<16) + (p[3]<<24);
}

uint64_t
read_uint64(char *p) {
  uint64_t v1 = read_uint32(p);
  uint64_t v2 = read_uint32(p+4);
  return (v1<<32) + v2;
}

value
read_value(char *p) {
  return (value)read_uint64(p);
}
