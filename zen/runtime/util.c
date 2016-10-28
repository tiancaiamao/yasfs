#include "value.h"

uint32_t
read_uint32(unsigned char *p) {
  uint32_t a = (uint32_t)p[0];
  uint32_t ret = a + (p[1]<<8) + (p[2]<<16) + (p[3]<<24);
  return ret;
}

uint64_t
read_uint64(unsigned char *p) {
  uint32_t v1 = read_uint32(p);
  uint32_t v2 = read_uint32(p+4);
  return (uint64_t)(v1 + ((uint64_t)v2<<32));
}

value
read_value(unsigned char *p) {
  return (value)read_uint64(p);
}
