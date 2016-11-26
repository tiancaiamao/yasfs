#include "value.h"
#include "unistd.h"
#include "fcntl.h"
#include "unistd.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

static int
read_all(int fd, char* buf, int size) {
  ssize_t off = 0;
  while(off < size) {
    ssize_t n = read(fd, buf+off, size-off);
    if (n == 0) break;
    if (n < 0) return n;
    off += n;
  }
  return off;
}

static char*
slice_append(char *to, int *size, int *cap, char *from, int sz) {
  if (*cap == 0) {
    *cap = (*size + sz) * 2;
    to = (char*)malloc(*cap);
  } else if (*cap < *size + sz) {
    *cap = (*size + sz) * 2;
    char *tmp = (char*)malloc(*cap);
    memcpy(tmp, to, *size);
    free(to);
    to = tmp;
  }

  memcpy(to+(*size), from, sz);
  *size = *size + sz;
  return to;
}

char*
read_file(char *file) {
  int fd = open(file, O_RDONLY);
  if (fd == 0) {
    return NULL;
  }

  char *ret = NULL;
  int size = 0;
  int cap = 0;
  while(1) {
    char buf[1024];
    int n = read_all(fd, buf, 1024);
    if (n < 0) return NULL;
    ret = slice_append(ret, &size, &cap, buf, n);
    if (n < 1024) break;
  }
  return ret;
}
