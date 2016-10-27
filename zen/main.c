#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "unistd.h"
#include "fcntl.h"
#include "value.h"
#include "runtime/vm.h"

int
main(int argc, char* argv[]) {
  int fd = open("test.out", O_RDONLY);
  if (fd == 0) {
    perror("open file fail");
    return -1;
  }

  char buf[1024];
  memset(buf, 0, 1024);

  ssize_t off = 0;
  while(1) {
    ssize_t n = read(fd, buf+off, 1024-off);
    if (n == 0) break;
    if (n < 0) {
      perror("read file error");
      return -2;
    }
    off += n;
  }

  struct VM* vm = vm_new(4000);
  value res = vm_run(vm, buf);
  print_value(res);

  return 0;
}
