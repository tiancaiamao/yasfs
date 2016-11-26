#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "value.h"
#include "vm.h"
#include "util.h"

int
main(int argc, char* argv[]) {
  if (argc < 2) return -1;
  char *buf = read_file(argv[1]);
  struct VM* vm = vm_new(4000);
  value res = vm_run(vm, buf);
  print_value(res);
  vm_close(vm);
  free(buf);
  return 0;
}
