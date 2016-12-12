#include <stdio.h>
#include "value.h"

value
print(value a, value b) {
  value ret =  value_add(a, b);
  printf("hello world %ld\n", ret);
  return ret;
}
