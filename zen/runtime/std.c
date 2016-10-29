#include <stdio.h>
#include "value.h"

value
print(value x) {
  printf("hello world!");
  return value_unit;
}
