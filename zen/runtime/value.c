#include "value.h"
#include <stdbool.h>
#include <stdint.h>

bool
is_block(value v) {
  return (v & 1) == 0;
}

bool
is_int(value v) {
  return (v & 1) != 0;
}

enum {
  tag_unit,
  tag_bool,
  tag_tuple,
  tag_closure,
  tag_string,
};

struct block_head {
  uint8_t tag;
  uint8_t gcflag;
  uint16_t _reversed;
  uint32_t size;
};

static struct block_head _const_true;
static struct block_head _const_false;
static struct block_head _const_unit;

value value_true = (value)&_const_true;
value value_false = (value)&_const_false;
value value_unit = (value)&_const_unit;

struct Env {
  struct block_head head;
  value data[0];
};

struct Closure {
  struct block_head head;
  char *code;
};

struct Tuple {
  struct block_head head;
  uint16_t tag;
  value data[0];
};
