#include "value.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

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
  int pc;
  struct Env *env;
};

struct Tuple {
  struct block_head head;
  uint16_t tag;
  value data[0];
};

static value
block_alloc(int count, uint8_t tag) {
  // TODO GC
  struct block_head* head = (struct block_head*)malloc(sizeof(value)*count);
  head->tag = tag;
  head->size = count;
  return (value)head;
}

int
env_length(value v) {
  if (v == (value)NULL) return 0;
  struct Env* env = (struct Env*)v;
  return env->head.size / sizeof(value) - 1;
}

value
env_get(value v, int n) {
  return ((struct Env*)v)->data[n];
}

value
new_closure(int pc, value env) {
  value b = block_alloc(3, tag_closure);
  struct Closure* cls = (struct Closure*)b;
  cls->pc = pc;
  cls->env = (struct Env*)env;
  return b;
}

int
closure_pc(value v) {
  return ((struct Closure*)v)->pc;
}

value
value_add(value a, value b) {
  return a + b;
}
