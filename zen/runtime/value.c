#include "value.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

bool
is_block(value v) {
  return (v & 7) == 0;
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
  tag_userdata,
  tag_env,
};

struct block_head {
  uint8_t tag;
  uint8_t gcflag;
  uint16_t _reversed;
  uint32_t size;
};

// lowest bit != 1 not int
// lowest 3 bit != 0 not a valid aligned pointer
value value_true = (value)0x12; // 18
value value_false = (value)0x22; // 34
value value_unit = (value)0x32; // 66

struct Env {
  struct block_head head;
  value data[0];
};

struct Closure {
  struct block_head head;
  int64_t pc;
  struct Env *env;
};

struct Tuple {
  struct block_head head;
  uint32_t tag;
  uint32_t size;
  value data[0];
};

struct String {
  struct block_head head;
  uint32_t size;
  char *ptr; // string is immutable, ptr may point to code!
};

static value
block_alloc(int count, uint8_t tag) {
  // TODO GC
  struct block_head* head = (struct block_head*)malloc(sizeof(value)*count);
  head->tag = tag;
  head->size = count;
  return (value)head;
}

void
print_value(value v) {
  if (is_int(v)) {
    printf("int: %d\n", (int)(v>>1));
  } else if (v == value_true) {
    printf("true\n");
  } else if (v == value_false) {
    printf("false\n");
  } else if (v == value_unit) {
    printf("unit\n");
  } else {
    struct block_head *head = (struct block_head*)v;
    switch (head->tag) {
    case tag_tuple:
      printf("tuple\n");
      break;
    case tag_closure:
      printf("closure\n");
      break;
    case tag_string:
      printf("string\n");
      break;
    }
  }
}

int
env_length(value v) {
  if (v == (value)NULL) return 0;
  struct Env* env = (struct Env*)v;
  return env->head.size - 1;
}

value
env_get(value v, int n) {
  return ((struct Env*)v)->data[n];
}

value
env_append(value v, value *ptr, int count) {
  if (count == 0) return v;
  int len = env_length(v);
  value ret = block_alloc(len+count+1, tag_env);
  struct Env* ev = (struct Env*)v;
  struct Env* er = (struct Env*)ret;
  memcpy(&er->data, &ev->data, len*sizeof(value));
  memcpy(&er->data[len], ptr, count*sizeof(value));
  return ret;
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
closure_addr(value v) {
  return ((struct Closure*)v)->pc;
}

value
closure_env(value v) {
  return (value)(((struct Closure*)v)->env);
}

void
closure_set_env(value cls, value env) {
  ((struct Closure*)cls)->env = (struct Env*)(env);
}

void
closure_set_addr(value cls, int64_t pc) {
  ((struct Closure*)cls)->pc = pc;
}

value
new_tuple(uint32_t tag, uint32_t size) {
  value b = block_alloc(size+3, tag_tuple);
  struct Tuple* t = (struct Tuple*)b;
  t->tag = tag;
  t->size = size;
  return b;
}

void
tuple_set(value tu, int i, value v) {
  struct Tuple *t = (struct Tuple*)tu;
  t->data[i] = v;
}

value
tuple_get(value tu, int i) {
  struct Tuple *t = (struct Tuple*)tu;
  return t->data[i];
}

uint32_t
tuple_tag(value t) {
  return ((struct Tuple*)t)->tag;
}

value
value_add(value a, value b) {
  return (((a>>1) + (b>>1)) << 1) | 1;
}

value
value_sub(value a, value b) {
  return (((a>>1) - (b>>1)) << 1) | 1;
}

value
value_mul(value a, value b) {
  return (((a>>1) * (b>>1)) << 1) | 1;
}

value
value_div(value a, value b) {
  return (((a>>1) / (b>>1)) << 1) | 1;
}

value
new_string(char *p, int n) {
  value b = block_alloc(3, tag_string);
  struct String* s = (struct String*)b;
  s->ptr = p;
  s->size = n;
  return b;
}

char *
value_string(value v) {
  return ((struct String*)v)->ptr;
}
