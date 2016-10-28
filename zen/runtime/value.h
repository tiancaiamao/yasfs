#ifndef _VALUE_H
#define _VALUE_H

#include <stdint.h>
#include <stdbool.h>

typedef uintptr_t value;

bool is_block(value v);
bool is_int(value v);

bool is_true(value v);
bool is_false(value v);
bool is_unit(value v);

value value_true;
value value_false;
value value_unit;

int env_length(value env);
value env_get(value env, int n);
value env_append(value v, value *ptr, int count);

value new_closure(int pc, value env);
int closure_pc(value cls);
value closure_env(value cls);
void closure_set_env(value cls, value env);
void closure_set_pc(value cls, int pc);

value value_add(value v1, value v2);
value value_sub(value v1, value v2);
value value_mul(value v1, value v2);
value value_div(value v1, value v2);


value new_tuple(uint32_t tag, uint32_t size);
void tuple_set(value tu, int i, value v);
value tuple_get(value tu, int i);
uint32_t tuple_tag(value t);

void print_value(value v);


#endif
