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

#endif
