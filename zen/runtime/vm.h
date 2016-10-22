#ifndef _VM_H
#define _VM_H

#include "value.h"

struct VM;

struct VM* vm_new(int sz);
value vm_run(struct VM *vm, char *code);

#endif
