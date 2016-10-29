#ifndef _VM_H
#define _VM_H

#include "value.h"

struct VM;

struct VM* vm_new(int sz);
value vm_run(struct VM *vm, char *code);
int vm_load(struct VM *vm, char *dylib_path);
void vm_close(struct VM *vm);

#endif
