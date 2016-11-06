#include "value.h"
#include "vm.h"
#include "instruct.h"
#include "util.h"
#include "dylib.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

static value c_call(struct VM* vm, void *fn, int n);

struct VM {
  int pc;	// pc register
  int sp;	// sp register
  int bp;	// bp is not saved to stack
  int mark;	// return point register
  value acc;	// acc register
  value* stack;
  value env; // env register

  struct Handle *handle; // dylib handle
};

static void
vm_init(struct VM* vm, int size) {
  vm->pc = 0;
  vm->sp = 0;
  vm->bp = 0;
  vm->mark = 0;
  vm->acc = value_unit;
  vm->stack = (value*)malloc(sizeof(value)*size);
  vm->env = (value)NULL;
  return;
}

struct VM*
vm_new(int sz) {
  struct VM *vm = (struct VM*)malloc(sizeof(*vm));
  vm_init(vm, sz);
  return vm;
}

void
vm_close(struct VM* vm) {
  handle_destroy(vm->handle);
  free(vm->stack);
  free(vm);
}


value
vm_run(struct VM* vm, char* code) {
  while (code[vm->pc] != STOP) {
    switch(code[vm->pc]) {
    case PUSHADDR:
      {
        int32_t size = read_uint32(&code[vm->pc+1]);
        printf("PUSHADDR sp=%d mark=%d pc=%d\n", vm->sp, vm->mark, vm->pc+size+5);
        vm->stack[vm->sp] = vm->env;
        vm->sp++;
        vm->stack[vm->sp] = vm->pc+5;
        vm->sp++;
        vm->stack[vm->sp] = vm->mark;
        vm->mark = vm->sp;
        vm->sp++;
        vm->pc = vm->pc + size + 5;
      }
      break;
    case CONST:
      {
        value v = read_value(&code[vm->pc+1]);
        printf("CONST %lu\n", v>>1);
        vm->acc = v;
        vm->pc += 9;
      }
      break;
    case CLOSURE:
      {
        int32_t size = read_uint32(&code[vm->pc+1]);
        printf("CLOSURE\n");
        if (vm->bp > 0) {
          value new_env = env_append(vm->env, &vm->stack[vm->bp], vm->sp-vm->bp);
          vm->acc = new_closure(vm->pc+6, new_env);
        } else {
          vm->acc = new_closure(vm->pc+6, vm->env);
        }
        vm->pc = vm->pc + size + 5;
      }
      break;
    case APPLY:
      printf("APPLY\n");
      vm->pc = closure_pc(vm->acc);
      vm->env = closure_env(vm->acc);
      break;
    case GRAB:
      {
        uint8_t n =  code[vm->pc+1];
        if (vm->sp - vm->mark > n) {
          printf("GRAB: want %d args, have %d args\n", n, vm->sp - vm->mark -1);
          vm->bp = vm->sp-n;
          vm->pc += 2;
        } else {
          printf("GRAB partial apply...\n");
          // TODO what about manual release it instead of GC
          value new_env = env_append(closure_env(vm->acc), &vm->stack[vm->mark+1], vm->sp-vm->mark-1);
          closure_set_env(vm->acc, new_env);
          closure_set_pc(vm->acc, vm->pc-1); // RESTART

          vm->env = vm->stack[vm->mark-2];
          vm->pc = vm->stack[vm->mark-1];
          vm->sp = vm->mark-2;
          vm->mark = vm->stack[vm->mark];
        }
      }
      break;
    case RESTART:
      {
        // TODO order!!!
        printf("RESTART\n");
        int len = env_length(vm->env);
        for (int i=0; i<len; i++) {
          vm->stack[vm->sp] = env_get(vm->env, i);
          vm->sp++;
        }
        vm->env = (value)NULL;
        vm->pc++;
      }
      break;
    case STACKACC:
      {
        int n = code[vm->pc+1];
        vm->acc = vm->stack[vm->bp+n];
        vm->pc += 2;
        printf("STACKACC: %d, bp=%d\n", n, vm->bp);
      }
      break;
    case ENVACC:
      {
        int n = code[vm->pc+1];
        vm->acc = env_get(vm->env, n);
        vm->pc += 2;
        printf("ENVACC: %d\n", n);
      }
      break;
    case PUSH:
      printf("PUSH: sp=%d\n", vm->sp);
      vm->stack[vm->sp] = vm->acc;
      vm->sp++;
      vm->pc++;
      break;
    case ADDINT:
      printf("ADDINT\n");
      vm->acc = value_add(vm->stack[vm->sp-1], vm->acc);
      vm->sp--;
      vm->pc++;
      break;
    case SUBINT:
      printf("SUBINT\n");
      vm->acc = value_sub(vm->stack[vm->sp-1], vm->acc);
      vm->sp--;
      vm->pc++;
      break;
    case MULINT:
      printf("MULINT\n");
      vm->acc = value_mul(vm->stack[vm->sp-1], vm->acc);
      vm->sp--;
      vm->pc++;
      break;
    case DIVINT:
      printf("DIVINT\n");
      vm->acc = value_div(vm->stack[vm->sp-1], vm->acc);
      vm->sp--;
      vm->pc++;
      break;
    case EQ:
      printf("EQ\n");
      if (vm->acc == vm->stack[vm->sp-1]) {
        vm->acc = value_true;
      } else {
        vm->acc = value_false;
      }
      vm->sp--;
      vm->pc++;
      break;
    case RETURN:
      if (vm->bp > vm->mark + 1) {
        printf("RETURN: more args apply %d sp=%d mark=%d\n", code[vm->pc+1], vm->sp, vm->mark);
        vm->sp = vm->bp;
        vm->env = closure_env(vm->acc);
        vm->pc = closure_pc(vm->acc);
      } else {
        printf("RETURN: %d sp=%d mark=%d\n", code[vm->pc+1], vm->sp, vm->mark);
        vm->env = vm->stack[vm->mark-2];
        vm->pc = vm->stack[vm->mark-1];
        vm->sp = vm->mark-2;
        vm->mark = vm->stack[vm->mark];
      }
      vm->bp = 0;
      break;
    case BRANCH:
      printf("BRANCH\n");
      vm->pc = vm->pc+5+read_uint32(&code[vm->pc+1]);
      break;
    case BRANCHIF:
      if (vm->acc == value_true) {
        printf("BRANCHIF true\n");
        vm->pc = vm->pc+5+read_uint32(&code[vm->pc+1]);
      } else {
        printf("BRANCHIF false\n");
        vm->pc += 5;
      }
      break;
    case MAKEBLOCK:
      {
        printf("MAKEBLOCK\n");
        uint32_t tag = read_uint32(&code[vm->pc+1]);
        uint32_t size = read_uint32(&code[vm->pc+5]);
        value t =  new_tuple(tag, size);
        if (size > 0) {
          tuple_set(t, 0, vm->acc);
          for (int i=1; i<size; i++) {
            tuple_set(t, i, vm->stack[vm->sp-1]);
            vm->sp--;
          }
        }
        vm->acc = t;
        vm->pc += 9;
      }
      break;
    case GETFIELD:
      printf("GETFIELD\n");
      vm->acc = tuple_get(vm->acc, read_uint32(&code[vm->pc+1]));
      vm->pc += 5;
      break;
    case SWITCH:
      {
        printf("SWITCH\n");
        uint32_t n = read_uint32(&code[vm->pc+1]);
        int ofst = 0;
        for (int i=0; i<n; i++) {
          uint32_t tag = read_uint32(&code[vm->pc+5+i*8]);
          uint32_t size = read_uint32(&code[vm->pc+9+i*8]);
          if (tag == tuple_tag(vm->acc)) {
            break;
          }
          ofst += size;
        }
        vm->pc = vm->pc+5+8*n+ofst;
      }
      break;
    case CCALL:
      {
        uint32_t n = read_uint32(&code[vm->pc+1]);
        char *str = value_string(vm->acc);
        char *prim = strchr(str, '.');
        if (prim == NULL) {
          // TODO
        }
        int len = prim-str;
        char *dylib = alloca(len+1);
        memcpy(dylib, str, len);
        dylib[len] = 0;
        prim++;

        printf("CCALL: n=%d %s %s\n", n, prim, dylib);

        void *fn_ptr = handle_get(vm->handle, prim, dylib);
        if (fn_ptr == NULL) {
          // TODO check
        }
        vm->acc = c_call(vm, fn_ptr, n);
        vm->sp -= (n-1);
        vm->pc += 5;
      }
      break;
    case STRING:
      {
        uint32_t n = read_uint32(&code[vm->pc+1]);
        printf("STRING: %s\n", &code[vm->pc+5]);
        vm->acc = new_string(&code[vm->pc+5], n);
        vm->pc += 5+n+1;
      }
    }
  }
  return vm->acc;
}

static value
c_call(struct VM* vm, void *ptr, int n) {
  switch (n) {
  case 0:
    {
      value (*fn_ptr)() = ptr;
      return fn_ptr();
    }
  case 1:
    {
      value (*fn_ptr)(value) = ptr;
      value ret = fn_ptr(vm->stack[vm->sp-1]);
      return ret;
    }
  case 2:
    {
      value (*fn_ptr)(value, value) = ptr;
      return fn_ptr(vm->stack[vm->sp-1], vm->stack[vm->sp-2]);
    }
  case 3:
    {
      value (*fn_ptr)(value,value,value) = ptr;
      return fn_ptr(vm->stack[vm->sp-1], vm->stack[vm->sp-2],
                    vm->stack[vm->sp-3]);
    }
  case 4:
    {
      value (*fn_ptr)(value,value,value, value) = ptr;
      return fn_ptr(vm->stack[vm->sp-1], vm->stack[vm->sp-2],
                    vm->stack[vm->sp-3], vm->stack[vm->sp-4]);
    }
  case 5:
    {
      value (*fn_ptr)(value,value,value,value,value) = ptr;
      return fn_ptr(vm->stack[vm->sp-1],vm->stack[vm->sp-2],
                    vm->stack[vm->sp-3],vm->stack[vm->sp-4],
                    vm->stack[vm->sp-5]);
    }
  case 6:
    {
      value (*fn_ptr)(value,value,value,value,value,value) = ptr;
      return fn_ptr(vm->stack[vm->sp-1],vm->stack[vm->sp-2],
                    vm->stack[vm->sp-3],vm->stack[vm->sp-4],
                    vm->stack[vm->sp-5],vm->stack[vm->sp-6]);
    }
  }
  return value_unit;
}
