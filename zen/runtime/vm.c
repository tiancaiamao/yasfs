#include "value.h"
#include "instruct.h"
#include "util.h"
#include <stdlib.h>
#include <stdio.h>

struct VM {
  int pc;
  int sp;
  int bp; // bp is not saved to stack
  int mark;
  value acc;
  value* stack;
  value env;
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
        // TODO should copy stack to env
        vm->acc = new_closure(vm->pc+6, vm->env);
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
          vm->bp = vm->sp-1;
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
    case STACKACC:
      {
        int n = code[vm->pc+1];
        vm->acc = vm->stack[vm->bp-n];
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
    case RETURN:
      printf("RETURN sp=%d mark=%d\n", vm->sp, vm->mark);

      vm->env = vm->stack[vm->mark-2];
      vm->pc = vm->stack[vm->mark-1];
      vm->sp = vm->mark-2;
      vm->mark = vm->stack[vm->mark];
      break;
    }
  }
  return vm->acc;
}
