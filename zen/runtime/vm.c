#include "value.h"
#include "instruct.h"
#include "util.h"
#include <stdlib.h>
#include <stdio.h>

struct Ctx {
  int mark;
  int sp;
  int* stack;
};

static void
ctx_push(struct Ctx *ctx, int v) {
  ctx->stack[ctx->sp] = v;
  ctx->sp++;
}

static int
ctx_pop(struct Ctx *ctx) {
  ctx->sp--;
  return ctx->stack[ctx->sp];
}

struct VM {
  int pc;
  int sp;
  int bp;
  value acc;
  value* stack;
  value env;

  // ctx is not register but a sperate memory block.
  struct Ctx ctx;
};

void
ctx_init(struct Ctx* ctx) {
  ctx->mark = 0;
  ctx->stack = (int*)malloc(sizeof(int) * 50);
  ctx->sp = 0;
}

static void
vm_init(struct VM* vm, int size) {
  vm->pc = 0;
  vm->sp = 0;
  vm->bp = 0;
  vm->acc = value_unit;
  vm->stack = (value*)malloc(sizeof(value)*size);
  vm->env = (value)NULL;
  ctx_init(&vm->ctx);
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
    case MARK:
      printf("MARK sp=%d\n", vm->sp);
      vm->ctx.mark = vm->sp;
      vm->pc++;
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
        vm->acc = new_closure(vm->pc+5, vm->env);
        vm->pc = vm->pc + size + 5;
      }
      break;
    case APPLY:
      {
        printf("APPLY\n");
        ctx_push(&vm->ctx, vm->pc+1);
        vm->pc = closure_pc(vm->acc);
      }
      break;
    case CHECK:
      {
        uint8_t n =  code[vm->pc+1];
        int len = env_length(closure_env(vm->acc));
        if (len + vm->sp - vm->ctx.mark >= n) {
          printf("CHECK: want %d args, have %d args\n", n, vm->sp - vm->ctx.mark);
          vm->pc += 2;
        } else {
          printf("CHECK partial apply...\n");
          // TODO what about manual release it instead of GC
          value new_env = env_append(closure_env(vm->acc), &vm->stack[vm->ctx.mark], vm->sp-vm->ctx.mark);
          closure_set_env(vm->acc, new_env);
          vm->sp = vm->ctx.mark;
          vm->pc = ctx_pop(&vm->ctx);
        }
      }
      break;
    case ENV:
      printf("ENV stk=%d env=%d\n", vm->sp - vm->bp, env_length(vm->env));
      ctx_push(&vm->ctx, vm->bp);
      vm->bp = vm->sp - 1;
      vm->pc++;
      break;
    case UNENV:
      vm->bp = ctx_pop(&vm->ctx);
      printf("UNENV: recover bp %d\n", vm->bp);
      vm->pc++;
      break;
    case ACCESS:
      {
        int n = code[vm->pc+1];
        int sz = env_length(vm->env);
        if (sz > n) {
          vm->acc = env_get(vm->env, n);
        } else {
          vm->acc = vm->stack[vm->bp-n-sz];
        }
        vm->pc += 2;
        printf("ACCESS: %d\n", n);
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
    case RETURN:
      vm->sp = vm->bp;
      vm->pc = ctx_pop(&vm->ctx);
      printf("RETURN sp=%d\n", vm->sp);
      break;
    }
  }
  return vm->acc;
}