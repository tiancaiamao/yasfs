#include "sexp.h"

#define CONSTANT 1
#define GLOBAL_SET 2

struct vm {
	sexp stack[1000];
	int idx;

	char *code;
	int pc;

	sexp env;
	sexp val;
	sexp fun;
	sexp arg1;
	sexp arg2;
};

static void
CONSTANT(struct vm *vm) {
	vm->val = vm->code[pc];
}

static void
GLOBAL_SET(struct vm *vm) {

}

static void
PUSH_VALUE(struct vm *vm) {
	vm->stack[vm->idx] = vm->val;
	vm->idx++;
}

static void
POP_FUNCTION(struct vm *vm) {
	vm->idx--;
	vm->fun = vm->stack[idx];
}

static void
PRESERVE_ENV(struct vm *vm) {
	vm->idx--;
	vm->env = vm->stack[idx];
}

static void
RESTORE_ENV(struct vm *vm) {
	vm->idx--;
	vm->env = vm->stack[idx];
}

static void
JUMP_FALSE(struct vm *vm) {
	if (sexp_not(vm->val)) {
		vm->pc++;
		vm->pc = vm->code[vm->pc];
	}
}

static void
GOTO(struct vm *vm) {
	vm->pc++;
	vm->pc = vm->code[vm->pc];
}

static void
CREATE_CLOSURE(struct vm *vm) {
	int offset = vm->code[vm->pc];
	vm->val = sexp_make_closure(vm->pc, vm->env);
}

static void
FUNCTION_INVOKE(struct vm *vm) {
	sexp closure = vm->fun;

	vm->stack[vm->idx] = vm->pc;
	vm->idx++;

	vm->env = sexp_closure_env(closure);
	vm->pc = sexp_closure_pc(closure);
}

static void
RETURN(struct vm *vm) {
	vm->pc--;
	vm->pc = vm->stack[vm->idx];
}

static void
SHALLOW_ARGUMENT_REF0(struct vm *vm) {
	vm->val = activation_frame_argument(vm->env, 0);
}

static void
SHALLOW_ARGUMENT_REF1(struct vm *vm) {
	vm->val = activation_frame_argument(vm->env, 1);
}

static void
SHALLOW_ARGUMENT_REF2(struct vm *vm) {
	vm->val = activation_frame_argument(vm->env, 2);
}

static void
SHALLOW_ARGUMENT_REF3(struct vm *vm) {
	vm->val = activation_frame_argument(vm->env, 3);
}

static void
SHALLOW_ARGUMENT_REF(struct vm *vm) {
	vm->val = activation_frame_argument(vm->env, vm->code[vm->pc]);
}

static void
SET_SHALLOW_ARGUMENT(struct vm *vm) {
	set_activation_frame_argument(vm->env, 2, vm->val);
}

static void
DEEP_ARGUMENT_REF(struct vm *vm) {
	char i, j;
	i = vm->code[vm->pc];
	vm->pc++;
	j = vm->code[vm->pc];
	vm->val = deep_fetch(vm->env, i, j);
}

static void
SET_DEEP_ARGUMENT(struct vm *vm) {
	deep_update(vm->env, i, j, vm->val);
}

static void
GLOBAL_REF(struct vm *vm) {
	int i = vm->code[vm->pc];
	vm->val = global_fetch(i);
}

static void
CHECKED_GLOBAL_REF(struct vm *vm) {
	int i = vm->code[vm->pc];
	vm->val = global_fetch(i);
	if (vm->val == SEXP_VOID) {
		printf("Uninitialized global variable")
	}
}

static void
SET_GLOBAL(struct vm *vm) {
	int i = vm->code[vm->pc];
	global_update(i, vm->val);
}

static void
SHORT_GOTO(struct vm *vm) {
	int offset = vm->code[vm->pc];
	vm->pc = vm->pc + offset;
}

static void
SHORT_JUMP_FALSE(struct vm *vm) {
	if (sexp_not(vm->val)) {
		int offset = vm->code[vm->pc];
		vm->pc = vm->pc + offset;
	}
}

static void
LONG_GOTO(struct vm *vm) {
	int offset1;
	int offset2;
	int offset;

	offset1 = vm->code[vm->pc];
	vm->pc++;
	offset2 = vm->code[vm->pc];
	vm->pc++;
	offset = offset1 + offset * 256;
	vm->pc = vm->pc + offset;
}

static void
ALLOCATE_FRAME0(struct vm *vm) {
	vm->val = allocate_activation_frame(0);
}

static void
ALLOCATE_FRAME1(struct vm *vm) {
	vm->val = allocate_activation_frame(1);
}

static void
ALLOCATE_FRAME2(struct vm *vm) {
	vm->val = allocate_activation_frame(2);
}

static void
ALLOCATE_FRAME3(struct vm *vm) {
	vm->val = allocate_activation_frame(3);
}

static void
ALLOCATE_FRAME(struct vm *vm) {
	int size = vm->code[vm->pc];
	vm->val = allocate_activation_frame(size);
}

static void
POP_FRAME(struct vm *vm) {
	vm->idx--;
	vm->val = vm->stack[vm->idx];
}

static void
CALL1_CAR(struct vm *vm) {
	vm->val = sexp_car(vm->val);
}

static void
CALL1_CDR(struct vm *vm) {
	vm->val = sexp_cdr(vm->val);
}

static void
EXTEND_ENV(struct vm *vm) {
	vm->env = sr_extend(vm->env, vm->val);
}

static void
POP_ARG1(struct vm *vm) {
	vm->idx--;
	vm->arg1 = vm->stack[idx];
}

static void
POP_ARG2(struct vm *vm) {
	vm->idx--;
	vm->arg2 = vm->stack[idx];
}

static void
FUNCTION_GOTO(struct vm *vm) {
	invoke(vm, true);
}

static void
FUNCTION_INVOKE(struct vm *vm) {
	invoke(vm, false);
}

typedef int (inst_t)(struct vm *vm);
static inst_t instructions[256];

instructions[28] = LONG_GOTO;
instructions[30] = SHORT_GOTO;
instructions[31] = SHORT_JUMP_FALSE;
instructions[32] = EXTEND_ENV;
// instructions[33] = UNLINK_ENV;
instructions[34] = PUSH_VALUE;
instructions[35] = POP_ARG1;
instructions[36] = POP_ARG2;
instructions[37] = PRESERVE_ENV;
instructions[38] = RESTORE_ENV;
instructions[39] = POP_FUNCTION;
instructions[40] = CREATE_CLOSURE;
instructions[43] = RETURN;
instructions[45] = FUNCTION_INVOKE;
instructions[46] = FUNCTION_GOTO;


instructions[55] = ALLOCATE_FRAME;
instructions[64] = POP_FRAME;

int run(struct vm *vm, sexp code) {
	int offset;
	
	vm->code = code;
	for ( ; ;) {
		offset = instructions[vm->pc](vm);
		if (offset < 0) {
			break;
		}
		vm->pc += offset;
	}
}
