#include "sexp.h"

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

sexp
sr_extend(sexp env, sexp vals) {
	return sexp_cons(NULL, vals, env);
}

// TODO GC
sexp 
allocate_activation_frame(int size) {
	return sexp_make_vector(NULL, sexp_make_fixnum(size), SEXP_ZERO);
}

int allocate_activation_frame_length(sexp vals) {
	return sexp_vector_length(vals);
}

sexp
activation_frame_argument(sexp env, int i) {
	return sexp_vector_data(sexp_car(env))[i];
}

void
set_activation_frame_argument(sexp sr, int i, sexp val) {
	sexp_vector_data(sr)[i]=val;
}

sexp 
sexp_make_closure(int pc, sexp env) {
  sexp proc = sexp_alloc_type(NULL, pair, SEXP_PROCEDURE);
  sexp_field(proc, closure, SEXP_PROCEDURE, pc) = pc;
  sexp_field(proc, closure, SEXP_PROCEDURE, env) = env;
  return proc;
}

#define sexp_closure_env(x) (sexp_field(x, closure, SEXP_PROCEDURE, env))
#define sexp_closure_pc(x) (sexp_field(x, closure, SEXP_PROCEDURE, pc))

static int
CONSTANT(struct vm *vm) {
	int ret = 1;
	switch (vm->code[vm->pc]) {
		case 10:
			vm->val = SEXP_TRUE;
			break;
		case 11:
			vm->val = SEXP_FALSE;
			break;
		case 12:
			vm->val = SEXP_NULL;
			break;
		case 80:
			vm->val = sexp_make_fixnum(-1);
			break;
		case 81:
			vm->val = sexp_make_fixnum(0);
			break;
		case 82:
			vm->val = sexp_make_fixnum(1);
			break;
		case 83:
			vm->val = sexp_make_fixnum(2);
			break;
		case 84:
			vm->val = sexp_make_fixnum(3);
			break;
		case 79:
		 	vm->val = sexp_make_fixnum(vm->code[vm->pc+1]);
			ret = 2;
			break;
		default:
			return -1;
	}
	return ret;
}

static void
GLOBAL_SET(struct vm *vm) {

}

static int
PUSH_VALUE(struct vm *vm) {
	vm->stack[vm->idx] = vm->val;
	vm->idx++;
	return 1;
}

static int
POP_FUNCTION(struct vm *vm) {
	vm->idx--;
	vm->fun = vm->stack[vm->idx];
	return 1;
}

static void
PRESERVE_ENV(struct vm *vm) {
	vm->idx--;
	vm->env = vm->stack[vm->idx];
}

static void
RESTORE_ENV(struct vm *vm) {
	vm->idx--;
	vm->env = vm->stack[vm->idx];
}

static int
FINISH(struct vm *vm) {
	return 0;
}

static int
CREATE_CLOSURE(struct vm *vm) {
	int offset = vm->code[vm->pc + 1];
	vm->val = sexp_make_closure(vm->pc + offset + 2, vm->env);
	return 2;
}

static int
FUNCTION_INVOKE(struct vm *vm) {
	sexp closure = vm->fun;

	if (sexp_procedurep(closure)) {
		sexp pc = sexp_make_fixnum(vm->pc);
		vm->stack[vm->idx] = pc;
		vm->idx++;
		
		vm->env = sexp_closure_env(closure);
		vm->pc = sexp_closure_pc(closure);
	} else {
		return -1;
	}
	return 1;
}

static int
INVOKE1(struct vm *vm) {
	switch (vm->code[vm->pc]) {
		case 90:
			vm->val = sexp_car(vm->val);
			break;
		case 91:
			vm->val = sexp_cdr(vm->val);
			break;
		case 92:
			vm->val = sexp_pairp(vm->val) ? SEXP_TRUE : SEXP_FALSE;
			break;
		case 93:
			vm->val = sexp_symbolp(vm->val) ? SEXP_TRUE:SEXP_FALSE;
			break;
		case 94:
			// TODO
			// vm->val = sexp_display(NULL, vm->val, NULL);
			break;
		case 95:
			vm->val = sexp_procedurep(vm->val) ? SEXP_TRUE : SEXP_FALSE;
			break;
		case 96:
			vm->val = sexp_nullp(vm->val) ? SEXP_TRUE : SEXP_FALSE;
			break;
		case 97:
			// TODO continuation
			break;
		case 98:
			vm->val = (vm->val == SEXP_EOF) ? SEXP_TRUE : SEXP_FALSE;
			break;
		default:
			return -1;
	}
	return 1;
}

static int
INVOKE2(struct vm *vm) {
	switch (vm->code[vm->pc]) {
		case 100:
			vm->val = sexp_cons(NULL, vm->arg1, vm->val);
			break;
		case 101:
			vm->val = vm->arg1 == vm->val ? SEXP_TRUE : SEXP_FALSE;
			break;
		case 102:
			sexp_car(vm->arg1) = vm->val;
			break;
		case 103:
			sexp_cdr(vm->arg1) = vm->val;
			break;
		case 104:
			vm->val = sexp_fx_add(vm->arg1, vm->val);
			break;
		case 105:
			vm->val = sexp_fx_sub(vm->arg1, vm->val);
			break;
		case 106:
			vm->val = sexp_equalp(NULL, vm->arg1, vm->val);
			break;
		case 107:
			vm->val = sexp_unbox_fixnum(vm->arg1) > sexp_unbox_fixnum(vm->arg2) ? SEXP_TRUE : SEXP_FALSE;
			break;
		case 108:
			vm->val = sexp_unbox_fixnum(vm->arg1) < sexp_unbox_fixnum(vm->arg2) ? SEXP_TRUE : SEXP_FALSE;
			break;
		case 109:
			vm->val = sexp_make_fixnum(sexp_unbox_fixnum(vm->arg1) * sexp_unbox_fixnum(vm->arg2));
			break;
		case 110:
			vm->val = sexp_unbox_fixnum(vm->arg1) <= sexp_unbox_fixnum(vm->arg2) ? SEXP_TRUE : SEXP_FALSE;
			break;
		case 111:
			vm->val = sexp_unbox_fixnum(vm->arg1) >= sexp_unbox_fixnum(vm->arg2) ? SEXP_TRUE : SEXP_FALSE;
			break;
		case 112:
			vm->val = sexp_make_fixnum(sexp_unbox_fixnum(vm->arg1) % sexp_unbox_fixnum(vm->arg2));
			break;
		default:
			return -1;
	}
	return 1;
}

static int
RETURN(struct vm *vm) {
	vm->idx--;
	vm->pc = sexp_unbox_fixnum(vm->stack[vm->idx]);
	return 1;
}

static int
SHALLOW_ARGUMENT_REF(struct vm *vm) {
	char pc = vm->code[vm->pc];
	
	if (pc >=1 && pc < 5) {
		vm->val = activation_frame_argument(vm->env, pc - 1);
		return 1;
	} else if (pc == 5) {
		vm->val = activation_frame_argument(vm->env, vm->code[pc+1]);
		return 2;
	}
	return -1;
}

/*
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
		printf("Uninitialized global variable");
	}
}

static void
SET_GLOBAL(struct vm *vm) {
	int i = vm->code[vm->pc];
	global_update(i, vm->val);
}
*/

static int
GOTO(struct vm *vm) {
	int offset2;
	int offset1 = vm->code[vm->pc+1];
	int op = vm->code[vm->pc];

	if (op == 30) {
		vm->pc += offset1;
		return 2;
	} else if (op == 28) {
		offset2 = vm->code[vm->pc+2];
		vm->pc += (offset1 + offset2 * 255);
		return 3;
	}
	return -1;
}

static int
JUMP_FALSE(struct vm *vm) {
	int op = vm->code[vm->pc];
	
	if (op == 31) {
		if (sexp_not(vm->val)) {
			int offset = vm->code[vm->pc+1];
			vm->pc += offset;
		}
		return 2;
	} else if (op == 29) {
		if (sexp_not(vm->val)) {
			int offset1 = vm->code[vm->pc+1];
			int offset2 = vm->code[vm->pc+2];
			vm->pc += (offset1 + offset2 * 256);
		}
		return 3;
	}
	return -1;
}

static int
ALLOCATE_FRAME(struct vm *vm) {
	char pc = vm->code[vm->pc];

	if (pc >= 50 && pc < 55) {
		vm->val = allocate_activation_frame(pc - 50);
		return 1;
	} else if (pc == 55) {
		vm->val = allocate_activation_frame(vm->code[pc+1]);
		return 2;
	} 
	return -1;
}

static int
POP_FRAME(struct vm *vm) {
	char pc = vm->code[vm->pc];
	sexp top;
	
	top = vm->stack[vm->idx-1];	
	vm->idx--;
	
	if (pc >= 60 && pc < 64) {
		set_activation_frame_argument(vm->val, pc-60, top);
		return 1;
	} else if (pc == 64) {
		set_activation_frame_argument(vm->val, vm->code[pc+1], top);
		return 2;
	}
	return -1;
}

static void
CALL1_CAR(struct vm *vm) {
	vm->val = sexp_car(vm->val);
}

static void
CALL1_CDR(struct vm *vm) {
	vm->val = sexp_cdr(vm->val);
}

static int
EXTEND_ENV(struct vm *vm) {
	vm->env = sr_extend(vm->env, vm->val);
	return 1;
}

static int
POP_ARG1(struct vm *vm) {
	vm->idx--;
	vm->arg1 = vm->stack[vm->idx];
	return 1;
}

/*
static void
POP_ARG2(struct vm *vm) {
	vm->idx--;
	vm->arg2 = vm->stack[vm->idx];
}


static void
FUNCTION_GOTO(struct vm *vm) {
	invoke(vm, true);
}
*/

static int
ARITYEQ(struct vm *vm) {
	char op = vm->code[vm->pc];
	if (op >= 71 && op < 75) {
		if (allocate_activation_frame_length(vm->val) != op-70) {
			return -2;
		}
		return 1;
	} else if (op == 75) {
		if (allocate_activation_frame_length(vm->val) != vm->code[vm->pc+1]) {
			return -2;
		}
		return 2;
	} 
	return -1;
}

typedef int (*inst_t)(struct vm *vm);
inst_t instructions[256];

void 
initialize() {
	instructions[1] = SHALLOW_ARGUMENT_REF;
	instructions[2] = SHALLOW_ARGUMENT_REF;
	instructions[3] = SHALLOW_ARGUMENT_REF;
	instructions[4] = SHALLOW_ARGUMENT_REF;
	instructions[5] = SHALLOW_ARGUMENT_REF;

	instructions[10] = CONSTANT;
	instructions[11] = CONSTANT;
	instructions[12] = CONSTANT;

	instructions[20] = FINISH;
	instructions[28] = GOTO;
	instructions[29] = JUMP_FALSE;
	instructions[30] = GOTO;
	instructions[31] = JUMP_FALSE;
	instructions[32] = EXTEND_ENV;
	// instructions[33] = UNLINK_ENV;
	instructions[34] = PUSH_VALUE;
	instructions[35] = POP_ARG1;
	// instructions[36] = POP_ARG2;
	// instructions[37] = PRESERVE_ENV;
	// instructions[38] = RESTORE_ENV;
	instructions[39] = POP_FUNCTION;
	instructions[40] = CREATE_CLOSURE;
	instructions[43] = RETURN;
	instructions[45] = FUNCTION_INVOKE;
	// instructions[46] = FUNCTION_GOTO;

	instructions[50] = ALLOCATE_FRAME;
	instructions[51] = ALLOCATE_FRAME;
	instructions[52] = ALLOCATE_FRAME;
	instructions[53] = ALLOCATE_FRAME;
	instructions[54] = ALLOCATE_FRAME;
	instructions[55] = ALLOCATE_FRAME;

	instructions[60] = POP_FRAME;
	instructions[61] = POP_FRAME;
	instructions[62] = POP_FRAME;
	instructions[63] = POP_FRAME;
	instructions[64] = POP_FRAME;
	
	instructions[71] = ARITYEQ;
	instructions[72] = ARITYEQ;
	instructions[73] = ARITYEQ;
	instructions[74] = ARITYEQ;
	instructions[75] = ARITYEQ;
	
	instructions[79] = CONSTANT;
	instructions[80] = CONSTANT;
	instructions[81] = CONSTANT;
	instructions[82] = CONSTANT;
	instructions[83] = CONSTANT;
	instructions[84] = CONSTANT;

	instructions[90] = INVOKE1;
	instructions[91] = INVOKE1;
	instructions[92] = INVOKE1;
	instructions[93] = INVOKE1;
	instructions[94] = INVOKE1;
	instructions[95] = INVOKE1;
	instructions[96] = INVOKE1;
	instructions[97] = INVOKE1;
	instructions[98] = INVOKE1;

	instructions[100] = INVOKE2;
	instructions[101] = INVOKE2;
	instructions[102] = INVOKE2;
	instructions[103] = INVOKE2;
	instructions[104] = INVOKE2;
	instructions[105] = INVOKE2;
	instructions[106] = INVOKE2;
	instructions[107] = INVOKE2;
	instructions[108] = INVOKE2;
	instructions[109] = INVOKE2;
	instructions[110] = INVOKE2;
	instructions[111] = INVOKE2;
	instructions[112] = INVOKE2;
}

int
run(struct vm *vm, char *code) {
	int offset;
	int op;
	
	vm->code = code;
	for ( ; ;) {
		op = code[vm->pc];
		offset = instructions[op](vm);
		
		if (offset < 0) {
			return offset;
		} else if (offset == 0) {
			break;
		} else {
			vm->pc += offset;			
		}
	}
	return 0;
}

void
vm_init(struct vm *vm) {
	vm->idx = 0;
	vm->pc = 0;

	vm->env = SEXP_VOID;
	vm->val = SEXP_VOID;
	vm->fun = SEXP_VOID;
	vm->arg1 = SEXP_VOID;
	vm->arg2 = SEXP_VOID;		
}

int
main(int argc, char *argv[]) {
	struct vm vm;
	// char bytecode[] = {82, 34, 83, 34, 52, 61, 60, 32, 1, 34, 2, 35, 104, 20};
	// char bytecode[] = {40, 2, 30, 8, 72, 32, 1, 34, 82, 35, 104, 43, 20};
	// char bytecode[] = {10, 31, 3, 82, 30, 1, 83, 20};
	char bytecode[] = {40, 2, 30, 4, 72, 32, 1, 43, 34, 51, 60, 32, 1, 34, 84, 34, 51, 60, 39, 45, 20};
	int succ;
	
	initialize();
	vm_init(&vm);
	succ = run(&vm, bytecode);
	printf("%ld", sexp_unbox_fixnum(vm.val));
	return 0;
}
