#include "sexp.h"
#include <stdlib.h>

struct vm {
	sexp stack[1000];
	int idx;
	
	sexp env;
	sexp val;
	sexp fun;
	sexp arg1;
	sexp arg2;

	char *code;
	int pc;	

	sexp global;
	sexp ctx;
};

// otherwise  can't compile
sexp sexp_apply (sexp ctx, sexp proc, sexp args) {
	return NULL;
}
sexp sexp_make_foreign (sexp ctx, const char *name, int num_args,
                        int flags, sexp_proc1 f, sexp data) {
							return NULL;
}

sexp
deep_fetch(sexp env, int i, int j) {
	sexp tmp = env;
	
	while (i > 0) {
		tmp = sexp_cdr(tmp);
		i--;
	}
	return sexp_vector_data(sexp_car(tmp))[j];
}

void
deep_update(sexp env, int i, int j, sexp val) {
	sexp tmp = env;
	
	while (i > 0) {
		tmp = sexp_cdr(tmp);
		i--;
	}
	sexp_vector_data(sexp_car(tmp))[j] = val;
}

sexp
sr_extend(sexp ctx, sexp env, sexp vals) {
	return sexp_cons(ctx, vals, env);
}

// TODO GC
sexp 
allocate_activation_frame(sexp ctx, int size) {
	return sexp_make_vector(ctx, sexp_make_fixnum(size), SEXP_ZERO);
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
sexp_make_closure(sexp ctx, int pc, sexp env) {
  sexp proc = sexp_alloc_type(ctx, closure, SEXP_PROCEDURE);
  sexp_field(proc, closure, SEXP_PROCEDURE, pc) = pc;
  sexp_field(proc, closure, SEXP_PROCEDURE, env) = env;
  return proc;
}

#define sexp_closure_env(x) (sexp_field(x, closure, SEXP_PROCEDURE, env))
#define sexp_closure_pc(x) (sexp_field(x, closure, SEXP_PROCEDURE, pc))

sexp 
sexp_make_primitive(sexp ctx, int code, int code1) {
	sexp prim = sexp_alloc_type(ctx, primitive, SEXP_OPCODE);
	sexp_field(prim, primitive, SEXP_OPCODE, code) = code;
	sexp_field(prim, primitive, SEXP_OPCODE, code1) = code1;
	return prim;
}

#define sexp_primitive_code(x) (sexp_field(x, primitive, SEXP_OPCODE, code))
#define sexp_primitive_code1(x) (sexp_field(x, primitive, SEXP_OPCODE, code1))

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

static int
PRESERVE_ENV(struct vm *vm) {
	vm->stack[vm->idx] = vm->env;
	vm->idx++;
	return 1;
}

static int
RESTORE_ENV(struct vm *vm) {
	vm->idx--;
	vm->env = vm->stack[vm->idx];
	return 1;
}

static int
FINISH(struct vm *vm) {
	return 0;
}

static int
CREATE_CLOSURE(struct vm *vm) {
	int offset = vm->code[vm->pc + 1];
	vm->val = sexp_make_closure(vm->ctx, vm->pc + offset + 2, vm->env);
	return 2;
}

static int
FUNCTION_INVOKE(struct vm *vm) {
	sexp fun = vm->fun;

	if (sexp_procedurep(fun)) {
		sexp pc = sexp_make_fixnum(vm->pc);
		vm->stack[vm->idx] = pc;
		vm->idx++;
		
		vm->env = sexp_closure_env(fun);
		vm->pc = sexp_closure_pc(fun);
	} else if (sexp_opcodep(fun)) {
		switch (sexp_primitive_code(fun)) {
			case 10:
			case 13:
			// cons
			case 14:
			// car
			case 15:
			// cdr
			case 16:
			// pair?
			case 17:
			// symbol?
			case 18:
			// eq?
			case 19:
			default:
				break;
		// TODO	
		}
	} else {
		return -1;
	}
	return 1;
}

static int
INVOKE0(struct vm *vm) {
	switch (vm->code[vm->pc]) {
		case 88:
		//(read)
		case 89:
		// (newline)
		default:
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
			vm->val = sexp_cons(vm->ctx, vm->arg1, vm->val);
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
			vm->val = sexp_equalp(vm->ctx, vm->arg1, vm->val);
			break;
		case 107:
			vm->val = sexp_unbox_fixnum(vm->val) > sexp_unbox_fixnum(vm->arg1) ? SEXP_TRUE : SEXP_FALSE;
			break;
		case 108:
			vm->val = sexp_unbox_fixnum(vm->val) < sexp_unbox_fixnum(vm->arg1) ? SEXP_TRUE : SEXP_FALSE;
			break;
		case 109:
			vm->val = sexp_make_fixnum(sexp_unbox_fixnum(vm->arg1) * sexp_unbox_fixnum(vm->val));
			break;
		case 110:
			vm->val = sexp_unbox_fixnum(vm->val) <= sexp_unbox_fixnum(vm->arg1) ? SEXP_TRUE : SEXP_FALSE;
			break;
		case 111:
			vm->val = sexp_unbox_fixnum(vm->val) >= sexp_unbox_fixnum(vm->arg1) ? SEXP_TRUE : SEXP_FALSE;
			break;
		case 112:
			vm->val = sexp_make_fixnum(sexp_unbox_fixnum(vm->val) % sexp_unbox_fixnum(vm->arg1));
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
	char code = vm->code[vm->pc];
	
	if (code >=1 && code < 5) {
		vm->val = activation_frame_argument(vm->env, code - 1);
		return 1;
	} else if (code == 5) {
		vm->val = activation_frame_argument(vm->env, vm->code[code + 1]);
		return 2;
	}
	return -1;
}

static int
SET_SHALLOW_ARGUMENT(struct vm *vm) {
	int code = vm->code[vm->pc];
	
	if (code >= 21 && code < 25) {
		set_activation_frame_argument(sexp_car(vm->env), code-21, vm->val);
		vm->val = SEXP_VOID;
		return 1;
	} else if (code == 25) {
		set_activation_frame_argument(vm->env, vm->code[vm->pc + 1], vm->val);
		vm->val = SEXP_VOID;
		return 2;
	}
	return -1;
}

static int
DEEP_ARGUMENT_REF(struct vm *vm) {
	int i, j;
	i = vm->code[vm->pc + 1];
	j = vm->code[vm->pc + 2];
	vm->val = deep_fetch(vm->env, i, j);
	return 3;
}


static int
SET_DEEP_ARGUMENT(struct vm *vm) {
	int i, j;
	i = vm->code[vm->pc + 1];
	j = vm->code[vm->pc + 2];
	deep_update(vm->env, i, j, vm->val);
	vm->val = SEXP_VOID;
	return 3;
}


static int
PREDEFINED(struct vm *vm) {
	int op = vm->code[vm->pc];
	
	if (op >= 10 && op < 19) {
		vm->val = sexp_make_primitive(vm->ctx, op, 0);
		return 1;
	} else if (op == 19) {
		vm->val = sexp_make_primitive(vm->ctx, op, vm->code[vm->pc + 1]);
		return 2;
	}
	return -1;
}

static int
CHECKED_GLOBAL_REF(struct vm *vm) {
	int i = vm->code[vm->pc + 1];
	vm->val = sexp_vector_data(vm->global)[i];
	if (vm->val == SEXP_VOID) {
		return -3;	//Uninitialized global variable
	}
	return 2;
}

static int
GLOBAL_REF(struct vm *vm) {
	int i = vm->code[vm->pc + 1];
	vm->val = sexp_vector_data(vm->global)[i];
	return 2;
}

static int
SET_GLOBAL(struct vm *vm) {
	int i = vm->code[vm->pc + 1];
	sexp_vector_data(vm->global)[i] = vm->val;
	vm->val = SEXP_VOID;
	return 2;
}

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
ALLOCATE_DOTTED_FRAME(struct vm *vm) {
	int arity = vm->code[vm->pc + 1];
	vm->val = allocate_activation_frame(vm->ctx, arity);
	sexp_vector_data(vm->val)[arity - 1] = SEXP_NULL;
	return 2;
}					   

static int
ALLOCATE_FRAME(struct vm *vm) {
	char pc = vm->code[vm->pc];

	if (pc >= 50 && pc < 55) {
		vm->val = allocate_activation_frame(vm->ctx, pc - 50);
		return 1;
	} else if (pc == 55) {
		vm->val = allocate_activation_frame(vm->ctx, vm->code[pc+1]);
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

static int
POP_CONS_FRAME(struct vm *vm) {
	int arity = vm->code[vm->pc + 1];
	sexp tmp;
	
	vm->idx--;
	tmp = sexp_cons(vm->ctx, vm->stack[vm->idx], sexp_vector_data(vm->val)[arity]);
	set_activation_frame_argument(vm->val, arity, tmp);
	return 2;
}

static int
EXTEND_ENV(struct vm *vm) {
	vm->env = sr_extend(vm->ctx, vm->env, vm->val);
	return 1;
}

static int
POP_ARG1(struct vm *vm) {
	vm->idx--;
	vm->arg1 = vm->stack[vm->idx];
	return 1;
}


static int
POP_ARG2(struct vm *vm) {
	vm->idx--;
	vm->arg2 = vm->stack[vm->idx];
	return 1;
}

static int
PACK_FRAME(struct vm *vm) {
	int i;
	int arity = vm->code[vm->pc + 1];
	sexp tmp = SEXP_NULL;
	for (i = arity; i<sexp_vector_length(vm->val); i++) {
		tmp = sexp_cons(vm->ctx, sexp_vector_data(vm->val)[i], tmp);
	}
	set_activation_frame_argument(vm->val, arity, tmp);
	return 2;
}

static int
UNLINK_ENV(struct vm *vm) {
	vm->env = sexp_cdr(vm->env);
	return 1;
}

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
	instructions[6] = DEEP_ARGUMENT_REF;
	instructions[7] = GLOBAL_REF;
	instructions[8] = CHECKED_GLOBAL_REF;
	//
	instructions[10] = CONSTANT;
	instructions[11] = CONSTANT;
	instructions[12] = CONSTANT;
	instructions[13] = PREDEFINED;
	instructions[14] = PREDEFINED;
	instructions[15] = PREDEFINED;
	instructions[16] = PREDEFINED;
	instructions[17] = PREDEFINED;
	instructions[18] = PREDEFINED;
	instructions[19] = PREDEFINED;
	instructions[20] = FINISH;
	instructions[21] = SET_SHALLOW_ARGUMENT;
	instructions[22] = SET_SHALLOW_ARGUMENT;
	instructions[23] = SET_SHALLOW_ARGUMENT;
	instructions[24] = SET_SHALLOW_ARGUMENT;
	instructions[25] = SET_SHALLOW_ARGUMENT;
	instructions[26] = SET_DEEP_ARGUMENT;
	instructions[27] = SET_GLOBAL;
	instructions[28] = GOTO;
	instructions[29] = JUMP_FALSE;
	instructions[30] = GOTO;
	instructions[31] = JUMP_FALSE;
	instructions[32] = EXTEND_ENV;
	instructions[33] = UNLINK_ENV;
	instructions[34] = PUSH_VALUE;
	instructions[35] = POP_ARG1;
	instructions[36] = POP_ARG2;
	instructions[37] = PRESERVE_ENV;
	instructions[38] = RESTORE_ENV;
	instructions[39] = POP_FUNCTION;
	instructions[40] = CREATE_CLOSURE;
	//
	instructions[43] = RETURN;
	instructions[44] = PACK_FRAME;
	instructions[45] = FUNCTION_INVOKE;
	//
	instructions[47] = POP_CONS_FRAME;
	//
	//
	instructions[50] = ALLOCATE_FRAME;
	instructions[51] = ALLOCATE_FRAME;
	instructions[52] = ALLOCATE_FRAME;
	instructions[53] = ALLOCATE_FRAME;
	instructions[54] = ALLOCATE_FRAME;
	instructions[55] = ALLOCATE_FRAME;
	instructions[56] = ALLOCATE_DOTTED_FRAME;
	//
	instructions[60] = POP_FRAME;
	instructions[61] = POP_FRAME;
	instructions[62] = POP_FRAME;
	instructions[63] = POP_FRAME;
	instructions[64] = POP_FRAME;
	//
	instructions[71] = ARITYEQ;
	instructions[72] = ARITYEQ;
	instructions[73] = ARITYEQ;
	instructions[74] = ARITYEQ;
	instructions[75] = ARITYEQ;
	//
	instructions[79] = CONSTANT;
	instructions[80] = CONSTANT;
	instructions[81] = CONSTANT;
	instructions[82] = CONSTANT;
	instructions[83] = CONSTANT;
	instructions[84] = CONSTANT;
	//
	instructions[89] = INVOKE0;
	instructions[89] = INVOKE0;
	instructions[90] = INVOKE1;
	instructions[91] = INVOKE1;
	instructions[92] = INVOKE1;
	instructions[93] = INVOKE1;
	instructions[94] = INVOKE1;
	instructions[95] = INVOKE1;
	instructions[96] = INVOKE1;
	instructions[97] = INVOKE1;
	instructions[98] = INVOKE1;
	//
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
run(struct vm *vm, sexp code, sexp global) {
	int offset;
	int op;
	int i;
	char *bytecode;

	bytecode = (char*)malloc(sexp_vector_length(code));	
	for (i = 0; i < sexp_vector_length(code); i++) {
		bytecode[i] = sexp_unbox_fixnum(sexp_vector_data(code)[i]);
	}
	vm->code = bytecode;
	vm->global = global;
	
	for ( ; ;) {
		op = bytecode[vm->pc];
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

sexp sexp_make_eval_context (sexp ctx, sexp stack, sexp env, sexp_uint_t size, sexp_uint_t max_size) {
	sexp res = sexp_make_context(ctx, size, max_size);
	return res;
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
	
	vm->ctx = sexp_make_context(NULL, 0, 0);
}

int
main(int argc, char *argv[]) {
	struct vm vm;
	int succ;
	sexp global;
	sexp bytecode;
	const char *str = "#(79 5 34 84 35 108 31 4 79 42 30 2 79 10 20)";
	
	global = sexp_make_vector(vm.ctx, SEXP_ONE, SEXP_ONE);
	initialize();
	vm_init(&vm);
	bytecode = sexp_read_from_string(vm.ctx, str, -1);
	succ = run(&vm, bytecode, global);
	if (succ != 0) {
		printf("失败了%d\n", succ);
	} else {
		printf("%ld", sexp_unbox_fixnum(vm.val));		
	}
	return 0;
}