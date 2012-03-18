enum bytecode
{
	CREATE_CLOSURE = 0,
	GOTO,
	CHANGE_ENV,
	SHALLOW_ARGUMENT_REF,
	ARG1,
	ARG2,
	PRIMITIVE_CALL2,
	RETURN,
	GLOBAL_SET,
	GLOBAL_REF,
	VAL_FUN,
	CONSTANT_NUM,
	STACK_PUSH,
	ALLOCATE_FRAME,
	SET_FRAME_ARGUMENT,
	ENV_STACK,
	TAIL_INVOKE,
	STACK_ENV,
	INSTRUCTION_TYPE_MAX
};

struct instruction
{
	enum bytecode code;
	unsigned size;

}instruction_set[INSTRUCTION_TYPE_MAX];

struct vm
{
	object_t env;
	object_t pc;
	object_t stack;
	unsigned int stack_index;
	object_t value;
	object_t constant;
	object_t global;
	object_t code;
};

int load_vm_from_file(const char *filename,struct vm *ret,unsigned int stack_size)
{
	FILE *in;
	object_t global_size;

	in = fopen(filename,"r");
	if(!in)
		return -1;
	ret->constant = read(in);
	global_size = read(in);
	ret->global = make_vector(global_size);
	ret->code = read(in);

	ret->env = scheme_null;
	ret->pc = ret->code->data[0];
	ret->stack = make_vector(stack_size);;
	ret->stack_index = 0;
	return 0;
}

object_t eval(struct *vm)
{
	while(*pc++ != FINISH)
	{
		switch(*pc)
		{
		case SHALLOW_ARGUMENT_REF:
			value = env[*pc];
			++pc;
			break;
		case CONSTANT_BOOL_TRUE:
			value = scheme_true;
			break;
		case CONSTANT_BOOL_FALSE:
			value = scheme_false;
			break;
		case CONSTANT_NULL:
			value = scheme_null;
			break;
case 
		defalut:
			sprintf(stderr,"unknown type of bytecode");
		}
	}
}
