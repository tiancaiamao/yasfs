enum instruction_type
{
	SHALLOW_ARGUMENT_REF = 0,
	INSTRUCTION_TYPE_MAX;
};

struct instruction
{
	enum instruction_type code;
	unsigned size;

}instruction_set[INSTRUCTION_TYPE_MAX];

struct vm
{
	object_t env;
	object_t pc;
	object_t stack;
	object_t value;
	int stack_index;
};


case SHALLOW_ARGUMENT_REF:
再读j
取*env*的第j个值放到*var*
