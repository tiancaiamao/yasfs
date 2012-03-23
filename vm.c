#include "vm.h"
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
	INVOKE,
	CONSTANT_TRUE,
	CONSTANT_FALSE,
	CONSTANT_NULL,
	CONSTANT_UNSPECIFIED,
	FINISH
};

struct primitive primitive_table[] = 
{
	{.head={OBJ_PRIMITIVE},.name="cons",.arg_num=2,.callback=cons_op},
	{.head={OBJ_PRIMITIVE},.name="car",.arg_num=1,.callback=car_op},
	{.head={OBJ_PRIMITIVE},.name="cdr",.arg_num=1,.callback=cdr_op},
	{.head={OBJ_PRIMITIVE},.name="assv",.arg_num=2,.callback=assv_op},
	{.head={OBJ_PRIMITIVE},.name="eqv?",.arg_num=2,.callback=eqv_op},
	{.head={OBJ_PRIMITIVE},.name="set-cdr!",.arg_num=2,.callback=set_cdr_op},
	{.head={OBJ_PRIMITIVE},.name="eq?",.arg_num=2,.callback=eq_op},
	{.head={OBJ_PRIMITIVE},.name="+",.arg_num=2,.callback=add_op},
	{.head={OBJ_PRIMITIVE},.name="-",.arg_num=2,.callback=sub_op},
	{.head={OBJ_PRIMITIVE},.name="*",.arg_num=2,.callback=mul_op},
	{.head={OBJ_PRIMITIVE},.name="/",.arg_num=2,.callback=div_op},
};

struct instruction
{
	enum bytecode code;
	unsigned size;

}instruction_set[FINISH];

struct vm
{
	object_t env;
	object_t *pc;
	object_t stack;
	unsigned int stack_index;
	unsigned int stack_base;
	object_t value;
	object_t func;
	object_t arg1;
	object_t arg2;
	object_t arg3;
	/* object_t constant; */
	/* object_t global; */
	/* object_t code; */
	unsigned int stack_size;
};
void wirte(FILE*,object_t);
object_t read(FILE *in);

char is_delimiter(int c) {
	return isspace(c) || c == EOF ||
		c == '('   || c == ')' ||
		c == '"'   || c == ';';
}

char is_initial(int c) {
	return isalpha(c) || c == '*' || c == '/' || c == '>' ||
		c == '<' || c == '=' || c == '?' || c == '!';
}

void eat_whitespace(FILE *in) 
{
	int c;
    
	while ((c = getc(in)) != EOF) 
	{
		if (isspace(c)) 
			continue;
		else if (c == ';') 
		{ /* comments are whitespace also */
			while (((c = getc(in)) != EOF) && (c != '\n'));
			continue;
		}
		ungetc(c, in);
		break;
	}
}
int peek(FILE *in)
{
	int c;
	c = getc(in);
	ungetc(c,in);
	return c;
}

void eat_expected_string(FILE *in, char *str) 
{
	int c;

	while (*str != '\0') {
		c = getc(in);
		if (c != *str) {
			fprintf(stderr, "unexpected character '%c'\n", c);
			exit(1);
		}
		str++;
	}
}

void peek_expected_delimiter(FILE *in) 
{
	if (!is_delimiter(peek(in))) 
	{
		fprintf(stderr, "character not followed by delimiter\n");
		exit(1);
	}
}
object_t read_character(FILE *in)
{
	int c;

	c = getc(in);
	switch (c) 
	{
        case EOF:
		fprintf(stderr, "incomplete character literal\n");
		exit(1);
        case 's':
		if (peek(in) == 'p') 
		{
			eat_expected_string(in, "pace");
			peek_expected_delimiter(in);
			return make_character(' ');
		}
		break;
        case 'n':
		if (peek(in) == 'e') 
		{
			eat_expected_string(in, "ewline");
			peek_expected_delimiter(in);
			return make_character('\n');
		}
		break;
	}
	peek_expected_delimiter(in);
	return make_character(c);
}
object_t read_pair(FILE *in) 
{
	int c;
	object_t car_obj;
	object_t cdr_obj;
	object_t ret;
    
	eat_whitespace(in);
    
	c = getc(in);
	if (c == ')') 
	{ /* read the empty list */
		return scheme_null;
	}
	ungetc(c, in);

	car_obj = read(in);
	

	eat_whitespace(in);
    
	c = getc(in);    
	if (c == '.') { /* read improper list */
		c = peek(in);
		if (!is_delimiter(c)) 
		{
			fprintf(stderr, "dot not followed by delimiter\n");
			exit(1);
		}
		cdr_obj = read(in);
		
		eat_whitespace(in);
		c = getc(in);
		if (c != ')') 
		{
			fprintf(stderr,
				"where was the trailing right paren?\n");
			exit(1);
		}
		ret = make_pair(car_obj,cdr_obj);
		return ret;
	}
	else 
	{ /* read list */
		ungetc(c, in);
		cdr_obj = read_pair(in);
		ret = make_pair(car_obj,cdr_obj);
		return ret;
	}
}
object_t read_vector(FILE *in)
{
	object_t lst;
	unsigned int len;
	object_t ret;
	int i;

	lst = read_pair(in);
	len = length(lst);
	ret = make_vector(len);
	for(i=0; i<len; i++)
	{
		vector_set(ret,i,car_op(lst));
		lst = cdr_op(lst);
	}
	return ret;
}
object_t read(FILE *in)
{
	int c;
	short sign = 1;
	int i;
	long num = 0;
#define BUFFER_MAX 300
	char buffer[BUFFER_MAX];

	eat_whitespace(in);
	c = getc(in);    
	if (c == '#') 
	{ /* read a boolean or character */
		c = getc(in);
		switch (c) 
		{
		case 't':
			return scheme_true;
		case 'f':
			return scheme_false;
		case '\\':
			return read_character(in);
		case '(':
			return read_vector(in);
		default:
			fprintf(stderr,
				"unknown boolean or character literal\n");
			exit(1);
		}
	}
	else if (isdigit(c) || (c == '-' && (isdigit(peek(in))))) 
	{
		/* read a fixnum */
		if (c == '-') {
			sign = -1;
		}
		else {
			ungetc(c, in);
		}
		while (isdigit(c = getc(in))) {
			num = (num * 10) + (c - '0');
		}
		num *= sign;
		if (is_delimiter(c)) 
		{
			ungetc(c, in);
			return make_number(num);
		}
		else 
		{
			fprintf(stderr, "number not followed by delimiter\n");
			exit(1);
		}
	}
	else if (is_initial(c) ||
		 ((c == '+' || c == '-') &&
		  is_delimiter(peek(in)))) { /* read a symbol */
		i = 0;
		while (is_initial(c) || isdigit(c) ||
		       c == '+' || c == '-') {
			/* subtract 1 to save space for '\0' terminator */
			if (i < BUFFER_MAX - 1) {
				buffer[i++] = c;
			}
			else {
				fprintf(stderr, "symbol too long. "
					"Maximum length is %d\n", BUFFER_MAX);
				exit(1);
			}
			c = getc(in);
		}
		if (is_delimiter(c)) {
			buffer[i] = '\0';
			ungetc(c, in);
			return make_symbol(buffer);
		}
		else {
			fprintf(stderr, "symbol not followed by delimiter. "
				"Found '%c'\n", c);
			exit(1);
		}
	}
	else if (c == '"') { /* read a string */
		i = 0;
		while ((c = getc(in)) != '"') {
			if (c == '\\') {
				c = getc(in);
				if (c == 'n') {
					c = '\n';
				}
			}
			if (c == EOF) {
				fprintf(stderr, "non-terminated string literal\n");
				exit(1);
			}
			/* subtract 1 to save space for '\0' terminator */
			if (i < BUFFER_MAX - 1) {
				buffer[i++] = c;
			}
			else {
				fprintf(stderr, 
					"string too long. Maximum length is %d\n",
					BUFFER_MAX);
				exit(1);
			}
		}
		buffer[i] = '\0';
		return make_string(buffer);
	}
	else if (c == '(') { /* read the empty list or pair */
		return read_pair(in);
	}
	else if (c == '\'') 
	{ /* read quoted expression */
		object_t tmp1,tmp2,quote_symbol,ret;
		tmp1 = read(in);
		tmp2 = cons_op(tmp1,scheme_null);
		quote_symbol = make_symbol("quote");

		ret = cons_op(quote_symbol,tmp2);
		return ret;
	}
	else if (c == EOF) 
		return NULL;
	else 
	{
		fprintf(stderr, "bad input. Unexpected '%c'\n", c);
		exit(1);
	}
	fprintf(stderr, "read illegal state\n");
	exit(1);
}

void write_pair(FILE *fp,struct pair* o)
{
	write(fp,o->car);
	if(o->cdr == scheme_null)
		fprintf(fp,")");
	else if(o->cdr->type == OBJ_PAIR)
	{
		fprintf(fp," ");
		write_pair(fp,o->cdr);
	}
	else
	{
		fprintf(fp," . ");
		write(fp,o->cdr);
		fprintf(fp,")");
	}
}
void write(FILE *fp,object_t o)
{
	switch(o->type)
	{
	case OBJ_NUMBER:
		fprintf(fp,"%d",((struct number*)o)->data.fixnum);
		break;
	case OBJ_PRIMITIVE:
		fprintf(fp,"#<primitive %s>",((struct primitive*)o)->name);
		break;
	case OBJ_PROCEDURE:
		fprintf(fp,"#<procedure>");
		break;
	case OBJ_CONT:
		fprintf(fp,"#<continuation>");
		break;
	case OBJ_STRING:
		fprintf(fp,"\"%s\"",((struct string*)o)->data);
		break;
	case OBJ_SYMBOL:
		fprintf(fp,"%s",((struct symbol*)o)->data);
		break;
	case OBJ_ERROR:
		fprintf(fp,"ERROR: %s",((struct error*)o)->info);
		break;
	case OBJ_UNSPECIFIED:
		break;
	case OBJ_PAIR:
		fprintf(fp,"(");
		write_pair(fp,o);
		break;
	case OBJ_SYNTAX:
		fprintf(fp,"#<syntax>");
		break;
	default:
		fprintf(fp,"ERROR: can't print this kind of object");
	}
}

int load_vm_from_file(const char *filename,object_t *code,object_t *constant,object_t *global)
{
	FILE *in;
	object_t global_size;

	in = fopen(filename,"r");
	if(!in)
		return -1;
	*constant = read(in);
	global_size = read(in);
	*global = make_vector(unbox(global_size));
	*code = read(in);
	return 0;
}
struct vm* create_vm(unsigned int stack_size)
{
	struct vm *ret = malloc(sizeof(struct vm));
	if(!ret)
		return NULL;
	ret->stack_size = stack_size;
	ret->stack = make_vector(stack_size);
	ret->stack_index = 0;
	ret->env = scheme_null;
	return ret;
}
object_t vm_eval(struct vm *vm,object_t code,object_t constant,object_t global)
{
	int bytecode;
	object_t finish = make_number(FINISH);

	vm->pc = ((struct vector*)code)->data;
	vector_set(vm->stack,vm->stack_index,scheme_null);
	++(vm->stack_index);
	vector_set(vm->stack,vm->stack_index,&finish);
	++(vm->stack_index);
	vector_set(vm->stack,vm->stack_index,make_number(0));
	++(vm->stack_index);
	vm->stack_base = 2;
	
	bytecode = unbox(*(vm->pc));
	while(bytecode != FINISH)
	{
		++(vm->pc);
		switch(bytecode)
		{
		case CONSTANT_TRUE:
			vm->value = scheme_true;
			break;
		case CONSTANT_FALSE:
			vm->value = scheme_false;
			break;
		case CONSTANT_NULL:
			vm->value = scheme_null;
			break;
		case CONSTANT_UNSPECIFIED:
			vm->value = scheme_unspecified;
			break;
		case CONSTANT_NUM:
			vm->value = *(vm->pc);
			++(vm->pc);
			break;
		case ALLOCATE_FRAME:
			if(unbox(*(vm->pc)) != unbox(vm->func->arity))
				goto error;
			else
				vm->value = make_vector(unbox(vm->func->size));
			++(vm->pc);
			break;
		case SET_FRAME_ARGUMENT:
			--(vm->stack_index);
			vector_set(vm->value,unbox(*(vm->pc)),vector_ref(vm->stack,vm->stack_index));
			++(vm->pc);
			break;
		case GOTO:
			vm->pc += (1+unbox(*(vm->pc)));
			break;
		case ARG1:
			vm->arg1 = vm->value;
			break;
		case ARG2:
			vm->arg2 = vm->value;
			break;
		case PRIMITIVE_CALL2:
			vm->value = (primitive_table[unbox(*(vm->pc))].callback.op2)(vm->arg1,vm->arg2);
//			vm->value = cons_op(vm->arg1,vm->arg2);
			++(vm->pc);
			break;
		case STACK_PUSH:
			vector_set(vm->stack,vm->stack_index,vm->value);
			vm->stack_index++;
			break;
		case VAL_FUN:
			vm->func = vm->value;
			break;
		case CREATE_CLOSURE:
			vm->value = make_closure(vm->pc+4,vm->env,(vm->pc)[0],(vm->pc)[1]);
			break;
		case CHANGE_ENV:
			vm->value = make_env(closure_env(vm->func),vm->value);
			vm->env = vm->value;
			break;
		case ENV_STACK:
			vector_set(vm->stack,vm->stack_index,vm->env);
			++(vm->stack_index);
			break;
		case STACK_ENV:
			vm->env = vector_ref(vm->stack,vm->stack_index);
			--(vm->stack_index);
			break;
		case TAIL_INVOKE:
			vm->pc = closure_code(vm->func);
			vm->stack_index = vm->stack_base;
			break;
		case INVOKE:
			vector_set(vm->stack,vm->stack_index,vm->pc);
			++(vm->stack_index);
			vector_set(vm->stack,vm->stack_index,make_number(vm->stack_base));
			vm->stack_base = vm->stack_index;
			++(vm->stack_index);
			vm->pc = closure_code(vm->func);
			break;
		case GLOBAL_SET:
			vector_set(global,unbox(*(vm->pc)),vm->value);
			++(vm->pc);
			break;
		case SHALLOW_ARGUMENT_REF:
			vm->value = vector_ref(env_frame(vm->env),unbox(*(vm->pc)));
			++(vm->pc);
			break;
		case CHECKED_SHALLOW_ARGUMENT_REF:
			vm->value = vector_ref(env_frame(vm->env),unbox(*(vm->pc)));
			if(vm->value == scheme_unspecified)
				goto error;
			++(vm->pc);
			break;
		case GLOBAL_REF:
			vm->value = vector_ref(global,unbox(*(vm->pc)));
			++(vm->pc);
			break;
		case RETURN:
			vm->stack_base = unbox(vector_ref(vm->stack,vm->stack_index));
			--(vm->stack_index);
			vm->pc = vector_ref(vm->stack,vm->stack_index);
			--(vm->stack_index);
			break;
		default:
			fprintf(stderr,"unknown type of bytecode: %d\n",bytecode);
			goto error;
		}
		bytecode = unbox(*(vm->pc));
	}
	return vm->value;
error:
	return sys_error(0);
}

int main()
{
	object_t constant;
	object_t global;
	object_t code;
	struct vm *vm;
	object_t res;

	if(load_vm_from_file("test.scm.so",&code,&constant,&global) != 0)
		exit(-1);
	vm = create_vm(200);
	if(!vm)
		exit(-1);
	res = vm_eval(vm,code,constant,global);
	return 0;
}
