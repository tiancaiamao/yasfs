#include <stdio.h>
#include <stdlib.h>
/*--------------------------data struct---------------------*/
enum object_type
{
	OBJ_PAIR = 0,
	OBJ_NUMBER,
	OBJ_STRING,
	OBJ_CHARACTER,
	OBJ_VECTOR,
	OBJ_NULL,
	OBJ_FALSE,
	OBJ_TRUE,
	OBJ_ENV,
	OBJ_SYMBOL,
	OBJ_SYNTAX,
	OBJ_PRIMITIVE,
	OBJ_PROCEDURE,
	OBJ_MACRO,
	OBJ_ERROR,
	OBJ_UNSPECIFIED,
	OBJ_UNINITED,
	OBJ_CONT,
	OBJ_CLOSURE,
	OBJ_TYPE_MAX
};

enum syntax_type
{
	SYNTAX_IF = 0,
	SYNTAX_BEGIN,
	SYNTAX_LAMBDA,
	SYNTAX_SET,
	SYNTAX_DEFINE,
	SYNTAX_QUOTE
};

struct object_head
{
	enum object_type type;
	char marked;
};

typedef struct object_head* object_t;

struct vm
{
	object_t env;
	object_t value;
	object_t func;
	object_t *ebp;
//	object_t *esp;
};
struct pair
{
	struct object_head head;
	object_t car;
	object_t cdr;
};
struct string
{
	struct object_head head;
	unsigned int size;
	char data[1];
};
struct symbol
{
	struct object_head head;
	unsigned int size;
	char data[1];
};
struct syntax
{
	struct object_head head;
	enum syntax_type tag;
	char *name;
};
struct env
{
	struct object_head head;
	struct env *parent;
	struct pair *binding;
};
struct number
{
	struct object_head head;
	union
	{
		int fixnum;
		double flonum;
	}data;
};
struct character
{
	struct object_head head;
	char data;
};
struct procedure
{
	struct object_head head;
	object_t variables;
	object_t body;
	struct env *env;
	struct cont *cont;
};
struct primitive
{
	struct object_head head;
	char *name;
	unsigned int arg_num;
	union
	{
		object_t (*op0)();
		object_t (*op1)(object_t);
		object_t (*op2)(object_t,object_t);
		object_t (*op3)(object_t,object_t,object_t);
		object_t (*op4)(object_t,object_t,object_t,object_t);
		object_t (*op5)(object_t,object_t,object_t,object_t,object_t);
		object_t (*op6)(object_t,object_t,object_t,object_t,object_t,object_t);
	}callback;
};
typedef void (*func_t)(struct vm*);
struct closure
{
	struct object_head head;
	func_t func;
	object_t env;
};
struct vector
{
	struct object_head head;
	unsigned int size;
	object_t data[0];
};
object_t make_vector(unsigned int size)
{
	struct vector *ret = malloc(sizeof(struct vector)+size*sizeof(object_t));
	ret->head.type = OBJ_VECTOR;
	ret->size = size;
	return ret;
}
void vector_set(object_t v,unsigned int idx,object_t o)
{
	((struct vector*)v)->data[idx] = o;
}
object_t vector_ref(object_t v,unsigned int idx)
{
	return ((struct vector*)v)->data[idx];
}
object_t closure_env(struct closure *c)
{
	return c->env;
}
func_t closure_func(struct closure *c)
{
	return c->func;
}
struct number* make_number(int n)
{
	struct number *ret=malloc(sizeof(struct number));
	ret->head.type = OBJ_NUMBER;
	ret->data.fixnum = n;
	return ret;
}
struct error
{
	struct object_head head;
	char *info;
};
struct closure* make_closure(func_t func,object_t env)
{
	struct closure *ret = malloc(sizeof(struct closure));
	ret->head.type = OBJ_CLOSURE;
	ret->func = func;
	ret->env = env;
	return ret;
}
struct pair* make_pair(object_t car,object_t cdr)
{
	struct pair* ret = malloc(sizeof(struct env));
	ret->head.type = OBJ_PAIR;
	ret->car = car;
	ret->cdr = cdr;
	return ret;
}

object_t cons(object_t car,object_t cdr)
{
	return make_pair(car,cdr);
}
object_t car(object_t o)
{
	return ((struct pair*)o)->car;
}

//struct vm *make_vm();
//struct vm *load_standard_environment(struct vm *);

#define PUSH(v) *(--vm->esp) = (v)
#define POP() *vm->esp++
#define SHALLOW_ARGUMENT_REF(n) vector_ref(car(vm->env),n)
#define CLOSURE_CALL() (closure_func(vm->func))(vm)

//#define PUSH(v) do{*vm->esp = v; ++vm->esp;}while(0)

struct pair scheme_null_obj;
object_t scheme_standard_environment = &scheme_null_obj;

//(lambda (a b) (cons a b))
/* static void funcXXX(struct vm *vm) */
/* { */
/* 	object_t save_esp; */
/* 	save_esp = vm->esp;//保存esp */
/* 	vm->esp = &save_esp;//新的esp */
/* 	PUSH(vm->ebp);//保存ebp */
/* 	vm->ebp = &save_esp;//新的ebp */
/* 	PUSH(vm->env);//保存环境 */
/* 	vm->env = cons(vm->value,closure_env(vm->func));//改变环境 */
/* //... */
/* 	vm->value = SHALLOW_ARGUMENT_REF(0); */
/* 	*(--vm->esp) = vm->value; */
/* //	PUSH(vm->value); */
/* 	vm->value = SHALLOW_ARGUMENT_REF(1); */
/* 	PUSH(vm->value); */
/* 	vm->value = cons(POP(),POP()); */

/* //... */
/* 	vm->env = POP();//恢复环境 */
/* 	vm->esp = vm->ebp[0];//恢复esp */
/* 	vm->ebp = vm->ebp[-1];//恢复ebp */
/* } */

#define ENTER_CLOSURE(size,name) \
	object_t stack[size];\
	unsigned int index = 4;\
	stack[0] = vm->ebp;\
	vm->ebp = stack;\
	stack[1] = size;\
	stack[2] = "name";\
	stack[3] = vm->env;\
	vm->env = cons(vm->value,closure_env(vm->func));
#define EXIT_CLOSURE()\
	vm->ebp = stack[0];\
	vm->env = stack[3];\

#define PUSH(v) stack[index++] = (v)
#define POP() stack[--index]
static void funcXXX(struct vm *vm)
{
	ENTER_CLOSURE(6,name);
//...
	vm->value = SHALLOW_ARGUMENT_REF(0);
	PUSH(vm->value);
	vm->value = SHALLOW_ARGUMENT_REF(1);
	PUSH(vm->value);
	vm->value = cons(POP(),POP());
//...
	vm->ebp = stack[0];//恢复ebp
	vm->env = stack[3];
}

int main()
{
	struct vm *vm = malloc(sizeof(struct vm));
	func_t fun_ptr;

//(f 1 2)
/* ... */
	/* reg_value = 1; */
	/* PUSH(reg_value); */
	/* reg_value = 2; */
	/* PUSH(reg_value); */
	/* reg_value = deep_argument_ref(3,0);//取到闭包f */
	/* reg_func = reg_value; */
	/* reg_value = make_vector(2);//构造环境的frame */
	/* vector_set(reg_value,0,POP()); */
	/* vector_set(reg_value,1,POP()); */
	/* CLOSURE_CALL();//闭包调用 */
	/* reg_env = POP();//恢复环境 */
	/* reg_stack_base = POP();//恢复栈寄存器 */
	/* reg_stack -= 2;  //消除C栈造成的影响 */

	vm->ebp = NULL;
	vm->value = make_vector(2);
	vector_set(vm->value,0,make_number(1));
	vector_set(vm->value,1,make_number(2));
	vm->env = scheme_standard_environment;
	vm->func = make_closure(funcXXX,scheme_standard_environment);
	CLOSURE_CALL();
	return 0;
}
