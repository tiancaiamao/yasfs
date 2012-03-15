#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*--------------------------data struct---------------------*/
enum object_type
{
	OBJ_PAIR = 0,
	OBJ_NUMBER,
	OBJ_STRING,
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
	OBJ_TYPE_MAX
};

enum syntax_type
{
	SYNTAX_IF = 0,
	SYNTAX_BEGIN,
	SYNTAX_LAMBDA,
	SYNTAX_SET,
	SYNTAX_DEFINE
};

struct object_head
{
	enum object_type type;
	char marked;
};

typedef struct object_head* object_t;

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
struct error
{
	struct object_head head;
	char *info;
};

struct object_head scheme_null_object = {.type = OBJ_NULL};
struct object_head scheme_false_object = {.type = OBJ_FALSE};
struct object_head scheme_true_object = {.type = OBJ_TRUE};
struct object_head scheme_unspecified_object = {.type = OBJ_UNSPECIFIED};

object_t scheme_null = &scheme_null_object;
object_t scheme_false = &scheme_false_object;
object_t scheme_true = &scheme_true_object;
object_t scheme_unspecified = &scheme_unspecified_object;

struct typeinfo
{
	char *name;
	unsigned int size;
	object_t (*generator)();
};

struct typeinfo typeinfo_table[OBJ_TYPE_MAX];

#define type_name(o) (typeinfo_table[o->type].name)
#define type_size(o) (typeinfo_table[o->type].size)

struct env* make_env(struct env *p,struct pair *binding)
{
	struct env* ret = malloc(sizeof(struct env));
	ret->head.type = OBJ_ENV;
	ret->parent = p;
	ret->binding = binding;
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
struct symbol* make_symbol(char *data)
{
	unsigned int size = strlen(data);
	struct symbol *ret = malloc(sizeof(struct symbol) + size);
	ret->head.type = OBJ_SYMBOL;
	ret->size = size;
	strcpy(ret->data,data);
	return ret;
}
struct number* make_number(int n)
{
	struct number *ret=malloc(sizeof(struct number));
	ret->head.type = OBJ_NUMBER;
	ret->data.fixnum = n;
	return ret;
}
char* error_message[] = 
{
	"can't eval this type of obj",//0
	"can't apply this type of obj",//1
	"unbinded variable in env",//2
	"can't cdr on a non-pair",//3
};
object_t sys_error(unsigned int num)
{
	struct error *ret;
	ret = malloc(sizeof(struct error));
	ret->head.type = OBJ_ERROR;
	ret->info = error_message[num];
	return ret;
}
void init()
{
	typeinfo_table[OBJ_PAIR].name = "pair";
	typeinfo_table[OBJ_PAIR].size = sizeof(struct pair);
	typeinfo_table[OBJ_PAIR].generator = NULL;

	typeinfo_table[OBJ_ENV].name = "environment";
	typeinfo_table[OBJ_ENV].size = sizeof(struct env);
	typeinfo_table[OBJ_ENV].generator = NULL;
}

struct syntax syntax_table[] =
{
	{.head={OBJ_SYNTAX},.tag=SYNTAX_IF},
	{.head={OBJ_SYNTAX},.tag=SYNTAX_BEGIN},
	{.head={OBJ_SYNTAX},.tag=SYNTAX_LAMBDA},
	{.head={OBJ_SYNTAX},.tag=SYNTAX_SET},
	{.head={OBJ_SYNTAX},.tag=SYNTAX_DEFINE},
};
object_t eqv_op(object_t,object_t);
object_t assv_op(object_t,object_t);
object_t cons_op(object_t,object_t);

object_t cons_op(object_t car,object_t cdr)
{
	return make_pair(car,cdr);
}

object_t cdr_op(object_t p)
{
	if(p->type != OBJ_PAIR)
	{
		return sys_error(3);
	}
	return ((struct pair*)p)->cdr;
}
object_t pair_op(object_t o)
{
	if(o->type != OBJ_PAIR)
		return scheme_false;
	return scheme_true;

}
object_t car_op(object_t p)
{
	if(p->type != OBJ_PAIR)
	{
		return sys_error(3);
	}
	return ((struct pair*)p)->car;
}

object_t caar_op(object_t p)
{
	return car_op(car_op(p));
}
object_t cddr_op(object_t p)
{
	return cdr_op(cdr_op(p));
}
object_t cadr_op(object_t p)
{
	return car_op(cdr_op(p));
}

object_t set_cdr_op(object_t o,object_t value)
{
	if(o->type != OBJ_PAIR)
	{
		return sys_error(3);
	}
	((struct pair*)o)->cdr = value;
	return scheme_unspecified;
}
object_t eqv_op(object_t a,object_t b)
{
	if(a->type==OBJ_SYMBOL && b->type==OBJ_SYMBOL)
	{
		if(strcmp(((struct symbol*)a)->data,((struct symbol*)b)->data) == 0)
			return scheme_false;
		else
			return scheme_true;
	}
	return a == b ? scheme_true :scheme_false;
}

object_t assv_op(object_t obj,object_t list)
{
	object_t tmp;

	while(list->type == OBJ_PAIR)
	{
		tmp = car_op(list);
		if(tmp->type==OBJ_PAIR && eqv_op(obj,car_op(tmp))!=scheme_false)
			return tmp;
		list = ((struct pair*)list)->cdr;
	}
	return scheme_false;
}

object_t eval_syntax(struct syntax *op,object_t form,struct env *env);
object_t eval_begin();
object_t eval_lambda();
object_t eval_set();
object_t eval_define();
object_t eval_if(object_t form,struct env *env);
object_t eval_primitive();
object_t eval_procedure();
object_t eval_macro();


/*---------------------------environment-------------------*/
object_t lookup_env_cell(struct symbol *name,struct env *env)
{
	object_t find;

	while((object_t)env != scheme_null)
	{
		find = assv_op(name,env->binding);
		if(find == scheme_false)
			env = env->binding;
	}
	return sys_error(2);
}

object_t lookup_env(struct symbol *name,struct env *env)
{
	object_t env_cell = lookup_env_cell(name,env);
	if(env_cell->type != OBJ_ERROR)
		return cdr_op(env_cell);
	return env_cell;
}
object_t define_env(struct env *env,struct symbol *name,object_t value)
{
	object_t find = assv_op(name,env);
	if(find == scheme_false)
	{
		find = cons_op(name,value);
		env->binding = cons_op(find,env->binding);
	}
	else
		return set_cdr_op(find,value);
}
struct env* make_basic_environment()
{
	struct env *ret = make_env(scheme_null,scheme_null);
	define_env(ret,make_symbol("if"),&syntax_table[SYNTAX_IF]);
	return ret;
}

/*---------------------------eval--------------------------*/
object_t eval(object_t obj,struct env *env)
{
	object_t op;
	switch(obj->type)
	{
	case OBJ_SYMBOL:
		return lookup_env(obj,env);
	case OBJ_NUMBER:
	case OBJ_STRING:
	case OBJ_VECTOR:
	case OBJ_NULL:
	case OBJ_FALSE:
                return obj;
	case OBJ_PAIR:
		break;
	default:
		return sys_error(0);
	}
	op = eval(car_op(obj),env);
	switch(op->type)
	{
	case OBJ_SYNTAX:
		return eval_syntax((struct syntax*)op,cdr_op(obj),env);
	case OBJ_PRIMITIVE:
		return eval_primitive();
	case OBJ_PROCEDURE:
		return eval_procedure();
	case OBJ_MACRO:
		return eval_macro();
	default:
		return sys_error(1);
	}
}

object_t eval_syntax(struct syntax *op,object_t form,struct env *env)
{
	switch(op->head.type)
	{
	case SYNTAX_IF:
		return eval_if(form,env);
	case SYNTAX_BEGIN:
		return eval_begin();
	case SYNTAX_LAMBDA:
		return eval_lambda();
	case SYNTAX_SET:
		return eval_set();
	case SYNTAX_DEFINE:
		return eval_define();
	default:
		return error("unknown type of core syntax");
	}
}
object_t eval_begin()
{
	return scheme_null;
}
object_t eval_set()
{
	return scheme_null;
}
object_t eval_lambda()
{
	return scheme_null;
}
object_t eval_define()
{
	return scheme_null;
}

object_t eval_primitive()
{
	return scheme_null;
}

object_t eval_procedure()
{
	return scheme_null;
}
object_t eval_macro()
{
	return scheme_null;
}

object_t eval_if(object_t form,struct env *env)
{
	object_t test;
	object_t yes;
	object_t no;
	if(form->type != OBJ_PAIR)
		return error("bad if syntax");
	test = car_op(form);
	if(cdr_op(form)->type != OBJ_PAIR)
		return error("bad if syntax");
	yes = cadr_op(form);
	no = cddr_op(form);
	if(eval(test,env) != scheme_false)
	{
		return eval(yes,env);
	}
	else
	{
		if(pair_op(no))
			return eval(car_op(no),env);
		else
			return error("bad if syntax");
	}
}


int main()
{
	object_t ret;
	object_t form;
	struct env *env;

	init();
	env = make_basic_environment();
	form = make_number(5);
	ret = eval(form,env);
	printf("result: %d\n",((struct number*)form)->data.fixnum);
	return 0;
}
