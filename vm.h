#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
	OBJ_CLOSURE,
	OBJ_MACRO,
	OBJ_ERROR,
	OBJ_UNSPECIFIED,
	OBJ_UNINITED,
	OBJ_CONT,
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
struct vector
{
	struct object_head head;
	unsigned int size;
	object_t data[0];
};
struct env
{
	struct object_head head;
	struct env *parent;
	struct vector *frame;
};
struct closure
{
	struct object_head head;
	struct env *env;
	object_t *code;
	object_t arity;
	object_t size;
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
struct error
{
	struct object_head head;
	char *info;
};

struct object_head scheme_null_object = {.type = OBJ_NULL};
struct object_head scheme_false_object = {.type = OBJ_FALSE};
struct object_head scheme_true_object = {.type = OBJ_TRUE};
struct object_head scheme_unspecified_object = {.type = OBJ_UNSPECIFIED};
struct object_head scheme_uninited_object = {.type = OBJ_UNINITED};

object_t scheme_null = &scheme_null_object;
object_t scheme_false = &scheme_false_object;
object_t scheme_true = &scheme_true_object;
object_t scheme_unspecified = &scheme_unspecified_object;
object_t scheme_uninited = &scheme_uninited_object;

struct typeinfo
{
	char *name;
	unsigned int size;
	object_t (*generator)();
};

struct typeinfo typeinfo_table[OBJ_TYPE_MAX];

#define type_name(o) (typeinfo_table[o->type].name)
#define type_size(o) (typeinfo_table[o->type].size)

struct env* make_env(struct env *p,struct vector *frame)
{
	struct env* ret = malloc(sizeof(struct env));
	ret->head.type = OBJ_ENV;
	ret->parent = p;
	ret->frame = frame;
	return ret;
}
#define closure_env(c) (((struct closure*)c)->env)
#define closure_code(c) (((struct closure*)c)->code)

struct vector* make_vector(unsigned int size)
{
	struct vector* ret = malloc(sizeof(struct vector)+size*sizeof(object_t));
	ret->head.type = OBJ_VECTOR;
	ret->size = size;
	return ret;
}
object_t vector_ref(struct vector *v,unsigned int idx)
{
	return v->data[idx];
}
object_t vector_set(struct vector *v,unsigned int idx,object_t o)
{
	v->data[idx] = o;
	return scheme_unspecified;
}

#define env_parent(e) (((struct env*)(e))->parent)
#define env_frame(e) (((struct env*)(e))->frame)
object_t make_pair(object_t car,object_t cdr)
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
struct string* make_string(char *data)
{
	unsigned int size = strlen(data);
	struct string *ret = malloc(sizeof(struct string) + size);
	ret->head.type = OBJ_STRING;
	ret->size = size;
	strcpy(ret->data,data);
	return ret;
}
struct character* make_character(char c)
{
	struct character *ret = malloc(sizeof(struct character));
	ret->head.type = OBJ_CHARACTER;
	ret->data = c;
	return ret;
}
struct closure* make_closure(object_t *code,object_t env,object_t arity,object_t size)
{
	struct closure *ret = malloc(sizeof(struct closure));
	ret->head.type = OBJ_CLOSURE;
	ret->env = env;
	ret->code = code;
	ret->arity = arity;
	ret->size = size;
	return ret;
}
struct number* make_number(int n)
{
	struct number *ret=malloc(sizeof(struct number));
	ret->head.type = OBJ_NUMBER;
	ret->data.fixnum = n;
	return ret;
}
int unbox(object_t o)
{
	return ((struct number*)o)->data.fixnum;
}
struct procedure* make_procedure(object_t variables,object_t body,struct env *env)
{
	struct procedure *ret = malloc(sizeof(struct procedure));
	ret->head.type = OBJ_PROCEDURE;
	ret->body = body;
	ret->variables = variables;
	ret->env = env;
	return ret;
}
char* error_message[] = 
{
	"can't eval this type of obj",//0
	"can't apply this type of obj",//1
	"unbinded variable in env",//2
	"can't cdr on a non-pair",//3
	"bad if syntax",//4
	"bad set syntax",//5
	"can't use non-symbol object as name in set syntax",//6
	"wrong begin syntax"//7,
	"bad lambda syntax",//8
	"eval_argument error",//9
	"not enough arguments",//10
	"too many arguments",//11
	"wrong number of arguments for primitive",//12
	"exceed maxium of length in list-ref ",//13
	"can't use add on non-number",//14
	"can't reverse non-list",//15
	"call/cc must receive a procedure",//16
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

object_t eqv_op(object_t,object_t);
object_t assv_op(object_t,object_t);
object_t cons_op(object_t,object_t);

object_t cons_op(object_t car,object_t cdr)
{
	return make_pair(car,cdr);
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
object_t cdr_op(object_t p)
{
	if(p->type != OBJ_PAIR)
	{
		return sys_error(3);
	}
	return ((struct pair*)p)->cdr;
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
object_t caddr_op(object_t p)
{
	return car_op(cddr_op(p));
}
object_t cdddr_op(object_t p)
{
	return cdr_op(cddr_op(p));
}
object_t cadddr_op(object_t p)
{
	return car_op(cdddr_op(p));
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
			return scheme_true;
		else
			return scheme_false;
	}
	else if(a->type==OBJ_NUMBER && b->type==OBJ_NUMBER)
	{
		return ((struct number*)a)->data.fixnum==((struct number*)b)->data.fixnum ? scheme_true:scheme_false;
	}
	return a == b ? scheme_true :scheme_false;
}
object_t eq_op(object_t a,object_t b)
{
	if(a->type==OBJ_NUMBER && b->type==OBJ_NUMBER)
	{
		return ((struct number*)a)->data.fixnum==((struct number*)b)->data.fixnum ? scheme_true:scheme_false;
	}
	return a==b ? scheme_true:scheme_false;
}
object_t add_op(object_t a,object_t b)
{
	if(a->type!=OBJ_NUMBER || b->type!=OBJ_NUMBER)
		return sys_error(14);
	return make_number(((struct number*)a)->data.fixnum + ((struct number*)b)->data.fixnum);
}
object_t sub_op(object_t a,object_t b)
{
	if(a->type!=OBJ_NUMBER || b->type!=OBJ_NUMBER)
		return sys_error(14);
	return make_number(((struct number*)a)->data.fixnum - ((struct number*)b)->data.fixnum);
}
object_t mul_op(object_t a,object_t b)
{
	if(a->type!=OBJ_NUMBER || b->type!=OBJ_NUMBER)
		return sys_error(14);
	return make_number(((struct number*)a)->data.fixnum * ((struct number*)b)->data.fixnum);
}
object_t div_op(object_t a,object_t b)
{
	if(a->type!=OBJ_NUMBER || b->type!=OBJ_NUMBER)
		return sys_error(14);
	return make_number(((struct number*)a)->data.fixnum / ((struct number*)b)->data.fixnum);
}
unsigned int length(object_t lst)
{
	unsigned int i = 0;
	while(lst->type == OBJ_PAIR)
	{
		lst = ((struct pair*)lst)->cdr;
		i++;
	}
	return i;
}
object_t length_op(object_t lst)
{
	int i = length(lst);
	return make_number(i);
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
object_t reverse_op(object_t lst)
{
	object_t ret = scheme_null;
	while(lst != scheme_null)
	{
		ret = cons_op(car_op(lst),ret);
		lst = cdr_op(lst);
	}
	return ret;
}
object_t list_ref(object_t lst,unsigned int i)
{
	object_t ret;
	while(i>0 && lst->type==OBJ_PAIR)
	{
		ret = lst;
		lst = ((struct pair*)lst)->cdr;
		i--;
	}
	if(lst->type!=OBJ_PAIR)
		return sys_error(13);
	if(lst->type==OBJ_PAIR && i==0)
		return ((struct pair*)lst)->car;
}
