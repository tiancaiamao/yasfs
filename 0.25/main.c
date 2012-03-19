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
struct error
{
	struct object_head head;
	char *info;
};
struct cont
{
	struct object_head head;
	struct cont *prev;
	object_t (*resume)(struct cont *this,object_t);
	union
	{
		struct 
		{
			object_t yes;
			object_t no;
			struct env *env;
		}if_cont;
		struct 
		{
			struct pair *env_cell;
		}set_cont;
		struct
		{
			struct env *env;
			object_t form;
		}begin_cont;
		struct
		{
			struct env *env;
			object_t form;
			object_t value;
		}argument_cont;
		struct
		{
			struct procedure *f;
		}apply_cont;
		struct
		{
			struct primitive *f;
		}primitive_cont;
	}data;
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

object_t resume_cont(struct cont *cont,object_t o)
{
	return o;
}
struct cont null_cont_object = 
{
	.head.type = OBJ_CONT,
	.prev = &scheme_null_object,
	.resume = resume_cont
};
struct cont *null_cont = &null_cont_object;
/*
struct cont* make_cont()
{
	struct cont *ret;
	ret = malloc(sizeof(struct cont));
	ret->head.type = OBJ_CONT;
	ret->prev = scheme_null;
	ret->resume = resume_cont;
	return ret;
}
*/
object_t resume_if_cont(struct cont*,object_t);
struct cont* make_if_cont(struct cont* prev,object_t yes,object_t no,struct env *env)
{
	struct cont* ret;
	ret = malloc(sizeof(struct cont));
	ret->head.type = OBJ_CONT;
	ret->prev = prev;
	ret->resume = resume_if_cont;
	ret->data.if_cont.yes = yes;
	ret->data.if_cont.no = no;
	ret->data.if_cont.env = env;
	return ret;
}

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
struct number* make_number(int n)
{
	struct number *ret=malloc(sizeof(struct number));
	ret->head.type = OBJ_NUMBER;
	ret->data.fixnum = n;
	return ret;
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
object_t apply(struct procedure *f,object_t values);
object_t call_with_current_continuation_op(object_t cont,object_t procedure)
{
	if(cont->type != OBJ_CONT || procedure->type != OBJ_PROCEDURE)
		return sys_error(16);
	apply(procedure, cons_op(cont,scheme_null));
}

object_t eval_syntax(struct syntax *op,object_t form,struct env *env,struct cont *cont);
object_t eval_quote(object_t form,struct cont *cont);
object_t eval_begin();
object_t eval_lambda(object_t form,struct env *env,struct cont *cont);
object_t eval_set(object_t form,struct env *env,struct cont *cont);
object_t eval_define();
object_t eval_if(object_t form,struct env *env,struct cont *cont);
object_t eval_primitive(struct primitive *f,object_t form,struct env *env,struct cont *cont);
object_t eval_procedure(struct procedure *f,object_t form,struct env *env,struct cont *cont);
object_t eval_macro();


/*---------------------------environment-------------------*/
object_t lookup_env_cell(struct symbol *name,struct env *env)
{
	object_t find;

	while((object_t)env != scheme_null)
	{
		find = assv_op(name,env->binding);
		if(find != scheme_false)
			return find;
		else
			env = env->parent;
	}
	return scheme_false;
}

object_t lookup_env(struct symbol *name,struct env *env)
{
	object_t env_cell = lookup_env_cell(name,env);
	if(env_cell != scheme_false)
		return cdr_op(env_cell);
	return sys_error(2);
}
object_t define_env(struct env *env,struct symbol *name,object_t value)
{
	object_t find = assv_op(name,env);
	if(find == scheme_false)
	{
		find = cons_op(name,value);
		env->binding = cons_op(find,env->binding);
		return scheme_unspecified;
	}
	else
		return set_cdr_op(find,value);
}
struct env* extend_env(struct evn *env,object_t names,object_t values)
{
	struct pair *binding = scheme_null;
	struct pair *env_cell;

	while(names->type == OBJ_PAIR && values->type == OBJ_PAIR)
	{
		env_cell = cons_op(((struct pair*)names)->car,((struct pair*)values)->car);
		binding = cons_op(env_cell,binding);
		names = ((struct pair*)names)->cdr;
		values = ((struct pair*)values)->cdr;
	}
	if(names->type == OBJ_PAIR && values->type != OBJ_PAIR)
		return sys_error(10);
	if(names == scheme_null && values->type == OBJ_PAIR)
		return sys_error(11);
	if(names->type == OBJ_SYMBOL)
		binding = cons_op(cons_op(names,values),binding);
	return make_env(env,binding);
}

struct syntax syntax_table[] =
{
	{.head={OBJ_SYNTAX},.tag=SYNTAX_IF,.name="if"},
	{.head={OBJ_SYNTAX},.tag=SYNTAX_BEGIN,.name="begin"},
	{.head={OBJ_SYNTAX},.tag=SYNTAX_LAMBDA,.name="lambda"},
	{.head={OBJ_SYNTAX},.tag=SYNTAX_SET,.name="set!"},
	{.head={OBJ_SYNTAX},.tag=SYNTAX_DEFINE,.name="define"},
	{.head={OBJ_SYNTAX},.tag=SYNTAX_QUOTE,.name="quote"},
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
	{.head={OBJ_PRIMITIVE},.name="call-with-current-continuation",.arg_num=2,.callback=call_with_current_continuation_op},
	{.head={OBJ_PRIMITIVE},.name="call/cc",.arg_num=2,.callback=call_with_current_continuation_op},
};
struct env* make_basic_environment()
{
	int i;
	struct env *ret = make_env(scheme_null,scheme_null);

	for(i=0; i<sizeof(syntax_table)/sizeof(struct syntax); i++)
	{
		define_env(ret,make_symbol(syntax_table[i].name),&syntax_table[i]);
	}
	for(i=0; i<sizeof(primitive_table)/sizeof(struct primitive); i++)
	{
		define_env(ret,make_symbol(primitive_table[i].name),&primitive_table[i]);
	}
	return ret;
}

/*---------------------------eval--------------------------*/
object_t eval(object_t obj,struct env *env,struct cont *cont)
{
	object_t op;
	switch(obj->type)
	{
	case OBJ_SYMBOL:
		return cont->resume(cont,lookup_env(obj,env));
	case OBJ_NUMBER:
	case OBJ_STRING:
	case OBJ_VECTOR:
	case OBJ_NULL:
	case OBJ_FALSE:
	case OBJ_CONT:
                return cont->resume(cont,obj);
	case OBJ_PAIR:
		break;
	default:
		return sys_error(0);
	}
	op = eval(car_op(obj),env,null_cont);
	switch(op->type)
	{
	case OBJ_SYNTAX:
		return eval_syntax((struct syntax*)op,cdr_op(obj),env,cont);
	case OBJ_PRIMITIVE:
		return eval_primitive(op,cdr_op(obj),env,cont);
	case OBJ_PROCEDURE:
		return eval_procedure(op,cdr_op(obj),env,cont);
	case OBJ_CONT:
		return ((struct cont*)op)->resume(op,eval(cadr_op(obj),env,null_cont));
	case OBJ_MACRO:
		return eval_macro();
	default:
		return sys_error(1);
	}
}

object_t eval_syntax(struct syntax *op,object_t form,struct env *env,struct cont *cont)
{
	switch(op->tag)
	{
	case SYNTAX_IF:
		return eval_if(form,env,cont);
	case SYNTAX_BEGIN:
		return eval_begin(form,env,cont);
	case SYNTAX_LAMBDA:
		return eval_lambda(form,env,cont);
	case SYNTAX_SET:
		return eval_set(form,env,cont);
	case SYNTAX_DEFINE:
		return eval_define(form,env);
	case SYNTAX_QUOTE:
		return eval_quote(form,cont);
	default:
		return error("unknown type of core syntax");
	}
}
object_t eval_quote(object_t form,struct cont *cont)
{
	return cont->resume(cont,car_op(form));
}
object_t eval_if(object_t form,struct env *env,struct cont *cont)
{
	object_t test;
	object_t yes;
	object_t no;
	if(form->type != OBJ_PAIR)
		return sys_error(4);
	test = car_op(form);
	if(cdr_op(form)->type != OBJ_PAIR)
		return sys_error(4);
	yes = cadr_op(form);
	no = cddr_op(form);
	return eval(test,env,make_if_cont(cont,yes,no,env));
}
object_t resume_if_cont(struct cont *this,object_t v)
{
	object_t yes = this->data.if_cont.yes;
	object_t no = this->data.if_cont.no;
	struct env *env = this->data.if_cont.env;
	struct cont *prev = this->prev;

	if(v != scheme_false)
		return eval(yes,env,prev);
	else
	{
		if(no == scheme_null)
			return scheme_unspecified;
		else if(pair_op(no) && cdr_op(no)==scheme_null)
			return eval(car_op(no),env,prev);
		else
			return sys_error(4);
	}
}
object_t resume_set_cont(struct cont *cont,object_t value)
{
	struct pair *env_cell;
	struct cont *prev;

	prev = cont->prev;
	env_cell = cont->data.set_cont.env_cell;
	if(value->type == OBJ_ERROR)
		return value;
	return prev->resume(prev,set_cdr_op(env_cell,value));
}
struct cont *make_set_cont(struct cont *prev,struct pair *env_cell)
{
	struct cont* ret;
	ret = malloc(sizeof(struct cont));
	ret->head.type = OBJ_CONT;
	ret->prev = prev;
	ret->resume = resume_set_cont;
	ret->data.set_cont.env_cell = env_cell;
	return ret;
}
object_t eval_set(object_t form,struct env *env,struct cont *cont)
{
	struct symbol *name;
	object_t value;
	object_t env_cell;

	if(form->type != OBJ_PAIR)
		return sys_error(5);
	if(((struct pair*)form)->car->type != OBJ_SYMBOL)
		return sys_error(5);
	name = ((struct pair*)form)->car;
	env_cell = lookup_env_cell(name,env);
	if(env_cell == scheme_false)
		return sys_error(2);
	return eval(cadr_op(form),env,make_set_cont(cont,env_cell));
}
object_t resume_begin_cont(struct cont *cont,object_t v)
{
	object_t form = cont->data.begin_cont.form;
	struct env *env = cont->data.begin_cont.env;

	if(form->type == OBJ_PAIR)
		return eval_begin(form,env,cont->prev);
	else
		return sys_error(7);
}
struct cont *make_begin_cont(struct cont *prev,object_t form,struct env *env)
{
	struct cont *ret = malloc(sizeof(struct cont));
	ret->head.type = OBJ_CONT;
	ret->prev = prev;
	ret->resume = resume_begin_cont;
	ret->data.begin_cont.form = form;
	ret->data.begin_cont.env = env;
	return ret;
}
object_t eval_begin(object_t form,struct env *env,struct cont *cont)
{
	if(form == scheme_null)
		return cont->resume(cont,scheme_unspecified);
	else if(form->type == OBJ_PAIR)
	{
		if(((struct pair*)form)->cdr == scheme_null)
			return eval(((struct pair*)form)->car,env,cont);
		else
			return eval(((struct pair*)form)->car,env,make_begin_cont(cont,((struct pair*)form)->cdr,env));
	}
	else 
		return sys_error(7);
}
object_t eval_lambda(object_t form,struct env *env,struct cont *cont)
{
	object_t variables;
	object_t body;

	if(form->type != OBJ_PAIR)
		return sys_error(8);
	variables = ((struct pair*)form)->car;
	body = ((struct pair*)form)->cdr;
	return cont->resume(cont,make_procedure(variables,body,env));
}
object_t eval_define(object_t form,struct env *env)
{
	object_t name;
	object_t value;

	if(form->type != OBJ_PAIR)
		return sys_error(5);
	if(((struct pair*)form)->car->type != OBJ_SYMBOL)
		return sys_error(5);
	name = ((struct pair*)form)->car;
	value = eval(cadr_op(form),env,null_cont);
	if(value->type == OBJ_ERROR)
		return value;
	return define_env(env,name,value);
}
object_t resume_argument_cont(struct cont *cont,object_t v)
{
	struct cont *prev = cont->prev;
	struct env *env = cont->data.argument_cont.env;
	object_t form = cont->data.argument_cont.form;
	object_t value;

	if(v->type == OBJ_ERROR)
		return v;
	value = eval(form,env,null_cont);
	return prev->resume(prev,cons_op(value,v));
}
struct cont *make_argument_cont(struct cont *prev,object_t form,struct env *env)
{
	struct cont *ret = malloc(sizeof(struct cont));
	ret->head.type = OBJ_CONT;
	ret->prev = prev;
	ret->resume = resume_argument_cont;
	ret->data.argument_cont.form = form;
	ret->data.argument_cont.env = env;
	return ret;
}
object_t eval_argument(object_t form,struct env *env,struct cont *cont)
{
	object_t value;

	if(form == scheme_null)
		return cont->resume(cont,scheme_null);
	else if(form->type == OBJ_PAIR)
	{
		return eval_argument(((struct pair*)form)->cdr,env,make_argument_cont(cont,((struct pair*)form)->car,env));
	}
	else
		return sys_error(9);
}
object_t resume_primitive_cont(struct cont *cont,object_t values)
{
	object_t ret;
	struct primitive *f = cont->data.primitive_cont.f;
	struct cont *prev = cont->prev;

	if(f->callback.op2 == call_with_current_continuation_op)
		values = cons_op(prev,values);
	if(length(values) != f->arg_num)
		return sys_error(12);
	switch(f->arg_num)
	{
	case 0:
		ret = (f->callback.op0)();
		break;
	case 1:
		ret = (f->callback.op1)(car_op(values));
		break;
	case 2:
		ret = (f->callback.op2)(car_op(values),cadr_op(values));
		break;
	case 3:
		ret = (f->callback.op3)(car_op(values),cadr_op(values),caddr_op(values));
		break;
	case 4:
		ret = (f->callback.op4)(car_op(values),cadr_op(values),caddr_op(values),cadddr_op(values));
		break;
	case 5:
		ret = (f->callback.op5)(car_op(values),cadr_op(values),caddr_op(values),cadddr_op(values),list_ref(values,4));
		break;
	case 6:
		ret = (f->callback.op6)(car_op(values),cadr_op(values),caddr_op(values),cadddr_op(values),list_ref(values,4),list_ref(values,5));
		break;
	}
	return prev->resume(prev,ret);
}
struct cont* make_primitive_cont(struct cont *prev,struct primitive *f)
{
	struct cont *ret = malloc(sizeof(struct cont));
	ret->head.type = OBJ_CONT;
	ret->data.primitive_cont.f = f;
	ret->prev = prev;
	ret->resume = resume_primitive_cont;
	return ret;
}
object_t eval_primitive(struct primitive *f,object_t form,struct env *env,struct cont *cont)
{
	return eval_argument(form,env,make_primitive_cont(cont,f));
}

object_t apply(struct procedure *f,object_t values)
{
	struct env *env;
	struct cont *cont;
	object_t variables;
	object_t body;

	env = f->env;
	cont = f->cont;
	variables = f->variables;
	body = f->body;
	env = extend_env(env,variables,values);
	return eval_begin(body,env,null_cont);
}
object_t resume_apply_cont(struct cont *cont,object_t values)
{
	struct cont *prev = cont->prev;
	struct procedure *f = cont->data.apply_cont.f;
	return prev->resume(prev,apply(f,values));
}
struct cont *make_apply_cont(struct cont *prev,struct procedure *f)
{
	struct cont *ret = malloc(sizeof(struct cont));
	ret->head.type = OBJ_CONT;
	ret->prev = prev;
	ret->resume = resume_apply_cont;
	ret->data.apply_cont.f = f;
	return ret;
}
object_t eval_procedure(struct procedure *f,object_t form,struct env *env,struct cont *cont)
{
	return eval_argument(form,env,make_apply_cont(cont,f));
}
object_t eval_macro()
{
	return scheme_null;
}

/*----------------------repl------------------------------*/
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

int main()
{
	object_t in;
	object_t ret;
	struct env *env;
	struct cont *cont;

	init();
	env = make_basic_environment();
	cont = null_cont;
	printf("welcome to yasfs!\n>");
	in = read(stdin);
	while(in)
	{
		ret = eval(in,env,cont);
		write(stdout,ret);
		printf("\n>");
		in = read(stdin);
	}
	return 0;
}
