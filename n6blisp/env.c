#include "type.h"
#include "gc.h"
#include "vm.h"
#include <stdio.h>
#include <stdlib.h>


#define env_base(env) obj_env_parent(env)
#define env_frame(env) obj_env_bindings(env)

static struct object core_syntax[] =
{
	{.tag=OBJ_CORE,.value={.core={DEFINE,"define"}}},
	{.tag=OBJ_CORE,.value={.core={IF,"if"}}},
	{.tag=OBJ_CORE,.value={.core={SET,"set!"}}},
	{.tag=OBJ_CORE,.value={.core={QUOTE,"quote"}}},
	{.tag=OBJ_CORE,.value={.core={BEGIN,"begin"}}},
	{.tag=OBJ_CORE,.value={.core={LAMBDA,"lambda"}}},
	{.tag=OBJ_CORE,.value={.core={SYNTAX_RULES,"syntax-rules"}}},
	{.tag=OBJ_CORE,.value={.core={DEFINE_SYNTAX,"define-syntax"}}},
};

static OBJ char_to_integer_proc(OBJ args);
static OBJ integer_to_char_proc(OBJ args);
static OBJ is_number_equal_proc(OBJ args2,OBJ arg2);

static struct object core_primitive[] = 
{
	{.tag=OBJ_PRIMITIVE,.value={.primitive={IMMEDIATE,"cons",CONS,2,0,0}}},
	{.tag=OBJ_PRIMITIVE,.value={.primitive={IMMEDIATE,"car",CAR,1,0,0}}},
	{.tag=OBJ_PRIMITIVE,.value={.primitive={IMMEDIATE,"cdr",CDR,1,0,0}}},
	{.tag=OBJ_PRIMITIVE,.value={.primitive={IMMEDIATE,"set-cdr!",SET_CDR,2,0,0}}},
	{.tag=OBJ_PRIMITIVE,.value={.primitive={IMMEDIATE,"set-car!",SET_CAR,2,0,0}}},
	{.tag=OBJ_PRIMITIVE,.value={.primitive={IMMEDIATE,"eq?",EQ,2,0,0}}},
	{.tag=OBJ_PRIMITIVE,.value={.primitive={IMMEDIATE,"+",ADD,2,0,0}}},
	{.tag=OBJ_PRIMITIVE,.value={.primitive={IMMEDIATE,"-",SUB,2,0,0}}},
	{.tag=OBJ_PRIMITIVE,.value={.primitive={IMMEDIATE,"*",MUL,2,0,0}}},
	{.tag=OBJ_PRIMITIVE,.value={.primitive={IMMEDIATE,"/",DIV,2,0,0}}},

	{.tag=OBJ_PRIMITIVE,.value={.primitive={DATA,"boolean?",TYPE,1,OBJ_BOOLEAN,OBJ_VOID}}},
	{.tag=OBJ_PRIMITIVE,.value={.primitive={DATA,"symbol?",TYPE,1,OBJ_SYMBOL,OBJ_VOID}}},
	{.tag=OBJ_PRIMITIVE,.value={.primitive={DATA,"char?",TYPE,1,OBJ_CHAR,OBJ_VOID}}},
	{.tag=OBJ_PRIMITIVE,.value={.primitive={DATA,"vector?",TYPE,1,OBJ_VECTOR,OBJ_VOID}}},
	{.tag=OBJ_PRIMITIVE,.value={.primitive={DATA,"procedure?",TYPE,1,OBJ_PROCEDURE,OBJ_VOID}}},
	{.tag=OBJ_PRIMITIVE,.value={.primitive={DATA,"pair?",TYPE,1,OBJ_PAIR,OBJ_VOID}}},
 	{.tag=OBJ_PRIMITIVE,.value={.primitive={DATA,"number?",TYPE,1,OBJ_NUMBER,OBJ_VOID}}},
 	{.tag=OBJ_PRIMITIVE,.value={.primitive={DATA,"string?",TYPE,1,OBJ_STRING,OBJ_VOID}}},

 	{.tag=OBJ_PRIMITIVE,.value={.primitive={FUNCALL,"char->integer",FC1,1,0,char_to_integer_proc}}},
 	{.tag=OBJ_PRIMITIVE,.value={.primitive={FUNCALL,"integer->char",FC1,1,0,integer_to_char_proc}}},
	{.tag=OBJ_PRIMITIVE,.value={.primitive={FUNCALL,"=",FC2,2,0,is_number_equal_proc}}},
	{.tag=OBJ_PRIMITIVE,.value={.primitive={IMMEDIATE,">",GT,2,0,0}}},
/* 	{.tag=OBJ_PRIMITIVE,.value={.primitive={DATA,"port?",TYPE,1,OBJ_PORT,OBJ_VOID}}},
 
 	add_procedure("number->string", number_to_string_proc);
	add_procedure("string->number", string_to_number_proc);
	add_procedure("symbol->string", symbol_to_string_proc);
	add_procedure("string->symbol", string_to_symbol_proc);
      



	add_procedure("quotient" , quotient_proc);
	add_procedure("remainder", remainder_proc);

	add_procedure("<"        , is_less_than_proc);
	add_procedure(">"        , is_greater_than_proc);






	add_procedure("list"    , list_proc);

	add_procedure("eq?", is_eq_proc);

	add_procedure("apply", apply_proc);
    
	add_procedure("interaction-environment", 
		      interaction_environment_proc);
	add_procedure("null-environment", null_environment_proc);
	add_procedure("environment"     , environment_proc);
	add_procedure("eval"            , eval_proc);

	add_procedure("load"             , load_proc);
	add_procedure("open-input-port"  , open_input_port_proc);
	add_procedure("close-input-port" , close_input_port_proc);
	add_procedure("input-port?"      , is_input_port_proc);
	add_procedure("read"             , read_proc);
	add_procedure("read-char"        , read_char_proc);
	add_procedure("peek-char"        , peek_char_proc);
	add_procedure("eof-object?"      , is_eof_object_proc);
	add_procedure("open-output-port" , open_output_port_proc);
	add_procedure("close-output-port", close_output_port_proc);
	add_procedure("output-port?"     , is_output_port_proc);
	add_procedure("write-char"       , write_char_proc);
	add_procedure("write"            , write_proc);

	add_procedure("error", error_proc);
*/
};
OBJ make_env(OBJ base,OBJ frame)
{
	return obj_make_env(base,frame);
}

OBJ define(OBJ name,OBJ value,OBJ env)
{
	OBJ find;

	find = assq(name,env_frame(env));
	if(find == OBJ_NULL)
	{
		find = cons(name,value);
		env_frame(env) = cons(find,env_frame(env));
	}
	else
	{
		cdr(find) = value;
	}
	return find;
}

OBJ lookup_variable_cell(OBJ name,OBJ env)
{
	OBJ find;

	while(!nullp(env))
	{
		find = assq(name,env_frame(env));
		if(!nullp(find))
			return find;
		env = env_base(env);
	}
	if(find == OBJ_NULL)
		fprintf(stderr,"undefined variable:%s\n",obj_symbol_data(name));
	return OBJ_NULL;
}

OBJ lookup_variable_value(OBJ name,OBJ env)
{
	OBJ find;

	while(!nullp(env))
	{
		find = assq(name,env_frame(env));
		if(!nullp(find))
			return cdr(find);
		env = env_base(env);
	}
	if(find == OBJ_NULL)
		fprintf(stderr,"undefined variable:%s\n",obj_symbol_data(name));
	return OBJ_NULL;
}

void set_variable_value(OBJ name,OBJ value,OBJ env)
{
	OBJ frame;
	OBJ names;
	OBJ values;

	while(!nullp(env))
	{
		frame = env_frame(env);
		names = car(frame);
		values = cdr(frame);
		while(!nullp(names))
		{
			if(eq(car(names),name))
			{
				car(values) = value;
					return;
			}
			names = cdr(names);
			values = cdr(values);
		}
		env = env_base(env);
	}
	fprintf(stderr, "can't set! unbound variable, %s\n", obj_symbol_data(name));
}

OBJ eval_proc(OBJ arguments) 
{
    fprintf(stderr, "illegal state: The body of the eval "
            "primitive procedure should not execute.\n");
    exit(1);
}

static OBJ char_to_integer_proc(OBJ args)
{
	int value = obj_unbox_character(args);
	return obj_make_integer(value);
}

static OBJ integer_to_char_proc(OBJ args)
{
	int value = obj_unbox_integer(args);
	return obj_make_character(value);
}

static OBJ is_number_equal_proc(OBJ arg1,OBJ arg2)
{
	if(obj_numberp(arg1) && obj_numberp(arg2))
	{
		return obj_make_boolean(obj_number_data(arg1) == obj_number_data(arg2));
	}
	else
	{
		fprintf(stderr,"= can't use between non-number");
		return OBJ_NULL;
	}
}

OBJ env_null()
{
	OBJ ret;
	int i;
	ret = make_env(OBJ_NULL,OBJ_NULL);
	for(i=0; i<sizeof(core_syntax)/sizeof(core_syntax[0]); i++)
	{
		define(obj_make_symbol(core_syntax[i].value.core.name),&core_syntax[i],ret);
	}
	return ret;
}

OBJ env_init()
{
	OBJ ret;
	int i;

	ret = env_null();
	for(i=0; i<sizeof(core_primitive)/sizeof(core_primitive[0]); i++)
	{
		define(obj_make_symbol(core_primitive[i].value.primitive.name),&core_primitive[i],ret);
	}
	return ret;
}

#ifdef ENV_TEST

int main()
{
	OBJ env;
	OBJ find;
	OBJ name;

	env = env_null();
	obj_write(stdout,env_frame(env));
	printf("\n");

	name = obj_make_symbol("if");
	find = lookup_variable_value(name,env);
	obj_write(stdout,find);
	find = lookup_variable_value(name,env);
	obj_write(stdout,find);

	printf("\n");
	obj_write(stdout,env_frame(env));
	printf("\n");

	gc(env);
	printf("\n");
	obj_write(stdout,env_frame(env));
	printf("\n");
	return 0;
}
#endif
