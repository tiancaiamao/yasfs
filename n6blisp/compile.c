#include "compile.h"
#include "type.h"
#include "env.h"
#include "vm.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct vec
{
	char *begin;
	char *end;
	char *cur;
};

struct analyze_t
{
	OBJ sexp;
	OBJ env;
	int macro;
	int tail;
	OBJ params;
	OBJ macro_expand_env;
};
static OBJ analyze_r(const struct analyze_t *args);  /*declare first*/
static void generate_r(struct vec *v,OBJ ast);
static char* generate(OBJ ast);

static void emit(struct vec *v,char op)
{

	if((v->cur+1) >= v->end)
	{
		unsigned int size = v->end - v->begin;
		v->begin = realloc(v->begin,2*size);
		if(v->begin == NULL)
			exit(-1);
		v->end = v->begin + 2*size;
		v->cur = v->begin + size;
	}
	*(v->cur) = op;
	v->cur++;
}

static void emitv(struct vec *v,OBJ value)
{
	if((v->cur+sizeof(OBJ)) >= v->end)
	{
		unsigned int size = v->end - v->begin;
		v->begin = realloc(v->begin,2*size);
		if(v->begin == NULL)
			exit(-1);
		v->end = v->begin + 2*size;
		v->cur = v->begin + size;
	}
	*((OBJ*)(v->cur)) = value;
	v->cur += sizeof(OBJ);
}

static OBJ analyze_variable_cell(OBJ variable,OBJ env,int macro,OBJ params,OBJ macro_expand_env)
{
	OBJ tmp;
	OBJ ret;
	if(macro)
	{
		tmp = assq(variable,params);
		if(nullp(tmp)) /* if variable isn't params of macro,lookup it at macro_define_env */
			ret = lookup_variable_cell(variable,env);
		else	/* otherwise lookup variable at macro_expand_env...ie,current env */
			ret = lookup_variable_cell(cdr(tmp),macro_expand_env);
	}
	else
		ret = lookup_variable_cell(variable, env);
	return ret;
}

static OBJ analyze_variable_value(OBJ variable,OBJ env,int macro,OBJ params,OBJ macro_expand_env)
{
	OBJ ret;
	ret = analyze_variable_cell( variable,env, macro, params, macro_expand_env);
	if(ret != OBJ_NULL)
		return cdr(ret);
	else
		return OBJ_NULL;
}

static OBJ analyze_define(const struct analyze_t *args)
{
	OBJ name;
	OBJ ast;
	OBJ cell;
	struct analyze_t new_arg;

	new_arg = *args;
	if(obj_pairp(car(new_arg.sexp)))
	{
/*fix me.......................*/
	}
	else if(obj_symbolp(car(new_arg.sexp)))
	{
		name = car(new_arg.sexp);
		cell = define(name,OBJ_VOID,new_arg.env);
		new_arg.sexp = cadr(new_arg.sexp);
		new_arg.tail = 0;
		ast = analyze_r(&new_arg);
		return obj_make_define(cell,ast);
	}
	else
	{
		fprintf(stderr,"left value is not a symbol in define.\n");
		return OBJ_NULL;
	}
	return OBJ_NULL;
}

static OBJ analyze_set(const struct analyze_t *args)
{
	OBJ name;
	OBJ ast;
	OBJ cell;
	struct analyze_t new_arg;

	new_arg = *args;
	name = car(new_arg.sexp);
	if(!obj_symbolp(name))
	{
		fprintf(stderr,"left value is not a symbol in define.\n");
		return OBJ_NULL;
	}
	cell = analyze_variable_cell(name,new_arg.env,new_arg.macro,new_arg.params,new_arg.macro_expand_env);
	new_arg.sexp = cadr(new_arg.sexp);
	new_arg.tail = 0;
	ast = analyze_r(&new_arg);
	if(ast==OBJ_NULL) 
	{
		fprintf(stderr,"wrong form of set!\n");
		return OBJ_NULL;
	}

	return (cell == OBJ_NULL) ? cell : obj_make_define(cell,ast);
}

static OBJ analyze_if(OBJ sexp,OBJ env,int tail)
{
	OBJ test;
	OBJ consequent;
	OBJ alternate;
	struct analyze_t args;

	if(!obj_pairp(sexp))
		goto error;
	args.sexp = car(sexp);
	args.env = env;
	args.tail = 0;
	test = analyze_r(&args);
	sexp = cdr(sexp);
	if(!obj_pairp(sexp))
		goto error;
	args.sexp = car(sexp);
	args.env = env;
	args.tail = tail;
	consequent = analyze_r(&args);
	sexp = cdr(sexp);
	if(!obj_pairp(sexp))
		alternate = OBJ_TRUE;
	else
	{
		args.sexp = car(sexp);
		args.env = env;
		args.tail = tail;
		alternate = analyze_r(&args);
	}
	return obj_make_if(test,consequent,alternate);
error:
	fprintf(stderr,"wrong form of if");
	exit(-1);
}

static OBJ analyze_params(OBJ sexp,OBJ env)
{
	OBJ data;
	struct analyze_t args;

	args.env = env;
	args.tail = 0;
	data = OBJ_NULL;
	while(!nullp(sexp))
	{
		args.sexp = car(sexp);
		data = cons(analyze_r(&args),data);
		sexp = cdr(sexp);
	}
	return obj_make_begin(data);
}

static OBJ analyze_begin(const struct analyze_t *args)
{
	OBJ data;
	OBJ sexp;
	struct analyze_t new_arg;

	new_arg = *args;
	sexp = args->sexp;
	data = OBJ_NULL;
	while(!nullp(sexp))
	{ 
		new_arg.sexp = car(sexp);
		data = cons(analyze_r(&new_arg),data);
		sexp = cdr(sexp);
	}
	data = reverse(data);
	return obj_make_begin(data);
}

static OBJ analyze_lambda(const struct analyze_t *arg)
{
	struct analyze_t new_arg;
	OBJ newenv;
	OBJ body;
	OBJ p,q;
	OBJ formals;
	OBJ tmp;

	new_arg = *arg;
	newenv = make_env(new_arg.env,OBJ_NULL);
	p = car(new_arg.sexp);
	q = OBJ_NULL;
	formals = OBJ_NULL;

	while(obj_pairp(p))	       /* (lambda (<variable1> ...) <body>) */
	{
		tmp = cons(define(car(p),OBJ_VOID,newenv),OBJ_NULL);
		if(formals == OBJ_NULL)
			formals = tmp;
		else
			cdr(q) = tmp;
		q = tmp;

		p = cdr(p);
	}
	if(!nullp(p)) 
	{
		if(formals != OBJ_NULL)	/* (lambda (<variable1> ... <variablen> . <variablen+1>) <body>) */
			cdr(q) = define(p,OBJ_VOID,newenv);
		else
			formals = define(p,OBJ_VOID,newenv); /*  (lambda <variable> <body>) */
	}

	new_arg.sexp = cdr(new_arg.sexp);
	new_arg.env = newenv;
	new_arg.tail = 1;
	body = analyze_begin(&new_arg);
	return obj_make_lambda(newenv,formals,body);
}

static OBJ analyze_syntax_rules(OBJ sexp,OBJ env)
{
	return obj_make_syntax(cdr(sexp),env);
}

static char* declare_label(struct vec *v)
{
	char *ret = v->cur;
	emitv(v,0);
	return ret;
}
static void make_label(struct vec *v,char *label)
{
	*((OBJ*)label) = (OBJ)(v->cur);
}

static void generate_if(struct vec *v,OBJ ast)
{
	char* label1;
	char* label2;

	generate_r(v,obj_if_test(ast));
	emit(v,JUMP_UNLESS);
	label1 = declare_label(v);
	generate_r(v,obj_if_consequent(ast));
	emit(v,JUMP);
	label2 = declare_label(v);
	make_label(v,label1);
	generate_r(v,obj_if_alternate(ast));
	make_label(v,label2);
}

static void generate_begin(struct vec *v,OBJ ast)
{
	OBJ list = obj_begin_data(ast);

	while(!nullp(list))
	{
		generate_r(v,car(list));
		list = cdr(list);
	}
}

static void generate_lambda(struct vec *v,OBJ ast)
{
	struct vec vec;
	OBJ procedure;
	unsigned length;
	OBJ code;

	vec.begin = malloc(100);
	vec.end = vec.begin + 100;
	vec.cur = vec.begin;

	generate_r(&vec,obj_lambda_body(ast));
	emit(&vec,RET);
	length = vec.cur - vec.begin;
	code = obj_make_string_2(length);
	memcpy(obj_string_data(code),vec.begin,length);
	free(vec.begin);

	procedure = obj_make_procedure(obj_lambda_env(ast),code,obj_lambda_formals(ast));

	emit(v,PUSH);
	emitv(v,procedure);
}

static void generate_app(struct vec *v,OBJ ast)
{
	OBJ procedure;
	OBJ primitive;
	OBJ cell;
	OBJ formals;

	if(obj_app_type(ast) == 0) /* procedure */
	{
		generate_begin(v,obj_app_params(ast));
		procedure = obj_app_data(ast);
		formals = obj_procedure_formals(procedure);
		while(obj_pairp(formals))
		{
			emit(v,PUSH);
			emitv(v,car(formals));
			emit(v,SET_CDR);
			emit(v,POP);
			formals = cdr(formals);
		}
		if(!nullp(formals))
		{
			emit(v,PUSH);
			emitv(v,formals);
			/* fixme: something should do to support other form formals */
		}
		emit(v,PUSH);
		emitv(v,procedure);
		emit(v,obj_app_tail(ast)?TAIL_CALL:CALL);
	}
	else if(obj_app_type(ast) == 1) /* primitive */
	{
		generate_begin(v,obj_app_params(ast));
		primitive = obj_app_data(ast);
		switch(obj_primitive_type(primitive))
		{
		case DATA:
			emit(v,obj_primitive_opcode(primitive));
			emitv(v,obj_primitive_data(primitive));
			break;
		case FUNCALL:
			emit(v,obj_primitive_opcode(primitive));
			emitv(v,obj_primitive_proc(primitive));
			break;
		default:
			emit(v,obj_primitive_opcode(primitive));
		}
	}
	else if(obj_app_type(ast) == 2) /* uninitialized procedure */
	{
		cell = obj_app_data(ast);
		generate_begin(v,obj_app_params(ast));
		emit(v,PUSH);
		emitv(v,cell);
		emit(v,UNINIT_REF);
		emit(v,BIND);
		emit(v,obj_app_tail(ast)?TAIL_CALL:CALL);
	}
}

static int is_self_evaluating(OBJ exp) 
{
	return obj_booleanp(exp)   ||
		obj_fixnump(exp)    ||
		obj_charp(exp) ||
		obj_stringp(exp);
}
static int is_variable(OBJ expression) 
{
	return obj_symbolp(expression);
}
/*
static OBJ analyze(OBJ sexp,OBJ env,int tail)
{
	OBJ ret;
	OBJ op;

	if (is_self_evaluating(sexp))
		ret = sexp;
	else if (is_variable(sexp)) 
		ret = lookup_variable_cell(sexp, env);
	else if(obj_pairp(sexp))
	{
		if(obj_pairp(car(sexp)))
		{
			op = eval(car(sexp),env);
		}
		else
			op = lookup_variable_value(car(sexp),env);
		if(op == OBJ_NULL) 
			return OBJ_NULL;
		if(obj_corep(op))
		{
			switch(obj_core_type(op))
			{
			case DEFINE:
			case DEFINE_SYNTAX:
				ret = analyze_define(cdr(sexp),env);
				break;
			case SET:
				ret = analyze_set(cdr(sexp),env);
				break;
			case IF:
				ret = analyze_if(cdr(sexp),env,tail);
				break;
			case QUOTE:
				ret = obj_make_quote(cadr(sexp));
				break;
			case BEGIN:
				ret = analyze_begin(cdr(sexp),env,tail);
				break;
			case LAMBDA:
				ret = analyze_lambda(cdr(sexp),env);
				break;
			case SYNTAX_RULES:
				ret = analyze_syntax_rules(cdr(sexp),env);
				break;
			default:
				fprintf(stderr,"unknown core tag\n");
			}
		}
		else if(obj_syntaxp(op))
		{
			OBJ params;
			OBJ data;
			OBJ patten;
			OBJ template;
			int match;

			match = 0;
			data = obj_syntax_data(op);
			while(obj_pairp(data))
			{
				patten = caar(data);
				template = cdar(data);
				if(eq(car(patten),car(sexp)))
				{
					patten = cdr(patten);
					sexp = cdr(sexp);
					params = OBJ_NULL;
					while(obj_pairp(patten) && obj_pairp(sexp))
					{
						params = cons(cons(car(patten),car(sexp)),parmms);
						patten = cdr(patten);
					}
					if(nullp(patten) && nullp(sexp))
					{
						match = 1;
						break;
					}
				}
				data = cdr(data);
			}
			if(match == 0)
			{
				fprintf(stderr,"can't match any patten of macro");
				exit(-1);
			}
			ret = analyze(template,param,env,obj_syntax_env(op));
		}
		else if(obj_procedurep(op))
		{
			OBJ params;
			params = analyze_params(cdr(sexp),env);
			ret = obj_make_app(0,params,op,tail);
		}
		else if(obj_primitivep(op))
		{
			OBJ params;
			params = analyze_params(cdr(sexp),env);
			ret = obj_make_app(1,params,op,0);
		}
		else if(op == OBJ_VOID)
		{
			OBJ params;
			params = analyze_params(cdr(sexp),env);
			op = lookup_variable_cell(car(sexp), env);
			ret = obj_make_app(2,params,op,tail);
		}
	}
	return ret;
}
*/

/* similar to eval,but use different args, it's a hack for macro because macro is not eval in one env,actually */
static OBJ fake_eval(const struct analyze_t *arg)
{
	char *bytecode;
	OBJ ast;
	OBJ ret;

	ast = analyze_r(arg);
	if(ast == OBJ_NULL)
	{
		fprintf(stderr,"ast error");
		return NULL;
	}
	bytecode = generate(ast);
	if(bytecode == NULL)
	{
		fprintf(stderr,"compile error");
		return OBJ_NULL;
	}
	ret = vm(bytecode);
	free(bytecode);
	return ret;
}

static OBJ analyze_r(const struct analyze_t *arg)
{
	OBJ op;
	OBJ ret;
	struct analyze_t new_arg;

	new_arg = *arg;
	ret = OBJ_NULL;

	if (is_self_evaluating(new_arg.sexp))
		ret = new_arg.sexp;
	else if (is_variable(new_arg.sexp))
		ret = analyze_variable_cell(new_arg.sexp,new_arg.env,new_arg.macro,new_arg.params,new_arg.macro_expand_env);
	else if(obj_pairp(new_arg.sexp))
 	{
		if(obj_pairp(car(new_arg.sexp)))
		{
			new_arg.sexp = car(new_arg.sexp);
			op = fake_eval(&new_arg);
			new_arg = *arg;
		}
		else
			op = analyze_variable_value(car(new_arg.sexp),new_arg.env,new_arg.macro,new_arg.params,new_arg.macro_expand_env);
		if(op == OBJ_NULL) /* error handle---fixme!! */
			return OBJ_NULL;
		if(obj_corep(op))
		{
			switch(obj_core_type(op))
			{
			case DEFINE:
			case DEFINE_SYNTAX:
				new_arg.sexp = cdr(new_arg.sexp);
				ret = analyze_define(&new_arg);
				break;
			case SET:
				new_arg.sexp = cdr(new_arg.sexp);
				ret = analyze_set(&new_arg);
				break;
			case IF:
				ret = analyze_if(cdr(new_arg.sexp),new_arg.env,new_arg.tail);
				break;
			case QUOTE:
				ret = obj_make_quote(cadr(new_arg.sexp));
				break;
			case BEGIN:
				new_arg.sexp = cdr(new_arg.sexp);
				ret = analyze_begin(&new_arg);
				break;
			case LAMBDA:
				new_arg.sexp = cdr(new_arg.sexp);
				ret = analyze_lambda(&new_arg);
				break;
			case SYNTAX_RULES:
				ret = analyze_syntax_rules(cdr(new_arg.sexp),new_arg.env);
				break;
			default:
				fprintf(stderr,"unknown core tag\n");
			}
		}
		else if(obj_syntaxp(op))
		{
			OBJ params;
			OBJ data;
			OBJ patten;
			OBJ template;
			int match;

			match = 0;
			data = obj_syntax_data(op);
			while(obj_pairp(data))
			{
				patten = caar(data);
				template = cadar(data);
				if(eq(car(patten),car(new_arg.sexp))) /* match patten */
				{
					patten = cdr(patten);
					new_arg.sexp = cdr(new_arg.sexp);
					params = OBJ_NULL;
					while(obj_pairp(patten) && obj_pairp(new_arg.sexp))
					{
						params = cons(cons(car(patten),car(new_arg.sexp)),params);
						patten = cdr(patten);
						new_arg.sexp = cdr(new_arg.sexp);
					}
					if(nullp(patten) && nullp(new_arg.sexp))
					{
						match = 1;
						break;
					}
				}
				data = cdr(data);
			}
			if(match == 0)
			{
				fprintf(stderr,"can't match any patten of macro");
				exit(-1);
			}
			new_arg.sexp = template;
			new_arg.env = obj_syntax_env(op);
			new_arg.macro = 1;
			new_arg.params = params;
			new_arg.macro_expand_env = new_arg.env;
			new_arg.tail = new_arg.tail;
			ret = analyze_r(&new_arg);
		}
		else if(obj_procedurep(op))
		{
			OBJ params;
			params = analyze_params(cdr(new_arg.sexp),new_arg.env);
			ret = obj_make_app(0,params,op,new_arg.tail);
		}
		else if(obj_primitivep(op))
		{
			OBJ params;
			params = analyze_params(cdr(new_arg.sexp),new_arg.env);
			ret = obj_make_app(1,params,op,0);
		}
		else if(op == OBJ_VOID)
		{
			OBJ params;
			params = analyze_params(cdr(new_arg.sexp),new_arg.env);
			op = lookup_variable_cell(car(new_arg.sexp), new_arg.env);
			ret = obj_make_app(2,params,op,new_arg.tail);
		}
	}
	return ret;
}

static OBJ analyze(OBJ sexp,OBJ env)
{
	struct analyze_t args;
	args.sexp = sexp;
	args.env = env;
	args.tail = 0;
	args.macro = 0;
	return analyze_r(&args);
}

static void generate_r(struct vec *v,OBJ ast)
{
	if(obj_pointerp(ast))
	{
		switch(obj_pointer_tag(ast))
		{
		case OBJ_AST_DEFINE:
			generate_r(v,obj_define_ast(ast));
			emit(v,PUSH);
			emitv(v,obj_define_cell(ast));
			emit(v,SET_CDR);
			break;
		case OBJ_AST_IF:
			generate_if(v,ast);
			break;
		case OBJ_AST_BEGIN:
			generate_begin(v,ast);
			break;
		case OBJ_AST_LAMBDA:
			generate_lambda(v,ast);
			break;
		case OBJ_AST_APP:
			generate_app(v,ast);
			break;
		case OBJ_AST_QUOTE:
			emit(v,PUSH);
			emitv(v,obj_quote_data(ast));
			break;
		case OBJ_PAIR: /* pair for variable's ast,it's a env cell,actually*/
			emit(v,PUSH);
			emitv(v,ast);
			emit(v,REF);
			break;
		default:
			emit(v,PUSH);
			emitv(v,ast);
		}
	}
	else
	{
		emit(v,PUSH);
		emitv(v,ast);
	}
}

static char* generate(OBJ ast)
{
	struct vec v;

	v.begin = malloc(100);
	v.end = v.begin + 100;
	v.cur = v.begin;

	generate_r(&v,ast);
	emit(&v,DONE);
	return v.begin;
}

/* NOTE: free returned char* after use compile to avoid memory leak! */
char* compile(OBJ sexp,OBJ env)
{
	OBJ ast;

	ast = analyze(sexp,env);
	if(ast == OBJ_NULL)
	{
		fprintf(stderr,"ast error");
		return NULL;
	}
	return generate(ast);
}
