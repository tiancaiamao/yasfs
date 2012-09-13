#include "type.h"
#include "mem.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

OBJ obj_alloc_tagged(unsigned size,unsigned tag)
{
	OBJ ret;
	ret = (OBJ)mem_alloc(size);
	if(ret == NULL)
	{
		fprintf(stderr,"out of memory");
		exit(-1);
	}
	obj_pointer_tag(ret) = tag;
	return ret;
}
OBJ obj_make_flonum(double f) 
{
	OBJ ret = obj_alloc_type(flonum, OBJ_FLONUM);
	obj_flonum_value(ret) = f;
	return ret;
}

OBJ obj_make_env(OBJ parent,OBJ bindings)
{
	OBJ ret = obj_alloc_type(env,OBJ_ENV);
	obj_env_parent(ret) = parent;
	obj_env_bindings(ret) = bindings;
	return ret;
}

OBJ obj_make_string(OBJ len, OBJ ch) 
{
	int clen = obj_unbox_integer(len);
	if (clen < 0) 
	{
		fprintf(stderr,"wrong arg");
		return OBJ_NULL;
	}
	OBJ s = (OBJ)mem_alloc(obj_sizeof(string)+clen+1);
	obj_pointer_tag(s) = OBJ_STRING;

	obj_string_length(s) = clen;
	if (obj_charp(ch))
		memset(obj_string_data(s), obj_unbox_character(ch), clen);
	obj_string_data(s)[clen] = '\0';
	return s;
}

OBJ obj_make_string_1(const char *str) 
{
	int clen = strlen(str);
	OBJ ret = (OBJ)mem_alloc(obj_sizeof(string)+clen+1);
	obj_pointer_tag(ret) = OBJ_STRING;
	obj_string_length(ret) = clen;
	strcpy(obj_string_data(ret),str);
	obj_string_data(ret)[clen] = '\0';
	return ret;
}

OBJ obj_make_string_2(unsigned len) 
{
	OBJ s = (OBJ)mem_alloc(obj_sizeof(string)+len+1);
	obj_pointer_tag(s) = OBJ_STRING;
	obj_string_length(s) = len;
	return s;
}

OBJ obj_make_symbol(const char *str) 
{
	int clen = strlen(str);
	OBJ ret = (OBJ)mem_alloc(obj_sizeof(symbol)+clen+1);
	obj_pointer_tag(ret) = OBJ_SYMBOL;
	obj_symbol_length(ret) = clen;
	strcpy(obj_symbol_data(ret),str);
	obj_symbol_data(ret)[clen] = '\0';
	return ret;
}

OBJ obj_make_define(OBJ cell,OBJ ast)
{
	OBJ ret = (OBJ)obj_alloc_type(define,OBJ_AST_DEFINE);
	obj_define_cell(ret) = cell;
	obj_define_ast(ret) = ast;
	return ret;
}

OBJ obj_make_if(OBJ test,OBJ consequent,OBJ alternate)
{
	OBJ ret = (OBJ)obj_alloc_type(define,OBJ_AST_IF);
	obj_if_test(ret) = test;
	obj_if_consequent(ret) = consequent;
	obj_if_alternate(ret) = alternate;
	return ret;
}

OBJ obj_make_begin(OBJ data)
{
	OBJ ret = (OBJ)obj_alloc_type(begin,OBJ_AST_BEGIN);
	obj_begin_data(ret) = data;
	return ret;
}

OBJ obj_make_quote(OBJ data)
{
	OBJ ret = (OBJ)obj_alloc_type(quote,OBJ_AST_QUOTE);
	obj_quote_data(ret) = data;
	return ret;
}

OBJ obj_make_lambda(OBJ env,OBJ formals,OBJ body)
{
	OBJ ret = (OBJ)obj_alloc_type(lambda,OBJ_AST_LAMBDA);
	obj_lambda_env(ret) = env;
	obj_lambda_formals(ret) = formals;
	obj_lambda_body(ret) = body;
	return ret;
}


OBJ obj_make_procedure(OBJ env,OBJ code,OBJ formals)
{
	OBJ ret = (OBJ)obj_alloc_type(procedure,OBJ_PROCEDURE);
	obj_procedure_env(ret) = env;
	obj_procedure_code(ret) = code;
	obj_procedure_formals(ret) = formals;
	return ret;
}

OBJ obj_make_syntax(OBJ data,OBJ env)
{
	OBJ ret = (OBJ)obj_alloc_type(syntax,OBJ_SYNTAX);
	obj_syntax_data(ret) = data;
	obj_syntax_env(ret) = env;
	return ret;
}

OBJ obj_make_app(char type,OBJ params,OBJ data,int tail)
{
	OBJ ret = (OBJ)obj_alloc_type(app,OBJ_AST_APP);
	obj_app_params(ret) = params;
	obj_app_data(ret) = data;
	obj_app_type(ret) = type;
	if(type == 0)
		obj_app_tail(ret) = tail;
	return ret;
}

OBJ cons (OBJ head, OBJ tail) 
{
	OBJ pair = obj_alloc_type(pair, OBJ_PAIR);
	car(pair) = head;
	cdr(pair) = tail;
	return pair;
}
OBJ listp (OBJ lst) 
{
	while(obj_pairp(lst))
		lst = cdr(lst);
	return obj_make_boolean(lst == OBJ_NULL);
}

int eq(OBJ a,OBJ b)
{
	if(obj_symbolp(a) && obj_symbolp(b))
		return strcmp(obj_symbol_data(a),obj_symbol_data(b))== 0 ;
	return a == b;
}

OBJ add(OBJ a,OBJ b)
{
	int fix_a;
	double flo_a;
	int fix_b;
	double flo_b;
	OBJ ret;

	if(!obj_numberp(a) || !obj_numberp(b))
	{
		fprintf(stderr,"error:can't add between non-number!");
		exit(0);
	}
	if(obj_fixnump(a) && obj_fixnump(b))
	{
		fix_a = obj_unbox_integer(a);
		fix_b = obj_unbox_integer(b);
		ret = obj_make_integer(fix_a+fix_b);
	}
	else if(obj_flonump(a) && obj_fixnump(b))
	{
		flo_a = obj_flonum_value(a);
		fix_b = obj_unbox_integer(b);
		ret = obj_make_flonum(flo_a+fix_b);
	}
	else if(obj_fixnump(a) && obj_flonump(b))
	{
		fix_a = obj_unbox_integer(a);
		flo_b = obj_flonum_value(b);
		ret = obj_make_flonum(fix_a+flo_b);
	}
	else
	{
		flo_a = obj_flonum_value(a);
		flo_b = obj_flonum_value(b);
		ret = obj_make_flonum(flo_a+flo_b);
	}
	return ret;
}

OBJ sub(OBJ a,OBJ b)
{
	int fix_a;
	double flo_a;
	int fix_b;
	double flo_b;
	OBJ ret;

	if(!obj_numberp(a) || !obj_numberp(b))
	{
		fprintf(stderr,"error:can't add between non-number!");
		exit(0);
	}
	if(obj_fixnump(a) && obj_fixnump(b))
	{
		fix_a = obj_unbox_integer(a);
		fix_b = obj_unbox_integer(b);
		ret = obj_make_integer(fix_a-fix_b);
	}
	else if(obj_flonump(a) && obj_fixnump(b))
	{
		flo_a = obj_flonum_value(a);
		fix_b = obj_unbox_integer(b);
		ret = obj_make_flonum(flo_a-fix_b);
	}
	else if(obj_fixnump(a) && obj_flonump(b))
	{
		fix_a = obj_unbox_integer(a);
		flo_b = obj_flonum_value(b);
		ret = obj_make_flonum(fix_a-flo_b);
	}
	else
	{
		flo_a = obj_flonum_value(a);
		flo_b = obj_flonum_value(b);
		ret = obj_make_flonum(flo_a-flo_b);
	}
	return ret;
}

OBJ mul(OBJ a,OBJ b)
{
	int fix_a;
	double flo_a;
	int fix_b;
	double flo_b;
	OBJ ret;

	if(!obj_numberp(a) || !obj_numberp(b))
	{
		fprintf(stderr,"error:can't add between non-number!");
		exit(0);
	}
	if(obj_fixnump(a) && obj_fixnump(b))
	{
		fix_a = obj_unbox_integer(a);
		fix_b = obj_unbox_integer(b);
		ret = obj_make_integer(fix_a*fix_b);
	}
	else if(obj_flonump(a) && obj_fixnump(b))
	{
		flo_a = obj_flonum_value(a);
		fix_b = obj_unbox_integer(b);
		ret = obj_make_flonum(flo_a*fix_b);
	}
	else if(obj_fixnump(a) && obj_flonump(b))
	{
		fix_a = obj_unbox_integer(a);
		flo_b = obj_flonum_value(b);
		ret = obj_make_flonum(fix_a*flo_b);
	}
	else
	{
		flo_a = obj_flonum_value(a);
		flo_b = obj_flonum_value(b);
		ret = obj_make_flonum(flo_a*flo_b);
	}
	return ret;
}

/* OBJ div(OBJ a,OBJ b) */
/* { */
/* 	int fix_a; */
/* 	double flo_a; */
/* 	int fix_b; */
/* 	double flo_b; */
/* 	OBJ ret; */

/* 	if(!obj_numberp(a) || !obj_numberp(b)) */
/* 	{ */
/* 		fprintf(stderr,"error:can't add between non-number!"); */
/* 		exit(0); */
/* 	} */
/* 	if(obj_fixnump(a) && obj_fixnump(b)) */
/* 	{ */
/* 		fix_a = obj_unbox_integer(a); */
/* 		fix_b = obj_unbox_integer(b); */
/* 		ret = obj_make_integer(fix_a/fix_b); */
/* 	} */
/* 	else if(obj_flonump(a) && obj_fixnump(b)) */
/* 	{ */
/* 		flo_a = obj_flonum_value(a); */
/* 		fix_b = obj_unbox_integer(b); */
/* 		ret = obj_make_flonum(flo_a/fix_b); */
/* 	} */
/* 	else if(obj_fixnump(a) && obj_flonump(b)) */
/* 	{ */
/* 		fix_a = obj_unbox_integer(a); */
/* 		flo_b = obj_flonum_value(b); */
/* 		ret = obj_make_flonum(flo_a/fix_b); */
/* 	} */
/* 	else */
/* 	{ */
/* 		flo_a = obj_flonum_value(a); */
/* 		flo_b = obj_flonum_value(b); */
/* 		ret = obj_make_flonum(flo_a/flo_b); */
/* 	} */
/* 	return ret; */
/* } */

OBJ assq(OBJ obj,OBJ list)
{
	OBJ tmp;

	while(obj_pairp(list))
	{
		tmp = car(list);
		if(obj_pairp(tmp) && eq(obj,car(tmp)))
			return tmp;
		list = cdr(list);
	}
	return OBJ_NULL;
}

OBJ reverse(OBJ list)
{
	OBJ ret;
	OBJ tmp;

	ret = OBJ_NULL;
	while(!nullp(list))
	{
		tmp = list;
		list = cdr(list);
		cdr(tmp) = ret;
		ret = tmp;
	}
	return ret;
}
