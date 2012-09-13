#include "eval.h"
#include "type.h"
#include "env.h"
#include "gc.h"
#include "compile.h"
#include "vm.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>


/*
static int symbol_equal(OBJ s1,OBJ s2)
{
	return strcmp(obj_symbol_data(s1),obj_symbol_data(s2))==0;
}

static int is_quoted(OBJ expression) 
{
	return is_tagged_list(expression,obj_make_symbol("quote"));
}

static OBJ text_of_quotation(OBJ exp) 
{
	return cadr(exp);
}

char is_assignment(OBJ exp) 
{
	return is_tagged_list(exp, obj_make_symbol("set!"));
}

OBJ assignment_variable(OBJ exp) 
{
	return cadr(exp);
}

OBJ assignment_value(OBJ exp) 
{
	return car(cdr(cdr(exp)));
}

OBJ eval_assignment(OBJ exp, OBJ env)
{
	set_variable_value(assignment_variable(exp), 
			   eval(assignment_value(exp), env),
			   env);
	return obj_make_symbol("ok");
}



OBJ make_lambda(OBJ parameters, OBJ body) 
{
	return cons(obj_make_symbol("lambda"), cons(parameters, body));
}

OBJ definition_value(OBJ exp) 
{
	if (obj_symbolp(cadr(exp))) 
	{
		return caddr(exp);
	}
	else 
	{
		return make_lambda(cdadr(exp), cddr(exp));
	}
}

OBJ definition_variable(OBJ exp) 
{
	if (obj_symbolp(cadr(exp))) 
	{
		return cadr(exp);
	}
	else 
	{
		return caadr(exp);
	}
}

OBJ eval_definition(OBJ exp, OBJ env) 
{
	define(definition_variable(exp), 
	       eval(definition_value(exp), env),env);
	return obj_make_symbol("ok");
}

static int is_if(OBJ expression) 
{
	return is_tagged_list(expression, obj_make_symbol("if"));
}

OBJ if_predicate(OBJ exp) 
{
	return cadr(exp);
}

OBJ if_consequent(OBJ exp) 
{
	return caddr(exp);
}

OBJ if_alternative(OBJ exp) 
{
	if (nullp(cdddr(exp))) 
	{
		return OBJ_FALSE;
	}
	else 
	{
		return cadddr(exp);
	}
}

static int is_lambda(OBJ exp) 
{
	return is_tagged_list(exp, obj_make_symbol("lambda"));
}

OBJ lambda_parameters(OBJ exp) 
{
	return cadr(exp);
}

OBJ lambda_body(OBJ exp) 
{
	return cddr(exp);
}

OBJ make_compound_proc(OBJ parameters, OBJ body, OBJ env) 
{
    OBJ obj;
    
    obj = obj_alloc_type(procedure,OBJ_PROCEDURE);
    obj_procedure_para(obj) = parameters;
    obj_procedure_body(obj) = body;
    obj_procedure_env(obj) = env;
    return obj;
}

static int is_begin(OBJ exp) 
{
	return is_tagged_list(exp, obj_make_symbol("begin"));
}

OBJ begin_actions(OBJ exp) 
{
	return cdr(exp);
}

static int is_last_exp(OBJ seq) 
{
	return nullp(cdr(seq));
}

OBJ first_exp(OBJ seq) 
{
	return car(seq);
}

OBJ rest_exps(OBJ seq) 
{
	return cdr(seq);
}

OBJ cond_predicate(OBJ clause) 
{
	return car(clause);
}

static int is_cond_else_clause(OBJ clause) 
{
	return cond_predicate(clause) == obj_make_symbol("else");
}

static int is_cond(OBJ exp) 
{
	return is_tagged_list(exp, obj_make_symbol("cond"));
}

OBJ cond_clauses(OBJ exp) 
{
	return cdr(exp);
}

OBJ cond_actions(OBJ clause) 
{
	return cdr(clause);
}

OBJ make_if(OBJ predicate, OBJ consequent, OBJ alternative) 
{
	OBJ tmp1,tmp2,tmp3,ret,if_symbol;
	tmp1 = cons(alternative,OBJ_NULL);
	tmp2 = cons(consequent,tmp1);
	tmp3 = cons(predicate,tmp2);
	if_symbol = obj_make_symbol("if");
	ret = cons(if_symbol,tmp3);
	return ret;
}

OBJ make_begin(OBJ seq) 
{
	return cons(obj_make_symbol("begin"), seq);
} 
OBJ sequence_to_exp(OBJ seq) 
{
	if (nullp(seq)) 
	{
		return seq;
	}
	else if (is_last_exp(seq)) 
	{
		return first_exp(seq);
	}
	else 
	{
		return make_begin(seq);
	}
}

OBJ expand_clauses(OBJ clauses) 
{
	OBJ first;
	OBJ rest;
    
	if (nullp(clauses))
	{
		return OBJ_FALSE;
	}
	else
	{
		first = car(clauses);
		rest  = cdr(clauses);
		if (is_cond_else_clause(first)) 
		{
			if (nullp(rest)) 
			{
				return sequence_to_exp(cond_actions(first));
			}
			else {
				fprintf(stderr, "else clause isn't last cond->if");
				exit(1);
			}
		}
		else {
			return make_if(cond_predicate(first),
				       sequence_to_exp(cond_actions(first)),
				       expand_clauses(rest));
		}
	}
}

OBJ cond_to_if(OBJ exp) 
{
	return expand_clauses(cond_clauses(exp));
}

OBJ make_application(OBJ operator, OBJ operands)
{
	return cons(operator, operands);
}

static int is_let(OBJ exp) 
{
	return is_tagged_list(exp, obj_make_symbol("let"));
}

OBJ let_bindings(OBJ exp) 
{
	return cadr(exp);
}

OBJ binding_argument(OBJ binding) 
{
	return cadr(binding);
}

OBJ binding_parameter(OBJ binding) 
{
	return car(binding);
}



OBJ bindings_parameters(OBJ bindings) 
{
	return nullp(bindings) ?
		OBJ_NULL :
		cons(binding_parameter(car(bindings)),
		     bindings_parameters(cdr(bindings)));
}

OBJ bindings_arguments(OBJ bindings) 
{
	return nullp(bindings) ?
		OBJ_NULL :
		cons(binding_argument(car(bindings)),
		     bindings_arguments(cdr(bindings)));
}

OBJ let_parameters(OBJ exp) 
{
	return bindings_parameters(let_bindings(exp));
}

OBJ let_arguments(OBJ exp) 
{
	return bindings_arguments(let_bindings(exp));
}

OBJ let_body(OBJ exp) 
{
	return cddr(exp);
}

OBJ let_to_application(OBJ exp) 
{
    return make_application(
               make_lambda(let_parameters(exp),
                           let_body(exp)),
               let_arguments(exp));
}

static int is_application(OBJ exp) 
{
	return obj_pairp(exp);
}

OBJ operator(OBJ exp) 
{
	return car(exp);
}

OBJ operands(OBJ exp) 
{
	return cdr(exp);
}

static int is_no_operands(OBJ ops) 
{
	return nullp(ops);
}

OBJ first_operand(OBJ ops) 
{
	return car(ops);
}

OBJ rest_operands(OBJ ops) {
	return cdr(ops);
}

OBJ list_of_values(OBJ exps, OBJ env) 
{
	if (is_no_operands(exps)) 
	{
		return OBJ_NULL;
	}
	else 
	{
		return cons(eval(first_operand(exps), env),
			    list_of_values(rest_operands(exps), env));
	}
}
*/
/*








object *let_to_application(object *exp) {
	return make_application(
		make_lambda(let_parameters(exp),
			    let_body(exp)),
		let_arguments(exp));
}

char is_and(object *exp) {
	return is_tagged_list(exp, and_symbol);
}

object *and_tests(object *exp) {
	return cdr(exp);
}

char is_or(object *exp) {
	return is_tagged_list(exp, or_symbol);
}

object *or_tests(object *exp) {
	return cdr(exp);
}

object *apply_operator(object *arguments) {
	return car(arguments);
}

object *prepare_apply_operands(object *arguments) {
	if (is_the_empty_list(cdr(arguments))) {
		return car(arguments);
	}
	else {
		return cons(car(arguments),
			    prepare_apply_operands(cdr(arguments)));
	}
}

object *apply_operands(object *arguments) {
	return prepare_apply_operands(cdr(arguments));
}

object *eval_expression(object *arguments) {
	return car(arguments);
}

object *eval_environment(object *arguments) {
	return cadr(arguments);
}




*/

OBJ eval(OBJ sexp, OBJ env) 
{
	char *bytecode;
	OBJ ret;

	bytecode = compile(sexp,env);
	if(bytecode == NULL)
	{
		fprintf(stderr,"compile error");
		return OBJ_NULL;
	}
//	print_bytecode(bytecode);
//	ret = OBJ_NULL;
	ret = vm(bytecode);
	free(bytecode);
	/* gc(env); */

	return ret;
/*
	OBJ procedure;
	OBJ arguments;
	OBJ result;

tailcall:
	if (is_self_evaluating(exp)) 
	{
		return exp;
	}
	else if (is_variable(exp)) 
	{
		return lookup_variable_value(exp, env);
	}
	else if (is_quoted(exp)) {
		return text_of_quotation(exp);
	}
;mae	else if (is_assignment(exp)) 
	{
		return eval_assignment(exp, env);
	}
	else if (is_definition(exp)) 
	{
		return eval_definition(exp, env);
	}
	else if (is_if(exp)) 
	{
		exp = (OBJ_TRUE == eval(if_predicate(exp), env)) ?
			if_consequent(exp) :
			if_alternative(exp);
		goto tailcall;
	}
	else if (is_lambda(exp)) 
	{
		return make_compound_proc(lambda_parameters(exp),
					  lambda_body(exp),
					  env);
	}
	else if (is_begin(exp)) 
	{
		exp = begin_actions(exp);
		while (!is_last_exp(exp)) 
		{
			eval(first_exp(exp), env);
			exp = rest_exps(exp);
		}
		exp = first_exp(exp);
		goto tailcall;
	}
	else if (is_cond(exp)) 
	{
		exp = cond_to_if(exp);
		goto tailcall;
	}
	else if (is_let(exp)) 
	{
		exp = let_to_application(exp);
		goto tailcall;
	}
	else if (is_and(exp)) {
		exp = and_tests(exp);
		if (is_the_empty_list(exp)) {
			return true;
		}
		while (!is_last_exp(exp)) {
			result = eval(first_exp(exp), env);
			if (is_false(result)) {
				return result;
			}
			exp = rest_exps(exp);
		}
		exp = first_exp(exp);
		goto tailcall;
	}
	else if (is_or(exp)) {
		exp = or_tests(exp);
		if (is_the_empty_list(exp)) {
			return false;
		}
		while (!is_last_exp(exp)) {
			result = eval(first_exp(exp), env);
			if (is_true(result)) {
				return result;
			}
			exp = rest_exps(exp);
		}
		exp = first_exp(exp);
		goto tailcall;
	}
	else if (is_application(exp)) 
	{
		procedure = eval(operator(exp), env);
		arguments = list_of_values(operands(exp), env);


		if (obj_primitivep(procedure))
		{
			return (obj_primitive_data(procedure))(arguments);
		}
		else if (obj_procedurep(procedure)) 
		{
			env = extend_environment(
				obj_procedure_para(procedure),
				arguments,
				obj_procedure_env(procedure));
			exp = make_begin(obj_procedure_body(procedure));
			
			result = eval(exp,env);
			
			return result;
			goto tailcall;
		}
		else {
			fprintf(stderr, "unknown procedure type\n");
			exit(1);
		}
	}
	else {
		fprintf(stderr, "cannot eval unknown expression type\n");
		exit(1);
	}
	fprintf(stderr, "eval illegal state\n");
	exit(1);
*/
}
