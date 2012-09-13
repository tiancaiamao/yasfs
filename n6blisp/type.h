#ifndef TYPE_H
#define TYPE_H

#include <stddef.h>


/* tagging system
 *   bits end in    00:  pointer
 *                  01:  fixnum
 *                 011:  immediate flonum (optional)
 *                 111:  immediate symbol (optional)
 *              000110:  char
 *              001110:  unique immediate (NULL, TRUE, FALSE)
 */

#define OBJ_FIXNUM_BITS 2
#define OBJ_IMMEDIATE_BITS 3
#define OBJ_EXTENDED_BITS 6

#define OBJ_FIXNUM_MASK 3
#define OBJ_IMMEDIATE_MASK 7
#define OBJ_EXTENDED_MASK 63

#define OBJ_POINTER_TAG 0
#define OBJ_FIXNUM_TAG 1
#define OBJ_ISYMBOL_TAG 7
#define OBJ_IFLONUM_TAG 3
#define OBJ_CHAR_TAG 6
#define OBJ_EXTENDED_TAG 14

enum obj_types {
	SEXP_OBJECT,
	SEXP_TYPE,
	OBJ_FIXNUM,
	OBJ_NUMBER,
	OBJ_CHAR,
	OBJ_BOOLEAN,
	OBJ_PAIR,
	OBJ_SYMBOL,
	SEXP_BYTES,
	OBJ_STRING,
	OBJ_VECTOR,
	OBJ_FLONUM,
	OBJ_PRIMITIVE,
	OBJ_PROCEDURE,
	OBJ_SYNTAX,
	OBJ_PROT,
	SEXP_BIGNUM,
	SEXP_IPORT,
	SEXP_OPORT,
	SEXP_EXCEPTION,
	SEXP_PROCEDURE,
	SEXP_MACRO,
	SEXP_SYNCLO,
	OBJ_ENV,
	SEXP_BYTECODE,
	OBJ_CORE,
	OBJ_AST_DEFINE,
	OBJ_AST_IF,
	OBJ_AST_BEGIN,
	OBJ_AST_QUOTE,
	OBJ_AST_LAMBDA,
	OBJ_AST_APP,
	SEXP_OPCODE,
	SEXP_CND,
	SEXP_REF,
	SEXP_SET,
	SEXP_SEQ,
	SEXP_LIT,
	SEXP_STACK,
	SEXP_CONTEXT,
	SEXP_CPOINTER,
	SEXP_PROMISE,
	SEXP_NUM_CORE_TYPES
};

enum core
{
	DEFINE,
	IF,
	LAMBDA,
	QUOTE,
	SET,
	BEGIN,
	SYNTAX_RULES,
	DEFINE_SYNTAX,
};

enum primitive_type
{
	IMMEDIATE,
	DATA,
	FUNCALL,
};

typedef struct object* OBJ;

struct object 
{
	unsigned short tag;
	char markedp;
	unsigned int immutablep:1;
	unsigned int freep:1;
	unsigned int brokenp:1;
	unsigned int syntacticp:1;
	union 
	{
		/* basic types */
		double flonum;
		struct 
		{
			OBJ car;
			OBJ cdr;
		} pair;
		struct 
		{
			unsigned length;
			OBJ data[];
		} vector;
		struct 
		{
			unsigned length;
			char data[];
		} string;
		struct 
		{
			unsigned length;
			char data[];
		} symbol;
		struct 
		{
			signed char sign;
			unsigned length;
			unsigned data[];
		} bignum;
		struct
		{
		} port;
		struct 
		{
			char type;
			char *name;
			char opcode;
			unsigned int num;
			OBJ data;
			OBJ proc;
		} primitive;
		struct 
		{
			OBJ env;
			OBJ formals;
			OBJ code;
		} procedure;
                /* inner-use types */
		struct 
		{
			char type;
			char *name;
		} core;
		/* runtime types */
		struct 
		{
			OBJ parent, bindings;
		} env;
		struct
		{
			OBJ env;
			OBJ data;
		}syntax;
		/* ast types */
		struct 
		{
			OBJ cell;
			OBJ ast;
		} define;
		struct 
		{
			OBJ data;
		} quote;
		struct
		{
			OBJ test;
			OBJ consequent;
			OBJ alternate;
		} ast_if;
		struct 
		{
			OBJ data;
		} begin;
		struct
		{
			OBJ env;
			OBJ formals;
			OBJ body;
		} lambda;
		struct 
		{
			int type; /* is it primitive? */
			OBJ params;
			OBJ data;
			int tail; /* is it a tail call when type is procedure */
		} app;
		/* compiler state */
		struct 
		{
			int donep;
			OBJ thunk, value;
		} promise;
	} value;
};

#define OBJ_MAKE_IMMEDIATE(n)  ((OBJ) ((n<<OBJ_EXTENDED_BITS)	\
				       + OBJ_EXTENDED_TAG))

#define OBJ_FALSE  OBJ_MAKE_IMMEDIATE(0) /* 14 0x0e */
#define OBJ_TRUE   OBJ_MAKE_IMMEDIATE(1) /* 30 0x4e */
#define OBJ_NULL   OBJ_MAKE_IMMEDIATE(2) /* 46 0x8e */
#define OBJ_EOF    OBJ_MAKE_IMMEDIATE(3) /* 62 0xce */
#define OBJ_VOID   OBJ_MAKE_IMMEDIATE(4) /* the unspecified value */

#define obj_pointer_tag(obj) ((obj)->tag)

/********************** predicates *******************/
#define obj_pointerp(obj) (((unsigned long)(obj) & OBJ_FIXNUM_MASK) == OBJ_POINTER_TAG)
#define obj_charp(obj)  (((unsigned long)(obj) & OBJ_EXTENDED_MASK) == OBJ_CHAR_TAG)
#define obj_fixnump(obj) (((unsigned long)(obj) & OBJ_FIXNUM_MASK) == OBJ_FIXNUM_TAG)
#define obj_integerp(obj) obj_fixnump(obj)

#define obj_check_tag(obj,tag) ((obj_pointerp(obj)) && (obj_pointer_tag(obj) == tag))
#define obj_booleanp(obj) (((obj) == OBJ_TRUE) || ((obj) == OBJ_FALSE))
#define obj_flonump(obj) (obj_check_tag(obj , OBJ_FLONUM))
#define obj_numberp(obj) (obj_fixnump(obj) || obj_flonump(obj))
#define obj_stringp(obj) (obj_check_tag(obj,OBJ_STRING))
#define obj_symbolp(obj) (obj_check_tag(obj,OBJ_SYMBOL))
#define obj_pairp(obj)   (obj_check_tag(obj,OBJ_PAIR))
#define obj_vectorp(obj) (obj_check_tag(obj,OBJ_VECTOR))
#define obj_corep(obj)   (obj_check_tag(obj,OBJ_CORE))
#define obj_primitivep(obj) (obj_check_tag(obj,OBJ_PRIMITIVE))
#define obj_syntaxp(obj) (obj_check_tag(obj,OBJ_SYNTAX))
#define obj_definep(obj) (obj_check_tag(obj,OBJ_AST_DEFINE))
#define obj_procedurep(obj) (obj_check_tag(obj,OBJ_PROCEDURE))
#define obj_markedp(obj) (obj_pointerp((obj)) && (obj)->markedp == 1)
#define nullp(obj) ((obj) == OBJ_NULL)
#define voidp(obj) ((obj) == OBJ_VOID)

/********************** constructors ******************/
OBJ obj_alloc_tagged(unsigned size,unsigned tag);

#define obj_make_boolean(x) ((x) ? OBJ_TRUE : OBJ_FALSE)
#define obj_unbox_boolean(x) (((x) == OBJ_FALSE) ? 0 : 1)

#define obj_make_integer(n)    ((OBJ) ((((long)n)<<OBJ_FIXNUM_BITS) + OBJ_FIXNUM_TAG))
#define obj_unbox_integer(n)   (((long)n)>>OBJ_FIXNUM_BITS)

#define obj_make_character(n)  ((OBJ) ((((long)n)<<OBJ_EXTENDED_BITS) + OBJ_CHAR_TAG))
#define obj_unbox_character(n) ((char) (((long)n)>>OBJ_EXTENDED_BITS))

#define obj_integer_to_flonum(x) (obj_make_flonum(obj_unbox_integer(x)))

#define obj_sizeof(type) (offsetof(struct object,value)			\
			  + sizeof(((struct object*)0)->value.type))
#define obj_alloc_type(type,tag) obj_alloc_tagged(obj_sizeof(type),tag)
OBJ obj_make_env(OBJ parent,OBJ bindings);
OBJ obj_make_flonum(double f);
OBJ obj_make_string(OBJ len,OBJ ch);
OBJ obj_make_string_1(const char *);
OBJ obj_make_string_2(unsigned);
OBJ obj_make_symbol(const char* str);
OBJ obj_make_vector();
OBJ obj_make_primitive(OBJ (*fn)(OBJ arg));
OBJ obj_make_procedure(OBJ env,OBJ code,OBJ formals);
OBJ obj_make_syntax(OBJ date,OBJ env);
OBJ obj_make_define(OBJ cell,OBJ ast);
OBJ obj_make_quote(OBJ data);
OBJ obj_make_if(OBJ test,OBJ consequent,OBJ alternate);
OBJ obj_make_begin(OBJ data);
OBJ obj_make_lambda(OBJ env,OBJ formals,OBJ body);
OBJ obj_make_app(char type,OBJ params,OBJ procedure,int tail);

/************************ accesser ********************/
#define obj_flonum_value(f) ((f)->value.flonum)
#define obj_number_data(n)  (obj_fixnump(n)?obj_unbox_integer(n):obj_flonum_value(n))
#define obj_string_length(s) ((s)->value.string.length)
#define obj_string_data(s) ((s)->value.string.data)
#define obj_vector_length(v) ((v)->value.vector.length)
#define obj_vector_ref(v,i) ((v)->value.vector.data[(i)])
#define obj_symbol_length(s) ((s)->value.symbol.length)
#define obj_symbol_data(s) ((s)->value.symbol.data)
#define obj_procedure_code(p) ((p)->value.procedure.code)
#define obj_procedure_env(p) ((p)->value.procedure.env)
#define obj_procedure_formals(p) ((p)->value.procedure.formals)
#define obj_syntax_data(s)  ((s)->value.syntax.data)
#define obj_syntax_env(s)   ((s)->value.syntax.env)
#define obj_define_cell(d) ((d)->value.define.cell)
#define obj_define_ast(d)  ((d)->value.define.ast)
#define obj_quote_data(o)  ((o)->value.quote.data)
#define obj_core_type(o)   ((o)->value.core.type)
#define obj_core_name(o)   ((o)->value.core.name)
#define obj_if_test(o)     ((o)->value.ast_if.test)
#define obj_if_consequent(o)  ((o)->value.ast_if.consequent)
#define obj_if_alternate(o)   ((o)->value.ast_if.alternate)
#define obj_begin_data(o)  ((o)->value.begin.data)
#define obj_lambda_env(o)  ((o)->value.lambda.env)
#define obj_lambda_body(o) ((o)->value.lambda.body)
#define obj_lambda_formals(o) ((o)->value.lambda.formals)
#define obj_app_params(o)  ((o)->value.app.params)
#define obj_app_data(o)  ((o)->value.app.data)
#define obj_app_type(o)  ((o)->value.app.type)
#define obj_app_tail(o)  ((o)->value.app.tail)
#define obj_primitive_opcode(o) ((o)->value.primitive.opcode)
#define obj_primitive_type(o)   ((o)->value.primitive.type)
#define obj_primitive_data(p) ((p)->value.primitive.data)
#define obj_primitive_proc(p) ((p)->value.primitive.proc)
#define obj_env_bindings(e)   ((e)->value.env.bindings)
#define obj_env_parent(e)     ((e)->value.env.parent)

/************************ utilities *******************/
#define car(x)       ((x)->value.pair.car)
#define cdr(x)       ((x)->value.pair.cdr)

#define caar(x)      (car(car(x)))
#define cadr(x)      (car(cdr(x)))
#define cdar(x)      (cdr(car(x)))
#define cddr(x)      (cdr(cdr(x)))
#define caaar(x)     (car(caar(x)))
#define caadr(x)     (car(cadr(x)))
#define cadar(x)     (car(cdar(x)))
#define caddr(x)     (car(cddr(x)))
#define cdaar(x)     (cdr(caar(x)))
#define cdadr(x)     (cdr(cadr(x)))
#define cddar(x)     (cdr(cdar(x)))
#define cdddr(x)     (cdr(cddr(x)))
#define cadddr(x)    (cadr(cddr(x)))
#define cddddr(x)    (cddr(cddr(x)))
OBJ cons(OBJ head,OBJ tail);
OBJ listp(OBJ lst);
OBJ assq(OBJ obj,OBJ list);
int eq(OBJ a,OBJ b);
OBJ add(OBJ a,OBJ b);
OBJ sub(OBJ a,OBJ b);
OBJ mul(OBJ a,OBJ b);
//OBJ div(OBJ a,OBJ b);
OBJ reverse(OBJ list);
#define obj_mark(obj) ((obj)->markedp = 1)
#define obj_unmark(obj) ((obj)->markedp = 0) 

#endif
