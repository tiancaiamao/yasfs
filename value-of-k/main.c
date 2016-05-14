#include "mpc.h"
#include <assert.h>

enum object_type {
  OBJ_PAIR = 0,
  OBJ_NUMBER,
  OBJ_CHARACTER,
  OBJ_VECTOR,
  OBJ_NULL,

  OBJ_FALSE,
  OBJ_TRUE,

  OBJ_STRING,
  OBJ_SYMBOL,
  OBJ_SYNTAX,

  OBJ_PRIMITIVE,
  OBJ_CONTINUATION,
  OBJ_PROCEDURE,
  OBJ_ERROR,
  OBJ_UNSPECIFIED,
  OBJ_UNINITED,
  OBJ_TYPE_MAX
};

enum syntax_type {
  SYNTAX_IF = 0,
  SYNTAX_BEGIN,
  SYNTAX_SET,
  SYNTAX_LAMBDA,
  SYNTAX_QUOTE,
  SYNTAX_DEFINE,
  SYNTAX_LET,
};

struct object_head {
  enum object_type type;
  char marked;
};

typedef struct object_head *object_t;

struct pair {
  struct object_head head;
  object_t car;
  object_t cdr;
};

struct syntax {
  struct object_head head;
  enum syntax_type type;
};

struct datnum {
  struct object_head head;
  unsigned int size;
  char data[1];
};

struct vector {
  struct object_head head;
  unsigned int size;
  object_t data[0];
};

struct number {
  struct object_head head;
  union {
    int fixnum;
    double flonum;
  } data;
};

struct character {
  struct object_head head;
  char data;
};

struct procedure {
  struct object_head head;
  object_t variables;
  object_t body;
  object_t env;
};

struct primitive {
  struct object_head head;
  char *name;
  unsigned int arg_num;
  union {
    object_t (*op0)();
    object_t (*op1)(object_t);
    object_t (*op2)(object_t, object_t);
    object_t (*op3)(object_t, object_t, object_t);
    object_t (*op4)(object_t, object_t, object_t, object_t);
    object_t (*op5)(object_t, object_t, object_t, object_t, object_t);
    object_t (*op6)(object_t, object_t, object_t, object_t, object_t,
                    object_t);
  } func;
};

enum cont_type {
  END_CONT,
  IF_TEST_CONT,
  BEGIN_CONT,
  SET_CONT,
  DEFINE_CONT,
  CALL_CONT,
  ARG_CONT,
  LET_CONT,
  PROCEDURE_CALL_CONT,
};

struct continuation {
  struct object_head head;
  enum cont_type type;
};

struct error {
  struct object_head head;
  char *info;
};

struct object_head scheme_null_object = {.type = OBJ_NULL};
struct object_head scheme_false_object = {.type = OBJ_FALSE};
struct object_head scheme_true_object = {.type = OBJ_TRUE};
struct object_head scheme_unspecified_object = {.type = OBJ_UNSPECIFIED};
struct object_head scheme_uninited_object = {.type = OBJ_UNINITED};
struct syntax syntax_if = {.head = {.type = OBJ_SYNTAX}, .type = SYNTAX_IF};
struct syntax syntax_begin = {.head = {.type = OBJ_SYNTAX}, .type = SYNTAX_BEGIN};
struct syntax syntax_set = {.head = {.type = OBJ_SYNTAX}, .type = SYNTAX_SET};
struct syntax syntax_define = {.head = {.type = OBJ_SYNTAX}, .type = SYNTAX_DEFINE};
struct syntax syntax_lambda = {.head = {.type = OBJ_SYNTAX}, .type = SYNTAX_LAMBDA};
struct syntax syntax_let = {.head = {.type = OBJ_SYNTAX}, .type = SYNTAX_LET};

object_t scheme_null = &scheme_null_object;
object_t scheme_false = &scheme_false_object;
object_t scheme_true = &scheme_true_object;
object_t scheme_unspecified = &scheme_unspecified_object;
object_t scheme_uninited = &scheme_uninited_object;

object_t eval(object_t exp, object_t env, object_t cont);
object_t apply(object_t fn, object_t list, object_t cont);
static void env_set(object_t env, object_t var, object_t val);
object_t env_get(object_t env, object_t var);
object_t env_extend(object_t env, object_t vars, object_t vals);
object_t apply_cont(object_t cont, object_t val);

struct vector *
make_vector(unsigned int size) {
  struct vector *ret =
    malloc(sizeof(struct vector) + size * sizeof(object_t));
  ret->head.type = OBJ_VECTOR;
  ret->size = size;
  return ret;
}

object_t
vector_ref(struct vector *v, unsigned int idx) {
  return v->data[idx];
}

object_t
vector_set(struct vector *v, unsigned int idx, object_t o) {
  v->data[idx] = o;
  return scheme_unspecified;
}

object_t
make_pair(object_t car, object_t cdr) {
  struct pair *ret = malloc(sizeof(struct pair));
  ret->head.type = OBJ_PAIR;
  ret->car = car;
  ret->cdr = cdr;
  return (object_t)ret;
}

object_t
make_symbol(char *data) {
  unsigned int size = strlen(data);
  struct datnum *ret = malloc(sizeof(struct datnum) + size);
  ret->head.type = OBJ_SYMBOL;
  ret->size = size;
  strcpy(ret->data, data);
  return (object_t)ret;
}

object_t
make_string(char *data) {
  unsigned int size = strlen(data);
  struct datnum *ret = malloc(sizeof(struct datnum) + size);
  ret->head.type = OBJ_STRING;
  ret->size = size;
  strcpy(ret->data, data);
  return (object_t)ret;
}

struct character *
make_character(char c) {
  struct character *ret = malloc(sizeof(struct character));
  ret->head.type = OBJ_CHARACTER;
  ret->data = c;
  return ret;
}

object_t
make_number(int n) {
  struct number *ret = malloc(sizeof(struct number));
  ret->head.type = OBJ_NUMBER;
  ret->data.fixnum = n;
  return (object_t)ret;
}

int
unbox(object_t o) {
  return ((struct number *)o)->data.fixnum;
}

object_t
make_procedure(object_t variables, object_t body, object_t env) {
  struct procedure *ret = malloc(sizeof(struct procedure));
  ret->head.type = OBJ_PROCEDURE;
  ret->body = body;
  ret->variables = variables;
  ret->env = env;
  return (object_t)ret;
}

object_t
make_primitive(char *name, int arg_num, void* func) {
  struct primitive *ret = malloc(sizeof(*ret));
  ret->head.type = OBJ_PRIMITIVE;
  ret->name = name;
  ret->arg_num = arg_num;
  ret->func.op1 = func;
  return (object_t)ret;
}

char *error_message[] = {
  "can't eval this type of obj", // 0
  "can't apply this type of obj", // 1
  "unbinded variable in env", // 2
  "can't cdr on a non-pair", // 3
  "bad if syntax", // 4
  "bad set syntax", // 5
  "can't use non-symbol object as name in set syntax", // 6
  "wrong begin syntax" // 7,
  "bad lambda syntax", // 8
  "eval_argument error", // 9
  "not enough arguments", // 10
  "too many arguments", // 11
  "wrong number of arguments for primitive", // 12
  "exceed maxium of length in list-ref ", // 13
  "can't use add on non-number", // 14
  "can't reverse non-list", // 15
  "call/cc must receive a procedure", // 16
};

object_t
sys_error(unsigned int num) {
  struct error *ret;
  ret = malloc(sizeof(struct error));
  ret->head.type = OBJ_ERROR;
  ret->info = error_message[num];
  return (object_t)ret;
}

object_t eqv_op(object_t, object_t);
object_t assv_op(object_t, object_t);
object_t cons_op(object_t, object_t);

object_t
cons_op(object_t car, object_t cdr) {
  return make_pair(car, cdr);
}

object_t
pair_op(object_t o) {
  if (o->type != OBJ_PAIR) return scheme_false;
  return scheme_true;
}

object_t
car_op(object_t p) {
  if (p->type != OBJ_PAIR) {
    return sys_error(3);
  }
  return ((struct pair *)p)->car;
}

object_t
cdr_op(object_t p) {
  if (p->type != OBJ_PAIR) {
    return sys_error(3);
  }
  return ((struct pair *)p)->cdr;
}
object_t
caar_op(object_t p) {
  return car_op(car_op(p));
}
object_t
cddr_op(object_t p) {
  return cdr_op(cdr_op(p));
}
object_t
cadr_op(object_t p) {
  return car_op(cdr_op(p));
}
object_t
caddr_op(object_t p) {
  return car_op(cddr_op(p));
}
object_t
cdddr_op(object_t p) {
  return cdr_op(cddr_op(p));
}
object_t
cadddr_op(object_t p) {
  return car_op(cdddr_op(p));
}
object_t
set_cdr_op(object_t o, object_t value) {
  if (o->type != OBJ_PAIR) {
    return sys_error(3);
  }
  ((struct pair *)o)->cdr = value;
  return scheme_unspecified;
}
object_t
eqv_op(object_t a, object_t b) {
  if (a->type == OBJ_SYMBOL && b->type == OBJ_SYMBOL) {
    if (strcmp(((struct datnum *)a)->data, ((struct datnum *)b)->data) == 0)
      return scheme_true;
    else
      return scheme_false;
  } else if (a->type == OBJ_NUMBER && b->type == OBJ_NUMBER) {
    return ((struct number *)a)->data.fixnum ==
      ((struct number *)b)->data.fixnum
      ? scheme_true
      : scheme_false;
  }
  return a == b ? scheme_true : scheme_false;
}
object_t
eq_op(object_t a, object_t b) {
  if (a->type == OBJ_NUMBER && b->type == OBJ_NUMBER) {
    return ((struct number *)a)->data.fixnum ==
      ((struct number *)b)->data.fixnum
      ? scheme_true
      : scheme_false;
  }
  return a == b ? scheme_true : scheme_false;
}
object_t
add_op(object_t a, object_t b) {
  if (a->type != OBJ_NUMBER || b->type != OBJ_NUMBER) return sys_error(14);
  return make_number(((struct number *)a)->data.fixnum +
                     ((struct number *)b)->data.fixnum);
}
object_t
sub_op(object_t a, object_t b) {
  if (a->type != OBJ_NUMBER || b->type != OBJ_NUMBER) return sys_error(14);
  return make_number(((struct number *)a)->data.fixnum -
                     ((struct number *)b)->data.fixnum);
}
object_t
mul_op(object_t a, object_t b) {
  if (a->type != OBJ_NUMBER || b->type != OBJ_NUMBER) return sys_error(14);
  return make_number(((struct number *)a)->data.fixnum *
                     ((struct number *)b)->data.fixnum);
}
object_t
div_op(object_t a, object_t b) {
  if (a->type != OBJ_NUMBER || b->type != OBJ_NUMBER) return sys_error(14);
  return make_number(((struct number *)a)->data.fixnum /
                     ((struct number *)b)->data.fixnum);
}
unsigned int
length(object_t lst) {
  unsigned int i = 0;
  while (lst->type == OBJ_PAIR) {
    lst = ((struct pair *)lst)->cdr;
    i++;
  }
  return i;
}
object_t
length_op(object_t lst) {
  int i = length(lst);
  return make_number(i);
}
object_t
assv_op(object_t obj, object_t list) {
  object_t tmp;

  while (list->type == OBJ_PAIR) {
    tmp = car_op(list);
    if (tmp->type == OBJ_PAIR && eqv_op(obj, car_op(tmp)) != scheme_false)
      return tmp;
    list = ((struct pair *)list)->cdr;
  }
  return scheme_false;
}
object_t
reverse_op(object_t lst) {
  object_t ret = scheme_null;
  while (lst != scheme_null) {
    ret = cons_op(car_op(lst), ret);
    lst = cdr_op(lst);
  }
  return ret;
}
object_t
list_ref(object_t lst, unsigned int i) {
  object_t ret;
  while (i > 0 && lst->type == OBJ_PAIR) {
    ret = lst;
    lst = ((struct pair *)lst)->cdr;
    i--;
  }
  if (lst->type != OBJ_PAIR)
    return sys_error(13);
  if (lst->type == OBJ_PAIR && i == 0)
    return ((struct pair *)lst)->car;
  return scheme_null;
}

object_t
map_op(object_t (*func)(object_t), object_t l) {
  object_t ret = scheme_null;
  for(; l!=scheme_null; l=cdr_op(l)) {
    ret = cons_op(func(car_op(l)), ret);
  }
  return reverse_op(ret);
}

void debug_object(object_t o);

void
debug_pair(object_t o, int begin) {
  if (begin) {
    printf("(");
  }
  if (scheme_null == cdr_op(o)) {
    debug_object(car_op(o));
    printf(")");
    return;
  }
  debug_object(car_op(o));
  if (cdr_op(o)->type != OBJ_PAIR) {
    printf(" . ");
    debug_object(cdr_op(o));
    printf(")");
  } else {
    printf(" ");
    debug_pair(cdr_op(o), 0);
  }
}

void
debug_object(object_t o) {
  switch(o->type) {
  case OBJ_UNSPECIFIED:
    break;
  case OBJ_NUMBER:
    printf("%d", ((struct number*)o)->data.fixnum);
    break;
  case OBJ_PAIR:
    debug_pair(o, 1);
    break;
  case OBJ_NULL:
    printf("()");
    break;
  case OBJ_STRING:
    printf("\"%s\"", ((struct datnum*)o)->data);
    break;
  case OBJ_SYMBOL:
    printf("%s", ((struct datnum*)o)->data);
    break;
  case OBJ_TRUE:
    printf("true");
    break;
  case OBJ_FALSE:
    printf("false");
    break;
  case OBJ_ERROR:
    printf("error: %s", ((struct error*)o)->info);
    break;
  case OBJ_SYNTAX:
    printf("|syntax|");
    break;
  case OBJ_PRIMITIVE:
    printf("<prim:%s>", ((struct primitive*)o)->name);
    break;
  case OBJ_PROCEDURE:
    printf("<proc:>");
    debug_object(((struct procedure*)o)->variables);
    debug_object(((struct procedure*)o)->body);
    break;
  default:
    printf("unprintable object type: %d\n", o->type);
  }
  fflush(stdout);
}

static char buffer[2048];

char *
readline(char *prompt) {
  fputs(prompt, stdout);
  fgets(buffer, 2048, stdin);
  char *cpy = malloc(strlen(buffer) + 1);
  strcpy(cpy, buffer);
  cpy[strlen(cpy) - 1] = '\0';
  return cpy;
}

/* Parser Declariations */
mpc_parser_t *Number;
mpc_parser_t *Symbol;
mpc_parser_t *String;
mpc_parser_t *Comment;
mpc_parser_t *Sexpr;
mpc_parser_t *Qexpr;
mpc_parser_t *Expr;
mpc_parser_t *Lispy;

/* Forward Declarations */
struct lval;
struct lenv;
typedef struct lval lval;
typedef struct lenv lenv;

/* Lisp Value */
enum {
  LVAL_ERR,
  LVAL_NUM,
  LVAL_SYM,
  LVAL_STR,
  LVAL_FUN,
  LVAL_SEXPR,
};

typedef lval *(*lbuiltin)(lenv *, lval *);

struct lval {
  int type;

  /* Basic */
  long num;
  char *err;
  char *sym;
  char *str;

  /* Function */
  lbuiltin builtin;
  lenv *env;
  lval *formals;
  lval *body;

  /* Expression */
  int count;
  lval **cell;
};

lval *
lval_num(long x) {
  lval *v = malloc(sizeof(lval));
  v->type = LVAL_NUM;
  v->num = x;
  return v;
}

lval *
lval_err(char *fmt, ...) {
  lval *v = malloc(sizeof(lval));
  v->type = LVAL_ERR;
  va_list va;
  va_start(va, fmt);
  v->err = malloc(512);
  vsnprintf(v->err, 511, fmt, va);
  v->err = realloc(v->err, strlen(v->err) + 1);
  va_end(va);
  return v;
}

lval *
lval_sym(char *s) {
  lval *v = malloc(sizeof(lval));
  v->type = LVAL_SYM;
  v->sym = malloc(strlen(s) + 1);
  strcpy(v->sym, s);
  return v;
}

lval *
lval_str(char *s) {
  lval *v = malloc(sizeof(lval));
  v->type = LVAL_STR;
  v->str = malloc(strlen(s) + 1);
  strcpy(v->str, s);
  return v;
}

lval *
lval_builtin(lbuiltin func) {
  lval *v = malloc(sizeof(lval));
  v->type = LVAL_FUN;
  v->builtin = func;
  return v;
}

lval *
lval_lambda(lval *formals, lval *body) {
  lval *v = malloc(sizeof(lval));
  v->type = LVAL_FUN;
  v->builtin = NULL;
  v->formals = formals;
  v->body = body;
  return v;
}

lval *
lval_sexpr(void) {
  lval *v = malloc(sizeof(lval));
  v->type = LVAL_SEXPR;
  v->count = 0;
  v->cell = NULL;
  return v;
}

void lenv_del(lenv *e);

void
lval_del(lval *v) {
  switch (v->type) {
  case LVAL_NUM:
    break;
  case LVAL_FUN:
    if (!v->builtin) {
      lenv_del(v->env);
      lval_del(v->formals);
      lval_del(v->body);
    }
    break;
  case LVAL_ERR:
    free(v->err);
    break;
  case LVAL_SYM:
    free(v->sym);
    break;
  case LVAL_STR:
    free(v->str);
    break;
  case LVAL_SEXPR:
    for (int i = 0; i < v->count; i++) {
      lval_del(v->cell[i]);
    }
    free(v->cell);
    break;
  }

  free(v);
}

lenv *lenv_copy(lenv *e);

lval *
lval_copy(lval *v) {
  lval *x = malloc(sizeof(lval));
  x->type = v->type;
  switch (v->type) {
  case LVAL_FUN:
    if (v->builtin) {
      x->builtin = v->builtin;
    } else {
      x->builtin = NULL;
      x->env = lenv_copy(v->env);
      x->formals = lval_copy(v->formals);
      x->body = lval_copy(v->body);
    }
    break;
  case LVAL_NUM:
    x->num = v->num;
    break;
  case LVAL_ERR:
    x->err = malloc(strlen(v->err) + 1);
    strcpy(x->err, v->err);
    break;
  case LVAL_SYM:
    x->sym = malloc(strlen(v->sym) + 1);
    strcpy(x->sym, v->sym);
    break;
  case LVAL_STR:
    x->str = malloc(strlen(v->str) + 1);
    strcpy(x->str, v->str);
    break;
  case LVAL_SEXPR:
    x->count = v->count;
    x->cell = malloc(sizeof(lval *) * x->count);
    for (int i = 0; i < x->count; i++) {
      x->cell[i] = lval_copy(v->cell[i]);
    }
    break;
  }
  return x;
}

lval *
lval_add(lval *v, lval *x) {
  v->count++;
  v->cell = realloc(v->cell, sizeof(lval *) * v->count);
  v->cell[v->count - 1] = x;
  return v;
}

lval *
lval_join(lval *x, lval *y) {
  for (int i = 0; i < y->count; i++) {
    x = lval_add(x, y->cell[i]);
  }
  free(y->cell);
  free(y);
  return x;
}

lval *
lval_pop(lval *v, int i) {
  lval *x = v->cell[i];
  memmove(&v->cell[i], &v->cell[i + 1], sizeof(lval *) * (v->count - i - 1));
  v->count--;
  v->cell = realloc(v->cell, sizeof(lval *) * v->count);
  return x;
}

lval *
lval_take(lval *v, int i) {
  lval *x = lval_pop(v, i);
  lval_del(v);
  return x;
}

void lval_print(lval *v);

void
lval_print_expr(lval *v, char open, char close) {
  putchar(open);
  for (int i = 0; i < v->count; i++) {
    lval_print(v->cell[i]);
    if (i != (v->count - 1)) {
      putchar(' ');
    }
  }
  putchar(close);
}

void
lval_print_str(lval *v) {
  /* Make a Copy of the string */
  char *escaped = malloc(strlen(v->str) + 1);
  strcpy(escaped, v->str);
  /* Pass it through the escape function */
  escaped = mpcf_escape(escaped);
  /* Print it between " characters */
  printf("\"%s\"", escaped);
  /* free the copied string */
  free(escaped);
}

void
lval_print(lval *v) {
  switch (v->type) {
  case LVAL_FUN:
    if (v->builtin) {
      printf("<builtin>");
    } else {
      printf("#FUN#(\\ ");
      lval_print(v->formals);
      putchar(' ');
      lval_print(v->body);
      putchar(')');
    }
    break;
  case LVAL_NUM:
    printf("#NUM#%li", v->num);
    break;
  case LVAL_ERR:
    printf("#ERR#: %s", v->err);
    break;
  case LVAL_SYM:
    printf("#SYM#%s", v->sym);
    break;
  case LVAL_STR:
    lval_print_str(v);
    break;
  case LVAL_SEXPR:
    lval_print_expr(v, '{', ')');
    break;
  }
}

void
lval_println(lval *v) {
  lval_print(v);
  putchar('\n');
}

int
lval_eq(lval *x, lval *y) {
  if (x->type != y->type) {
    return 0;
  }

  switch (x->type) {
  case LVAL_NUM:
    return (x->num == y->num);
  case LVAL_ERR:
    return (strcmp(x->err, y->err) == 0);
  case LVAL_SYM:
    return (strcmp(x->sym, y->sym) == 0);
  case LVAL_STR:
    return (strcmp(x->str, y->str) == 0);
  case LVAL_FUN:
    if (x->builtin || y->builtin) {
      return x->builtin == y->builtin;
    } else {
      return lval_eq(x->formals, y->formals) && lval_eq(x->body, y->body);
    }
  case LVAL_SEXPR:
    if (x->count != y->count) {
      return 0;
    }
    for (int i = 0; i < x->count; i++) {
      if (!lval_eq(x->cell[i], y->cell[i])) {
        return 0;
      }
    }
    return 1;
    break;
  }
  return 0;
}

char *
ltype_name(int t) {
  switch (t) {
  case LVAL_FUN:
    return "Function";
  case LVAL_NUM:
    return "Number";
  case LVAL_ERR:
    return "Error";
  case LVAL_SYM:
    return "Symbol";
  case LVAL_STR:
    return "String";
  case LVAL_SEXPR:
    return "S-Expression";
  default:
    return "Unknown";
  }
}

/* Lisp Environment */

struct lenv {
  lenv *par;
  int count;
  char **syms;
  lval **vals;
};

lenv *
lenv_new(void) {
  lenv *e = malloc(sizeof(lenv));
  e->par = NULL;
  e->count = 0;
  e->syms = NULL;
  e->vals = NULL;
  return e;
}

void
lenv_del(lenv *e) {
  for (int i = 0; i < e->count; i++) {
    free(e->syms[i]);
    lval_del(e->vals[i]);
  }
  free(e->syms);
  free(e->vals);
  free(e);
}

lenv *
lenv_copy(lenv *e) {
  lenv *n = malloc(sizeof(lenv));
  n->par = e->par;
  n->count = e->count;
  n->syms = malloc(sizeof(char *) * n->count);
  n->vals = malloc(sizeof(lval *) * n->count);
  for (int i = 0; i < e->count; i++) {
    n->syms[i] = malloc(strlen(e->syms[i]) + 1);
    strcpy(n->syms[i], e->syms[i]);
    n->vals[i] = lval_copy(e->vals[i]);
  }
  return n;
}

/* Builtins */
#define LASSERT(args, cond, fmt, ...)           \
  if (!(cond)) {                                \
    lval *err = lval_err(fmt, ##__VA_ARGS__);   \
    lval_del(args);                             \
    return err;                                 \
  }

#define LASSERT_TYPE(func, args, index, expect)                         \
  LASSERT(args, args->cell[index]->type == expect,                      \
          "Function '%s' passed incorrect type for argument %i. Got %s, " \
          "Expected %s.",                                               \
          func, index, ltype_name(args->cell[index]->type),             \
          ltype_name(expect))

#define LASSERT_NUM(func, args, num)                                    \
  LASSERT(args, args->count == num, "Function '%s' passed incorrect number " \
          "of arguments. Got %i, Expected %i.",                         \
          func, args->count, num)

#define LASSERT_NOT_EMPTY(func, args, index)                        \
  LASSERT(args, args->cell[index]->count != 0,                      \
          "Function '%s' passed {} for argument %i.", func, index);

lval *lval_eval(lenv *e, lval *v);

lval *lval_read(mpc_ast_t *t);

lval *
lval_read_num(mpc_ast_t *t) {
  errno = 0;
  long x = strtol(t->contents, NULL, 10);
  return errno != ERANGE ? lval_num(x) : lval_err("Invalid Number.");
}

lval *
lval_read_str(mpc_ast_t *t) {
  /* Cut off the final quote character */
  t->contents[strlen(t->contents) - 1] = '\0';
  /* Copy the string missing out the first quote character */
  char *unescaped = malloc(strlen(t->contents + 1) + 1);
  strcpy(unescaped, t->contents + 1);
  /* Pass through the unescape function */
  unescaped = mpcf_unescape(unescaped);
  /* Construct a new lval using the string */
  lval *str = lval_str(unescaped);
  /* Free the string and return */
  free(unescaped);
  return str;
}

lval *
lval_read(mpc_ast_t *t) {
  if (strstr(t->tag, "number")) {
    return lval_read_num(t);
  }
  if (strstr(t->tag, "string")) {
    return lval_read_str(t);
  }
  if (strstr(t->tag, "symbol")) {
    return lval_sym(t->contents);
  }

  lval *x = NULL;
  if (strcmp(t->tag, ">") == 0) {
    x = lval_sexpr();
  }
  if (strstr(t->tag, "sexpr")) {
    x = lval_sexpr();
  }

  for (int i = 0; i < t->children_num; i++) {
    if (strcmp(t->children[i]->contents, "(") == 0) {
      continue;
    }
    if (strcmp(t->children[i]->contents, ")") == 0) {
      continue;
    }
    if (strcmp(t->children[i]->contents, "}") == 0) {
      continue;
    }
    if (strcmp(t->children[i]->contents, "{") == 0) {
      continue;
    }
    if (strcmp(t->children[i]->tag, "regex") == 0) {
      continue;
    }
    if (strstr(t->children[i]->tag, "comment")) {
      continue;
    }
    x = lval_add(x, lval_read(t->children[i]));
  }

  return x;
}

static object_t
ast_to_ir(lval *v) {
  switch(v->type) {
  case LVAL_SEXPR:
    {
      object_t tail = scheme_null;
      for (int i = v->count-1; i>=0; i--) {
        tail = cons_op(ast_to_ir(v->cell[i]), tail);
      }
      return tail;
    }
  case LVAL_ERR:
    return sys_error(0); // TODO
  case LVAL_NUM:
    return make_number(v->num);
  case LVAL_STR:
    return make_string(v->str);
  case LVAL_SYM:
    return make_symbol(v->sym);
  case LVAL_FUN:
    return NULL; // TODO
  }
  return NULL; // TODO
}

// poor man's closure
struct if_test_cont {
  struct object_head head;
  enum cont_type type;
  object_t exp2;
  object_t exp3;
  object_t env;
  object_t cont;
};

static object_t
__if_test_cont(struct if_test_cont* self, object_t val) {
  if (val != scheme_false) {
    return eval(self->exp2, self->env, self->cont);
  }
  return eval(self->exp3, self->env, self->cont);
}

object_t
make_if_test_cont(object_t exp2,
                  object_t exp3,
                  object_t env,
                  object_t cont) {
  struct if_test_cont *priv = malloc(sizeof(*priv));
  priv->head.type = OBJ_CONTINUATION;
  priv->type = IF_TEST_CONT;
  priv->exp2 = exp2;
  priv->exp3 = exp3;
  priv->env = env;
  priv->cont = cont;
  return (object_t)priv;
}

struct end_cont {
  struct object_head head;
  enum cont_type type;
};

struct end_cont __the_end_cont = {.head = {.type = OBJ_CONTINUATION}, .type = END_CONT};

object_t
make_end_cont() {
  return (object_t)&__the_end_cont;
}

object_t
__end_cont(object_t v) {
  return v;
}

struct begin_cont {
  struct object_head head;
  enum cont_type type;
  object_t exp;
  object_t env;
  object_t cont;
};

object_t
make_begin_cont(object_t exp, object_t env, object_t cont) {
  struct begin_cont *bc = malloc(sizeof(*bc));
  bc->head.type = OBJ_CONTINUATION;
  bc->type = BEGIN_CONT;
  bc->exp = exp;
  bc->env = env;
  bc->cont = cont;
  return (object_t)bc;
}

object_t
__begin_cont(struct begin_cont* bc, object_t val) {
  assert(bc->exp != scheme_null);
  if (scheme_null == cdr_op(bc->exp)) {
    return eval(car_op(bc->exp), bc->env, bc->cont);
  }
  object_t next_cc = make_begin_cont(cdr_op(bc->exp), bc->env, bc->cont);
  return eval(car_op(bc->exp), bc->env, next_cc);
}

struct set_cont {
  struct object_head head;
  enum cont_type type;
  object_t env;
  object_t cont;
  object_t var;
};

object_t
make_set_cont(object_t var, object_t env, object_t cont, enum cont_type type) {
  struct set_cont *ret = malloc(sizeof(*ret));
  ret->head.type = OBJ_CONTINUATION;
  ret->type = type;
  ret->env = env;
  ret->cont = cont;
  ret->var = var;
  return (object_t)ret;
}

object_t
__set_cont(struct set_cont *sc, object_t val) {
  object_t find = env_get(sc->env, sc->var);
  if (find->type == OBJ_PAIR) {
    set_cdr_op(find, val);
    return eval(scheme_unspecified, sc->env, sc->cont);
  }
  return eval(sys_error(2), sc->env, sc->cont);
}

object_t
__define_cont(struct set_cont *sc, object_t val) {
  env_set(sc->env, sc->var, val);
  return eval(scheme_unspecified, sc->env, sc->cont);
}

struct let_cont {
  struct object_head head;
  enum cont_type type;
  object_t vars;
  object_t body;
  object_t env;
  object_t cont;
};

static object_t
make_let_cont(object_t vars, object_t body, object_t env, object_t cont) {
  struct let_cont *lc = malloc(sizeof(*lc));
  lc->head.type = OBJ_CONTINUATION;
  lc->type = LET_CONT;
  lc->vars = vars;
  lc->body = body;
  lc->env = env;
  lc->cont = cont;
  return (object_t)lc;
}

static object_t
__let_cont(struct let_cont *lc, object_t vals) {
  object_t env = env_extend(lc->env, lc->vars, vals);
  return eval(cons_op(make_symbol("begin"), lc->body), env, lc->cont);
}

struct procedure_call_cont {
  struct object_head head;
  enum cont_type type;
  object_t env;
  object_t cont;
  object_t func;
};

static object_t
make_procedure_call_cont(object_t func, object_t env, object_t cont) {
  struct procedure_call_cont *pcc = malloc(sizeof(*pcc));
  pcc->head.type = OBJ_CONTINUATION;
  pcc->type = PROCEDURE_CALL_CONT;
  pcc->env = env;
  pcc->func = func;
  pcc->cont = cont;
  return (object_t)pcc;
}

static object_t
__procedure_call_cont(struct procedure_call_cont* pcc, object_t vals) {
  return apply(pcc->func, vals, pcc->cont);
}

struct arg_cont {
  struct object_head head;
  enum cont_type type;
  object_t env;
  object_t cont;
  object_t todo;
  object_t finish;
};

static object_t
make_arg_cont(object_t env, object_t cont, object_t todo) {
  struct arg_cont *ac = malloc(sizeof(*ac));
  ac->head.type = OBJ_CONTINUATION;
  ac->type = ARG_CONT;
  ac->env = env;
  ac->cont = cont;
  ac->todo = todo;
  ac->finish = scheme_null;
  return (object_t)ac;
}

static object_t
__arg_cont(struct arg_cont *ac, object_t v) {
  ac->finish = cons_op(v, ac->finish);

  if (scheme_null == ac->todo) {
    return apply_cont(ac->cont, reverse_op(ac->finish));
  }

  object_t arg = car_op(ac->todo);
  ac->todo = cdr_op(ac->todo);
  return eval(arg, ac->env, (object_t)ac);
}

object_t
apply(object_t fn, object_t args, object_t cont) {
  if (fn->type == OBJ_PRIMITIVE) {
    struct primitive *priv = (struct primitive*)fn;
    if (priv->arg_num != length(args)) {
      return sys_error(12);
    }

    switch (priv->arg_num) {
    case 0:
      return apply_cont(cont, priv->func.op0());
    case 1:
      return apply_cont(cont, priv->func.op1(car_op(args)));
    case 2:
      return apply_cont(cont, priv->func.op2(car_op(args), cadr_op(args)));
    case 3:
      return apply_cont(cont, priv->func.op3(car_op(args),
                                             cadr_op(args),
                                             caddr_op(args)));
    case 4:
    case 5:
    case 6:
      // TODO
      assert(0);
    }
  }

  if (fn->type == OBJ_PROCEDURE) {
    struct procedure* proc = (struct procedure*)fn;
    object_t env = env_extend(proc->env, proc->variables, args);
    return eval(cons_op(make_symbol("begin"), proc->body), env, cont);
  }

  return sys_error(1);
}

struct call_cont {
  struct object_head head;
  enum cont_type type;
  object_t env;
  object_t cont;
  object_t args;
};

object_t
make_call_cont(object_t args, object_t env, object_t cont) {
  struct call_cont *fcc = malloc(sizeof(*fcc));
  fcc->head.type = OBJ_CONTINUATION;
  fcc->type = CALL_CONT;
  fcc->env = env;
  fcc->cont = cont;
  fcc->args = args;
  return (object_t)fcc;
}

object_t
__call_cont(struct call_cont* fcc, object_t val) {
  object_t env = fcc->env;
  object_t cont = fcc->cont;
  object_t exp = fcc->args;
  if (val->type == OBJ_SYNTAX) {
    switch(((struct syntax*)val)->type) {
    case SYNTAX_QUOTE:
      return apply_cont(cont, cdr_op(exp));
    case SYNTAX_IF:
      {
        cont = make_if_test_cont(cadr_op(exp), caddr_op(exp), env, cont);
        return eval(car_op(exp), env, cont);
      }
    case SYNTAX_BEGIN:
      if (cdr_op(exp) != scheme_null) {
        cont = make_begin_cont(cdr_op(exp), env, cont);
      }
      return eval(car_op(exp), env, cont);
    case SYNTAX_DEFINE:
      return eval(cadr_op(exp), env, make_set_cont(car_op(exp), env, cont, DEFINE_CONT));
    case SYNTAX_SET:
      return eval(cadr_op(exp), env, make_set_cont(car_op(exp), env, cont, SET_CONT));
    case SYNTAX_LAMBDA:
      return apply_cont(cont, make_procedure(car_op(exp), cdr_op(exp), env));
    case SYNTAX_LET:
      {
        object_t vars = map_op(car_op, car_op(exp));
        object_t vals = map_op(cadr_op, car_op(exp));
        object_t cont1 = make_let_cont(vars, cdr_op(exp), env, cont);
        if (scheme_null == vals) {
          return apply_cont(cont1, scheme_null);
        }
        return eval(car_op(vals), env, make_arg_cont(env, cont1, cdr_op(vals)));
      }
    default:
      *((char*)NULL) = 0;
    }
  } else if (val->type == OBJ_PROCEDURE || val->type == OBJ_PRIMITIVE) {
    object_t cont1 = make_procedure_call_cont(val, env, cont);
    if (scheme_null == exp) {
      return apply_cont(cont1, scheme_null);
    }
    return eval(car_op(exp), env, make_arg_cont(env, cont1, cdr_op(exp)));
  }
  return sys_error(1);
}

object_t
apply_cont(object_t cont, object_t val) {
  assert(cont->type == OBJ_CONTINUATION);
  switch(((struct continuation*)cont)->type) {
  case END_CONT:
    return __end_cont(val);
  case IF_TEST_CONT:
    return __if_test_cont((struct if_test_cont*)cont, val);
  case BEGIN_CONT:
    return __begin_cont((struct begin_cont*)cont, val);
  case SET_CONT:
    return __set_cont((struct set_cont*)cont, val);
  case DEFINE_CONT:
    return __define_cont((struct set_cont*)cont, val);
  case CALL_CONT:
    return __call_cont((struct call_cont*)cont, val);
  case ARG_CONT:
    return __arg_cont((struct arg_cont*)cont, val);
  case PROCEDURE_CALL_CONT:
    return __procedure_call_cont((struct procedure_call_cont*)cont, val);
  case LET_CONT:
    return __let_cont((struct let_cont*)cont, val);
  default:
    *((char*)NULL) = 0;
  }
  return sys_error(0);
}

object_t
env_get(object_t env, object_t var) {
  for(; env != scheme_null; env = cdr_op(env)) {
    object_t cur = car_op(env);
    object_t find = assv_op(var, cur);
    if (find != scheme_false) {
      return find;
    }
  }
  return sys_error(2);
}

static void
env_set(object_t env, object_t var, object_t val) {
  object_t cur;
  for(; env != scheme_null; env = cdr_op(env)) {
    cur = car_op(env);
    object_t find = assv_op(var, cur);
    if (find != scheme_false) {
      set_cdr_op(find, val);
      return;
    }
  }

  object_t kv = cons_op(var, val);
  set_cdr_op(cur, cons_op(kv, cdr_op(cur)));
}

object_t
env_extend(object_t env, object_t vars, object_t vals) {
  object_t binding = scheme_null;
  while(vars!=scheme_null && vals!=scheme_null) {
    object_t kv = cons_op(car_op(vars), car_op(vals));
    binding = cons_op(kv, binding);

    vars=cdr_op(vars);
    vals = cdr_op(vals);
  }
  return cons_op(binding, env);
}

object_t
eval(object_t exp, object_t env, object_t cont) {
  if (exp->type != OBJ_PAIR) {
    if (exp->type != OBJ_SYMBOL) {
      return apply_cont(cont, exp);
    }
    object_t val = env_get(env, exp);
    if (val->type != OBJ_PAIR) {
      return apply_cont(cont, val);
    }
    return apply_cont(cont, cdr_op(val));
  }

  object_t next_cont = make_call_cont(cdr_op(exp), env, cont);
  return eval(car_op(exp), env, next_cont);
}

object_t
scheme_init_env() {
  object_t _if = cons_op(make_symbol("if"), (object_t)&syntax_if);
  object_t _begin = cons_op(make_symbol("begin"), (object_t)&syntax_begin);
  object_t _set = cons_op(make_symbol("set!"), (object_t)&syntax_set);
  object_t _define = cons_op(make_symbol("define"), (object_t)&syntax_define);
  object_t _lambda = cons_op(make_symbol("lambda"), (object_t)&syntax_lambda);
  object_t _let = cons_op(make_symbol("let"), (object_t)&syntax_let);
  object_t _add = cons_op(make_symbol("+"), make_primitive("+", 2, add_op));
  object_t _eq = cons_op(make_symbol("eq?"), make_primitive("eq?", 2, eq_op));
  object_t _sub = cons_op(make_symbol("-"), make_primitive("-", 2, sub_op));
  object_t _mul = cons_op(make_symbol("*"), make_primitive("*", 2, mul_op));

  object_t l = scheme_null;
  l = cons_op(_if, l);
  l = cons_op(_begin, l);
  l = cons_op(_set, l);
  l = cons_op(_lambda, l);
  l = cons_op(_define, l);
  l = cons_op(_let, l);
  l = cons_op(_add, l);
  l = cons_op(_eq, l);
  l = cons_op(_sub, l);
  l = cons_op(_mul, l);

  return cons_op(l, scheme_null);
}

int
main(int argc, char **argv) {
  Number = mpc_new("number");
  Symbol = mpc_new("symbol");
  String = mpc_new("string");
  Comment = mpc_new("comment");
  Sexpr = mpc_new("sexpr");
  Qexpr = mpc_new("qexpr");
  Expr = mpc_new("expr");
  Lispy = mpc_new("lispy");

  mpca_lang(MPCA_LANG_DEFAULT,
            "                                              \
      number  : /-?[0-9]+/ ;                       \
      symbol  : /[a-zA-Z0-9_+\\-\?*\\/\\\\=<>!&]+/ ; \
      string  : /\"(\\\\.|[^\"])*\"/ ;             \
      comment : /;[^\\r\\n]*/ ;                    \
      sexpr   : '(' <expr>* ')' ;                  \
      qexpr   : '{' <expr>* '}' ;                  \
      expr    : <number>  | <symbol> | <string>    \
              | <comment> | <sexpr>  | <qexpr>;    \
      lispy   : /^/ <expr>* /$/ ;                  \
    ",
            Number, Symbol, String, Comment, Sexpr, Qexpr, Expr, Lispy);

  /* Interactive Prompt */
  if (argc == 1) {
    puts("Lispy Version 0.0.0.0.1");
    puts("Press Ctrl+c to Exit\n");

    object_t init_env = scheme_init_env();
    while (1) {
      char *input = readline("lispy> ");
      mpc_result_t r;
      if (mpc_parse("<stdin>", input, Lispy, &r)) {
        lval *ast = lval_read(r.output);
        if (ast->type != LVAL_SEXPR) continue;
        object_t ir = ast_to_ir(ast->cell[0]);
        debug_object(ir);
        printf("\n");
        lval_del(ast);

        object_t result = eval(ir, init_env, make_end_cont());
        debug_object(result);
        printf("\n");

        /*
          lval *x = lval_eval(e, ast);
          lval_println(x);
          lval_del(x);
        */
        mpc_ast_delete(r.output);
      } else {
        mpc_err_print(r.error);
        mpc_err_delete(r.error);
      }
      free(input);
    }
  }

  mpc_cleanup(8, Number, Symbol, String, Comment, Sexpr, Qexpr, Expr, Lispy);
  return 0;
}
