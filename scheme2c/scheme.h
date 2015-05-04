#include <assert.h>

typedef union Value_t* Value;
enum Tag { VOID, INT, BOOLEAN, CLOSURE, CELL, ENV, VECTOR } ;

typedef void (*Lambda)() ;

struct Int {
  enum Tag t ;
  int value ;
};

struct Boolean {
  enum Tag t ;
  unsigned int value ;
};

struct Closure {
  enum Tag t ;
  Lambda lam ;
  Value env ;
};

struct Env {
  enum Tag t ;
  Value d1;
  Value d2;
  void* env ;
};

struct Vector {
	enum Tag t;
	int size;
	Value *value;
};

struct Cell {
  enum Tag t ;
  Value addr ; 
};

union Value_t {
  enum Tag t ;
  struct Int z ;
  struct Boolean b ;
  struct Closure clo ;
  struct Env env ;
  struct Cell cell ;
};

static Value InitClosure(Value addr, Lambda lam, Value env) {
  struct Closure* v  =	(struct Closure*)addr;
  v->t = CLOSURE ;
  v->lam = lam ;
  v->env = env ;
  return (Value)v;
}

Value MakeInt(int n) {
  struct Int* v = malloc(sizeof(struct Int));
  v->t = INT ;
  v->value = n ;
  return (Value)v ;
}

Value MakeBoolean(unsigned int b) {
  struct Boolean* v  = malloc(sizeof(struct Boolean));
  v->t = BOOLEAN ;
  v->value = b ;
  return (Value)v ;
}

// static Value InitPrimitive(Lambda prim) {
//   struct Primitive * v = malloc(sizeof(struct Primitive));
//   v->t = CLOSURE ;
//   v->lam = prim ;
//   v->env = NULL ;
//   return (Value)v ;
// }

// Value InitVector(int n) {
// 	struct Vector* v = malloc(sizeof(struct Vector) + n*sizeof(Value));
// 	v->t = VECTOR;
// 	v->size = n;
// 	return (Value)v;
// }

Value VectorGet(Value v, int n) {
	assert(v->t == VECTOR);
	assert(((struct Vector*)v)->size > n);
	return ((struct Vector*)v)->value[n];
}

Value VectorRef(Value n, Value e) {
	assert(e->t == VECTOR);
	assert(n->t == INT);

	int nn = ((struct Int*)n)->value;
	assert(((struct Vector*)e)->size > nn);
	
	return ((struct Vector*)e)->value[nn];
}

Value NewCell(Value initialValue) {
  struct Cell* v = malloc(sizeof(struct Cell));
  v->t = CELL ;
  v->addr = initialValue ;
  return (Value)v ;
}