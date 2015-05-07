#include <assert.h>
#include <stdarg.h>

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

// static Value InitPrimitive(Lambda prim) {
//   struct Primitive * v = malloc(sizeof(struct Primitive));
//   v->t = CLOSURE ;
//   v->lam = prim ;
//   v->env = NULL ;
//   return (Value)v ;
// }

Value InitClosure(struct Closure *addr, Lambda lam, Value env);
Value MakeInt(int n);
Value MakeBoolean(unsigned int b);
Value InitVector(struct Vector *addr, int n, ...);
Value VectorGet(Value v, int n);
Value VectorRef(Value n, Value e);
Value NewCell(Value initialValue);

Value __sub(Value v1, Value v2);
Value __product(Value v1, Value v2);
Value ValueEqual(Value v1, Value v2);

// EntryPoint是整个库的入口点。它的参数是整个计算结果的返回点
void EntryPoint(Value);
void CheckMinorGC(Value);
void MinorGC();

// TopLevel是生成的代码入口点
extern void TopLevel(Value);

extern Value ValueTrue;
extern Value ValueFalse;