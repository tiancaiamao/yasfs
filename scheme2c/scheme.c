#include "scheme.h"
#include <setjmp.h>
#include <stdlib.h>

Value InitClosure(struct Closure *addr, Lambda lam, Value env) {
    struct Closure *v = (struct Closure *)addr;
    v->t = CLOSURE;
    v->lam = lam;
    v->env = env;
    return (Value)v;
}

Value MakeInt(int n) {
    struct Int *v = malloc(sizeof(struct Int));
    v->t = INT;
    v->value = n;
    return (Value)v;
}

Value MakeBoolean(unsigned int b) {
    struct Boolean *v = malloc(sizeof(struct Boolean));
    v->t = BOOLEAN;
    v->value = b;
    return (Value)v;
}

// static Value InitPrimitive(Lambda prim) {
//   struct Primitive * v = malloc(sizeof(struct Primitive));
//   v->t = CLOSURE ;
//   v->lam = prim ;
//   v->env = NULL ;
//   return (Value)v ;
// }

Value InitVector(struct Vector *addr, int n, ...) {
    va_list ap;
    addr->t = VECTOR;
    addr->size = n;

    va_start(ap, n);
    for (int i = 0; i < n; i++) {
        addr->value[i] = va_arg(ap, Value);
    }
    va_end(ap);

    return (Value)addr;
}

Value VectorGet(Value v, int n) {
    assert(v->t == VECTOR);
    assert(((struct Vector *)v)->size > n);
    return ((struct Vector *)v)->value[n];
}

Value VectorRef(Value n, Value e) {
    assert(e->t == VECTOR);
    assert(n->t == INT);

    int nn = ((struct Int *)n)->value;
    assert(((struct Vector *)e)->size > nn);

    return ((struct Vector *)e)->value[nn];
}

Value NewCell(Value initialValue) {
    struct Cell *v = malloc(sizeof(struct Cell));
    v->t = CELL;
    v->addr = initialValue;
    return (Value)v;
}

Value ValueEqual(Value v1, Value v2) {
    assert(v1->t == v2->t);
    if (((struct Int *)v1)->value == ((struct Int *)v2)->value) {
        return ValueTrue;
    }
    return ValueFalse;
}

Value __product(Value v1, Value v2) {
    int tmp = ((struct Int *)v1)->value * ((struct Int *)v2)->value;
    return MakeInt(tmp);
}

Value __sub(Value v1, Value v2) {
    int tmp = ((struct Int *)v1)->value - ((struct Int *)v2)->value;
    return MakeInt(tmp);
}

Value ValueTrue;
Value ValueFalse;
Value saved_cont_call;
jmp_buf empty_stack_state;

void CheckMinorGC(Value c) {}

void DriverLoop(Value call) {
    // setjmp将当前上下文保存起来并返回0
    if (setjmp(empty_stack_state)) {
        call = saved_cont_call;
    }

    ((struct Closure *)call)->lam(((struct Closure *)call)->env);
}

void EntryPoint(Value halt) {
    ValueTrue = MakeBoolean(1);
    ValueFalse = MakeBoolean(0);

    struct Closure tmp;
    InitClosure(&tmp, TopLevel, halt);

    DriverLoop((Value)&tmp);
}

void MinorGC() {
    // 将数据拷到堆中
    longjmp(empty_stack_state, 1);
}
