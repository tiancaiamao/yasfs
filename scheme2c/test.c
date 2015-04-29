#include "stdio.h"
#include "stdlib.h"
#include "scheme.h"
#include "assert.h"
#include "stdarg.h"

Value ValueTrue;
Value ValueFalse;
Value fact;
Value c0;

Value MakeEnv(int n, ...) {
    va_list ap;
    struct Vector *ret = (struct Vector *)MakeVector(n);

    va_start(ap, n);
    for (int i = 0; i < n; i++) {
        ret->value[i] = va_arg(ap, Value);
    }
    va_end(ap);

    return (Value)ret;
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

void lambda__c0(Value e, Value v) { 
	printf("return = %d\n", ((struct Int *)v)->value); 
}

void lambda__tmp124(Value env30361, Value rv30359) {
    struct Closure *k = (struct Closure *)VectorGet(env30361, 0);
    k->lam(k->env, __product(VectorGet(env30361, 1), rv30359));
}

void lambda__tmp123(Value e, Value n, Value k) {
    if (ValueEqual(n, MakeInt(0)) == ValueTrue) {
        ((struct Closure *)k)->lam(((struct Closure *)k)->env, MakeInt(1));
    } else {
        ((struct Closure *)fact)
            ->lam(((struct Closure *)fact)->env, __sub(n, MakeInt(1)),
                  MakeClosure(lambda__tmp124, MakeEnv(2, k, n)));
    }
}

void init() {
    ValueTrue = MakeBoolean(1);
    ValueFalse = MakeBoolean(0);
    fact = MakeClosure(lambda__tmp123, NULL);
    c0 = MakeClosure(lambda__c0, NULL);
}

int main() {
    init();

    ((struct Closure *)fact)
        ->lam(((struct Closure *)fact)->env, MakeInt(5), c0);

    return 0;
}