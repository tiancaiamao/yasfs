#include "stdio.h"
#include "stdlib.h"
#include "scheme.h"
#include "assert.h"


Value ValueTrue;
Value ValueFalse;
Value fact;
Value c0;

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

void lambda__tmp7246(Value env15307, Value rv$5141) {
    ((struct Closure *)VectorRef(MakeInt(0), env15307))
        ->lam(((struct Closure *)VectorRef(MakeInt(0), env15307))->env,
              __product(VectorRef(MakeInt(1), env15307), rv$5141));
}

void lambda__tmp7245(Value env15308, Value n, Value k5140) {
    struct Closure tmp7240;
    struct Vector tmp7241;
    tmp7241.value = alloca(sizeof(Value) * 2);
    if (ValueEqual(n, MakeInt(0)) == ValueTrue) {
        ((struct Closure *)k5140)
            ->lam(((struct Closure *)k5140)->env, MakeInt(1));
    } else {
        ((struct Closure *)fact)
            ->lam(((struct Closure *)fact)->env, __sub(n, MakeInt(1)),
                  InitClosure(&tmp7240, lambda__tmp7246,
                              InitVector(&tmp7241, 2, k5140, n)));
    }
}

int main() {
    ValueTrue = MakeBoolean(1);
    ValueFalse = MakeBoolean(0);
    struct Closure tmp2;
    c0 = InitClosure(&tmp2, lambda__c0, NULL);

    struct Closure tmp7239;
    struct Vector tmp7242;
    tmp7242.value = alloca(sizeof(Value) * 1);
    fact =
        InitClosure(&tmp7239, lambda__tmp7245, InitVector(&tmp7242, 1, fact));

    ((struct Closure *)fact)
        ->lam(((struct Closure *)fact)->env, MakeInt(5), c0);

    return 0;
}