#include "stdio.h"
#include "stdlib.h"
#include "scheme.h"
#include "assert.h"
#include "stdarg.h"

Value ValueTrue;
Value ValueFalse;
Value fact;
Value c0;

Value MakeEnv(Value n1, ...) {
    int n;
    assert(n1->t == INT);
    n = ((struct Int *)n1)->value;
    va_list ap;
    struct Vector *ret = (struct Vector *)MakeVector(n);

    va_start(ap, n1);
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

void lambda__tmp8219(Value env5388, Value rv$5385) {
    ((struct Closure *)VectorRef(MakeInt(0), env5388))
        ->lam(((struct Closure *)VectorRef(MakeInt(0), env5388))->env,
              __product(VectorRef(MakeInt(1), env5388), rv$5385));
}

void lambda__tmp8218(Value env5389, Value n, Value k5384) {
    if (ValueEqual(n, MakeInt(0)) == ValueTrue) {
        ((struct Closure *)k5384)
            ->lam(((struct Closure *)k5384)->env, MakeInt(1));
    } else {
        ((struct Closure *)fact)
            ->lam(((struct Closure *)fact)->env, __sub(n, MakeInt(1)),
                  MakeClosure(lambda__tmp8219, MakeEnv(MakeInt(2), k5384, n)));
    }
}

void init() {
    ValueTrue = MakeBoolean(1);
    ValueFalse = MakeBoolean(0);
    fact = MakeClosure(lambda__tmp8218, MakeEnv(MakeInt(1), fact));
    c0 = MakeClosure(lambda__c0, NULL);
}

int main() {
    init();

    ((struct Closure *)fact)
        ->lam(((struct Closure *)fact)->env, MakeInt(5), c0);

    return 0;
}