#include "stdio.h"
#include "stdlib.h"
#include "scheme.h"
#include "assert.h"

void lambda__tmp8534(Value env8527, Value rv$8526);
Value fact;
void lambda__tmp8533(Value env8528, Value n, Value k8525) {

    struct Closure tmp8530;
    struct Env tmp8531;
    tmp8531.value = alloca(sizeof(Value) * 2);
    if (ValueEqual(n, MakeInt(0)) == ValueTrue) {
        ((struct Closure *)k8525)->lam(((struct Closure *)k8525)->env, MakeInt(1));
    } else {
        ((struct Closure *)fact)
            ->lam(((struct Closure *)fact)->env, __sub(n, MakeInt(1)), InitClosure(&tmp8530, lambda__tmp8534, InitEnv(&tmp8531, 2, k8525, n)));
    }
}

void lambda__tmp8534(Value env8527, Value rv$8526) {

    ((struct Closure *)EnvRef(MakeInt(0), env8527))->lam(((struct Closure *)EnvRef(MakeInt(0), env8527))->env, __product(EnvRef(MakeInt(1), env8527), rv$8526));
}

void TopLevel(Value cont) {
    if (CheckMinorGC()) {
        SaveCall(TopLevel, 1, cont);
        MinorGC();
    }
    struct Closure tmp8529;
    struct Env tmp8532;
    tmp8532.value = alloca(sizeof(Value) * 1);
    fact = InitClosure(&tmp8529, lambda__tmp8533, InitEnv(&tmp8532, 1, fact));

    ((struct Closure *)fact)->lam(((struct Closure *)fact)->env, MakeInt(5), cont);
}

void lambda__c0(Value e, Value v) { printf("return = %ld\n", (long)v >> 1); }

int main() {
    // c0的初始化要在EntryPoint前面
    struct Closure tmp2;
    InitClosure(&tmp2, lambda__c0, NULL);

    EntryPoint((Value)&tmp2);

    return 0;
}