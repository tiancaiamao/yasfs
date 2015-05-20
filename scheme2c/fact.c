#include "stdio.h"
#include "stdlib.h"
#include "scheme.h"
#include "assert.h"

void lambda__tmp8586(Value env8585, Value fact, Value k8578);
void lambda__tmp8589(Value env8584, Value rv$8579);
void lambda__tmp8587(Value env8583, Value n, Value k8580);
void lambda__tmp8588(Value env8582, Value rv$8581);

void lambda__tmp8586(Value env8585, Value fact, Value k8578) {
    if (CheckMinorGC()) {
        SaveCall(lambda__tmp8586, 3, env8585, fact, k8578);
        MinorGC();
    }
    __set(&fact, ((struct Closure *)closure)->lam(((struct Closure *)closure)->env, lambda__tmp8587, env - make(MakeInt(1), fact)),
          ((struct Closure *)closure)->lam(((struct Closure *)closure)->env, lambda__tmp8589, env - make(MakeInt(2), fact, k8578)));
}

void lambda__tmp8589(Value env8584, Value rv$8579) {
    if (CheckMinorGC()) {
        SaveCall(lambda__tmp8589, 2, env8584, rv$8579);
        MinorGC();
    }
    ((struct Closure *)EnvRef(MakeInt(0), env8584))->lam(((struct Closure *)EnvRef(MakeInt(0), env8584))->env, MakeInt(5), EnvRef(MakeInt(1), env8584));
}

void lambda__tmp8587(Value env8583, Value n, Value k8580) {
    if (CheckMinorGC()) {
        SaveCall(lambda__tmp8587, 3, env8583, n, k8580);
        MinorGC();
    }
    if (ValueEqual(n, MakeInt(0)) == ValueTrue) {
        ((struct Closure *)k8580)->lam(((struct Closure *)k8580)->env, MakeInt(1));
    } else {
        ((struct Closure *)EnvRef(MakeInt(0), env8583))
            ->lam(((struct Closure *)EnvRef(MakeInt(0), env8583))->env, __sub(n, MakeInt(1)),
                  ((struct Closure *)closure)->lam(((struct Closure *)closure)->env, lambda__tmp8588, env - make(MakeInt(2), k8580, n)));
    }
}

void lambda__tmp8588(Value env8582, Value rv$8581) {
    if (CheckMinorGC()) {
        SaveCall(lambda__tmp8588, 2, env8582, rv$8581);
        MinorGC();
    }
    ((struct Closure *)EnvRef(MakeInt(0), env8582))->lam(((struct Closure *)EnvRef(MakeInt(0), env8582))->env, __product(EnvRef(MakeInt(1), env8582), rv$8581));
}

void TopLevel(Value cont) {
    ((struct Closure *)((struct Closure *)closure)->lam(((struct Closure *)closure)->env, lambda__tmp8586, env - make(MakeInt(0))))
        ->lam(((struct Closure *)((struct Closure *)closure)->lam(((struct Closure *)closure)->env, lambda__tmp8586, env - make(MakeInt(0))))->env, cont);
}

void lambda__c0(Value e, Value v) { printf("return = %ld\n", (long)v >> 1); }

int main() {
    // c0的初始化要在EntryPoint前面
    struct Closure tmp2;
    InitClosure(&tmp2, lambda__c0, NULL);

    EntryPoint((Value)&tmp2);

    return 0;
}