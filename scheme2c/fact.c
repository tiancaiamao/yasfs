#include "stdio.h"
#include "stdlib.h"
#include "scheme.h"
#include "assert.h"

void lambda__tmp8534(Value env8527, Value rv$8526);
Value fact;
void lambda__tmp8533(Value env8528, Value n, Value k8525) {
    printf("lambda__tmp8533中, n=%ld, 栈位置为%p\n", (Tag)n >> 1, &n);
    if (CheckMinorGC()) {
        SaveCall(lambda__tmp8533, 3, env8528, n, k8525);
        MinorGC();
    }

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
    printf("lambda__tmp8534中\n");
    if (CheckMinorGC()) {
        SaveCall(lambda__tmp8534, 2, env8527, rv$8526);
        MinorGC();
    }

    ((struct Closure *)EnvRef(MakeInt(0), env8527))->lam(((struct Closure *)EnvRef(MakeInt(0), env8527))->env, __product(EnvRef(MakeInt(1), env8527), rv$8526));
}

void TopLevel(Value cont) {
    if (CheckMinorGC()) {
        SaveCall(TopLevel, 1, cont);
        MinorGC();
    }
	
	// 全局变量的东西需要在堆上分配，因为MinorGC没有扫描全局变量
    // struct Closure tmp8529;
    // struct Env tmp8532;
    struct Closure *tmp8529 = malloc(sizeof(struct Closure));
    struct Env *tmp8532 = malloc(sizeof(struct Env));
    tmp8532->value = malloc(sizeof(Value) * 1);
    // tmp8532.value = alloca(sizeof(Value) * 1);
    fact = InitClosure(tmp8529, lambda__tmp8533, InitEnv(tmp8532, 1, fact));

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