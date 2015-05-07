#include "stdio.h"
#include "stdlib.h"
#include "scheme.h"
#include "assert.h"

Value fact;

void lambda__tmp24711(Value env24704, Value rv$24703) {
    ((struct Closure *)VectorRef(MakeInt(0), env24704))
        ->lam(((struct Closure *)VectorRef(MakeInt(0), env24704))->env, __product(VectorRef(MakeInt(1), env24704), rv$24703));
}

void lambda__tmp24710(Value env24705, Value n, Value k24702) {
    struct Closure tmp24707;
    struct Vector tmp24708;
    tmp24708.value = alloca(sizeof(Value) * 2);
    if (ValueEqual(n, MakeInt(0)) == ValueTrue) {
        ((struct Closure *)k24702)->lam(((struct Closure *)k24702)->env, MakeInt(1));
    } else {
        ((struct Closure *)fact)
            ->lam(((struct Closure *)fact)->env, __sub(n, MakeInt(1)), InitClosure(&tmp24707, lambda__tmp24711, InitVector(&tmp24708, 2, k24702, n)));
    }
}

void TopLevel(Value cont) {
    CheckMinorGC(cont);

    // 正常代码
    struct Closure tmp24706;
    struct Vector tmp24709;
    tmp24709.value = alloca(sizeof(Value) * 1);
    fact = InitClosure(&tmp24706, lambda__tmp24710, InitVector(&tmp24709, 1, fact));

    ((struct Closure *)fact)->lam(((struct Closure *)fact)->env, MakeInt(5), cont);
}

void lambda__c0(Value e, Value v) { printf("return = %d\n", ((struct Int *)v)->value); }

int main() {
    // c0的初始化要在EntryPoint前面
    struct Closure tmp2;
    InitClosure(&tmp2, lambda__c0, NULL);

    EntryPoint((Value)&tmp2);

    return 0;
}