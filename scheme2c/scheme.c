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

Value MakeInt(int n) { return (Value)(((intptr_t)n << 1) | 0x1); }

Value MakeBoolean(unsigned int b) { return b == 0 ? ValueFalse : ValueTrue; }

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

Value InitEnv(struct Env *addr, int n, ...) {
    va_list ap;
    addr->t = ENV;
    addr->size = n;

    va_start(ap, n);
    for (int i = 0; i < n; i++) {
        addr->value[i] = va_arg(ap, Value);
    }
    va_end(ap);

    return (Value)addr;
}

Value VectorGet(Value v, int n) {
    assert((long)v->t == VECTOR);
    assert(((struct Vector *)v)->size > n);
    return ((struct Vector *)v)->value[n];
}

Value VectorRef(Value n, Value e) {
    assert((long)e->t == VECTOR);
    assert(((long)n & 1) == 1);

    int nn = (long)n >> 1;
    assert(((struct Vector *)e)->size > nn);

    return ((struct Vector *)e)->value[nn];
}

Value EnvRef(Value n, Value e) {
    assert((long)e->t == ENV);
    assert(((long)n & 1) == 1);

    int nn = (long)n >> 1;
    assert(((struct Vector *)e)->size > nn);

    return ((struct Vector *)e)->value[nn];
}

Value ValueEqual(Value v1, Value v2) {
    if (v1 == v2) {
        return ValueTrue;
    }
    return ValueFalse;
}

Value __product(Value v1, Value v2) { return (Value)(((((Tag)v1 >> 1) * ((Tag)v2 >> 1)) << 1) | 0x1); }

Value __sub(Value v1, Value v2) { return (Value)(((Tag)v1 - (Tag)v2) | 0x1); }

Value ValueTrue = (Value)0xa;
Value ValueFalse = (Value)0x2;
Value saved_cont_call;
jmp_buf empty_stack_state;
char *stackBottom;
char *stackTop;
static int MinorGCSize = 4 << 10;
// static int MinorGCSize = 20;

char *heapStart;
char *heapEnd;

// DriverLoop接受的参数是一个可以直接执行的Closure
void DriverLoop(Value call) {
    stackTop = (char *)&call;
    // setjmp将当前上下文保存起来并返回0
    if (setjmp(empty_stack_state)) {
        call = saved_cont_call;
    }

    ((struct Closure *)call)->lam(((struct Closure *)call)->env);
}

void EntryPoint(Value halt) {
    // 初始化部分可以写到这里

    // 初始4M的堆空间
    const int heapSize = 4 << 20;
    heapStart = malloc(heapSize);
    heapEnd = heapStart + heapSize;

    struct Closure tmp;
    InitClosure(&tmp, TopLevel, halt);

    DriverLoop((Value)&tmp);
}

// --------------垃圾回收相关的一组函数--------------
// 参数是一个vector，vector中第一个是closure，后面的是这个closure的参数
static void _lambda_save_call(Value v) {
    assert((((Tag)v & 7) == 0) && (v->t == VECTOR));
    struct Vector *vec = (struct Vector *)v;
    assert(vec->size > 0);
    assert((((long)(vec->value[0]) & 7) == 0) && (((long)((struct Vector *)vec->value[0])->t) == CLOSURE));

    struct Closure *clo = (struct Closure *)vec->value[0];

    switch (vec->size) {
    case 1:
    case 2:
    case 3:
        clo->lam(clo->env, vec->value[1], vec->value[2], vec->value[3]);
        break;
    case 4:
    case 5:
    case 6:
    case 7:
    case 8:
    case 9:
    case 10:
    case 11:
    case 12:
    case 13:
    case 14:
    case 15:
    case 16:
        break;
    }
}

// TODO 注意到堆上分配
void SaveCall(Lambda lam, int n, ...) {
    struct Closure *clo = malloc(sizeof(struct Closure));
    InitClosure(clo, lam, NULL);

    struct Vector *vec = malloc(sizeof(struct Vector));
    // clo以及参数拷到vec
    vec->value[0] = (Value)clo;

    struct Closure *save = malloc(sizeof(struct Closure));
    saved_cont_call = InitClosure(save, _lambda_save_call, (Value)vec);
}

int CheckMinorGC() {
    char *ptr = (char *)&ptr;
    if (stackTop - ptr > MinorGCSize) {
        return 1;
    }
    return 0;
}

static int objectCount(Value obj) {
    if (((long)obj & 7) == 0) {
        switch ((long)obj) {
        case VECTOR:
            return ((struct Vector *)obj)->size;
        case CLOSURE:
            return 1;
        case CONS:
            return 2;
        case ENV:
            return ((struct Env *)obj)->size;
        }
    }
    return 0;
}

static Value getSlot(Value obj, int i) {
    switch ((long)obj) {
    case VECTOR:
        return ((struct Vector *)obj)->value[i];
    case CLOSURE:
        assert(i == 0);
        return ((struct Closure *)obj)->env;
    case CONS:
        assert(i == 0 || i == 1);
        return i == 0 ? ((struct Cons *)obj)->car : ((struct Cons *)obj)->cdr;
    case ENV:
        return ((struct Env *)obj)->value[i];
    }
    assert(0);
    return NULL;
}

static void setSlot(Value obj, int i, Value v) {
    switch ((long)obj) {
    case VECTOR:
        ((struct Vector *)obj)->value[i] = v;
    case CLOSURE:
        assert(i == 0);
        ((struct Closure *)obj)->env = v;
    case CONS:
        assert(i == 0 || i == 1);
        if (i == 0) {
            ((struct Cons *)obj)->car = v;
        } else {
            ((struct Cons *)obj)->cdr = v;
        }
    case ENV:
        ((struct Env *)obj)->value[i] = v;
    }
    assert(0);
}

static int existsInStack(Value obj) { return (char *)obj <= stackTop && (char *)obj >= stackBottom; }

static int isForwardingPtr(Value obj) { return ((long)obj & FORWARD_BIT) == 0; }

static Value forwardingPtrTarget(Value obj) {
    switch (((long)((struct Env *)obj)->t) & INTERMEDIA_TYPE_MASK) {
    case CLOSURE:
        return ((struct Closure *)obj)->env;
    case CONS:
        return ((struct Cons *)obj)->car;
    case ENV:
        return (Value)((struct Env *)obj)->value;
    case VECTOR:
        return (Value)((struct Vector *)obj)->value;
    }
    assert(0);
    return NULL;
}

static int objectSize(Value obj) {
    switch ((long)obj) {
    case CLOSURE:
        return sizeof(struct Closure);
    case ENV:
        return sizeof(struct Env);
    case CONS:
        return sizeof(struct Cons);
    case VECTOR:
        return sizeof(struct Vector);
    }
    assert(0);
}

static int copyObject(Value obj, char *ptr) {
    switch ((long)obj) {
    case CLOSURE:
        *((struct Closure *)ptr) = *((struct Closure *)obj);
        ((struct Closure *)obj)->env = (Value)ptr;
        return sizeof(struct Closure);
    case ENV:
        *((struct Env *)ptr) = *((struct Env *)obj);
        ((struct Env *)obj)->value = (Value*)ptr;
        return sizeof(struct Env);
    case CONS:
        *((struct Cons *)ptr) = *((struct Cons *)obj);
        ((struct Cons *)obj)->car = (Value)ptr;
        return sizeof(struct Cons);
    case VECTOR:
        *((struct Vector *)ptr) = *((struct Vector *)obj);
        ((struct Vector *)obj)->value = (Value*)ptr;
        return sizeof(struct Vector);
    }
    assert(0);
}

void MinorGC() {
    int i, bytes, count;
    char *scanStart;
    Value obj, slot;
    stackBottom = (char *)&i;

    struct Vector *vec = (struct Vector *)saved_cont_call;
    for (i = 0; i < vec->size; i++) {
        bytes = copyObject(vec->value[i], heapEnd);
        heapEnd += bytes;
    }

    scanStart = heapStart;
    while (scanStart < heapEnd) {
        obj = (Value)scanStart;
        count = objectCount(obj);
        for (i = 0; i < count; ++i) {
            slot = getSlot(obj, i);
            // 只需要处理在栈上的对象
            if (existsInStack(slot)) {
                if (isForwardingPtr(slot)) {
                    setSlot(obj, i, forwardingPtrTarget(slot));
                } else {
                    bytes = copyObject(slot, heapEnd);
                    setSlot(obj, i, (Value)heapEnd);
                    heapEnd += bytes;
                }
            }
        }
        scanStart += objectSize(obj);
    }

    longjmp(empty_stack_state, 1);
}
