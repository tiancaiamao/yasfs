void funcXXX(struct vm *vm){
ENTER_CLOSURE(1,"funcXXX");
vm->value = make_number(3);
PUSH(vm->value);
value = SHALLOW_ARGUMENT_REF(0);
vm->func = vm->value;
vm->value = make_vector(1);
CLOSURE_CALL()
vector_set(vm->value,0,POP())
PUSH(vm->value);
vm->value = make_closure(funcXXX,vm->env);
value = SHALLOW_ARGUMENT_SET(0);
vm->func = vm->value;
vm->value = make_vector(1);
CLOSURE_CALL()
vector_set(vm->value,0,POP())
EXIT_CLOSURE();
}
void funcXXX(struct vm *vm){
ENTER_CLOSURE(1,"funcXXX");
vm->value = make_number(6);
value = SHALLOW_ARGUMENT_SET(1);
value = SHALLOW_ARGUMENT_REF(0);
PUSH(vm->value);
value = SHALLOW_ARGUMENT_REF(1);
PUSH(vm->value);
vm->value = cons(POP(),POP())
EXIT_CLOSURE();
}
void funcXXX(struct vm *vm){
ENTER_CLOSURE(1,"funcXXX");
vm->value = make_number(6);
value = SHALLOW_ARGUMENT_SET(1);
value = SHALLOW_ARGUMENT_REF(0);
PUSH(vm->value);
value = SHALLOW_ARGUMENT_REF(1);
PUSH(vm->value);
vm->value = cons(POP(),POP())
EXIT_CLOSURE();
}


//code
vm->value = make_closure(funcXXX,vm->env);
vm->func = vm->value;
vm->value = make_vector(0);
CLOSURE_CALL()
