#include "vm.h"
#include "type.h"
#include <stdio.h>

/* char* generate_push(char *p,int v) */
/* { */
/* 	*p++ = PUSH; */
/* 	*((int *)p) = v; */
/* 	p += sizeof(int); */
/* 	return p; */
/* } */


/* char* generate_add(char *bytecode,int a,int b) */
/* { */
/* 	char *p; */

/* 	p = bytecode; */
/* 	*p++ = PUSH; */
/* 	*((int *)p) = a; */
/* 	p += sizeof(int); */
/* 	*p++ = PUSH; */
/* 	*((int *)p) = b; */
/* 	p += sizeof(int); */
/* 	*p++ = ADD; */
/* 	return p; */
/* } */


/* void fix_label(char *locate,char *value) */
/* { */
/* 	*((char**)locate) = value; */
/* } */

/* char *generate_label(char *bytecode,char **label) */
/* { */
/* 	*label = bytecode; */
/* 	bytecode += sizeof(char*); */
/* 	return bytecode; */
/* } */

/* char* generate_if(char *bytecode) */
/* { */
/* 	char *ip; */
/* 	char *label1; */
/* 	char *label2; */

/* 	ip = bytecode; */
/* 	ip = generate_add(ip,3, 5); */
/* 	ip = generate_push(ip,8); */
/* 	*ip++ = EQ; */
/* 	*ip++ = JUMP_UNLESS; */
/* 	ip = generate_label(ip,&label1); */
/* 	ip = generate_push(ip,4); */
/* 	*ip++ = JUMP; */
/* 	ip = generate_label(ip,&label2); */
/* 	fix_label(label1,ip); */
/* 	ip = generate_push(ip,6); */
/* 	fix_label(label2,ip); */
/* 	return ip; */
/* } */

typedef OBJ (*fn1_t)(OBJ);
typedef OBJ (*fn2_t)(OBJ,OBJ);

void print_bytecode(char *bytecode)
{
	char *p = bytecode;
loop:
	printf("%p    ",p);
	switch(*p++)
	{
	case CAR:
		printf("CAR\n");
		goto loop;
	case CDR:
		printf("CDR\n");
		goto loop;
	case SET_CAR:
		printf("SET_CAR\n");
		goto loop;
	case CONS:
		printf("CONS\n");
		goto loop;
	case ADD:
		printf("ADD\n");
		goto loop;
	case SUB:
		printf("SUB\n");
		goto loop;
	case MUL:
		printf("MUL\n");
		goto loop;
	case DIV:
		printf("DIV\n");
		goto loop;
	case PUSH:
		printf("PUSH %d\n",*((int*)p));
		p += sizeof(OBJ);
		goto loop;
	case POP:
		printf("POP\n");
		goto loop;
	case SET_CDR:
		printf("SET_CDR\n");
		goto loop;
	case REF:
		printf("REF\n");
		goto loop;
	case TYPE:
		switch(*p)
		{
		case OBJ_BOOLEAN:
			printf("TYPE OBJ_BOOLEAN\n");
			break;
		case OBJ_SYMBOL:
			printf("TYPE OBJ_SYMBOL\n");
			break;
		case OBJ_CHAR:
			printf("TYPE OBJ_CHAR\n");
			break;
		case OBJ_VECTOR:
			printf("TYPE OBJ_VECTOR\n");
			break;
		case OBJ_PROCEDURE:
			printf("TYPE OBJ_PROCEDURE\n");
			break;
		case OBJ_PAIR:
			printf("TYPE OBJ_PAIR\n");
			break;
		case OBJ_NUMBER:
			printf("TYPE OBJ_NUMBER\n");
			break;
		case OBJ_STRING:
			printf("TYPE OBJ_STRING\n");
			break;
		default:
			printf("TYPE UNKNOWN TYPE!!!\n");
		}
		p += sizeof(OBJ);
		goto loop;
	case EQ:
		printf("EQ\n");
		goto loop;
	case JUMP:
		printf("JUMP %x\n",*((unsigned int*)p));
		p += sizeof(char*);
		goto loop;
	case JUMP_UNLESS:
		printf("JUMP_UNLESS %x\n",*((unsigned int*)p));
		p += sizeof(char*);
		goto loop;
	case CALL:
		printf("CALL\n");
		goto loop;
	case TAIL_CALL:
		printf("TAIL_CALL %x\n",*((unsigned int*)p));
		print_bytecode(obj_string_data(obj_procedure_code(*((OBJ*)p))));
		p += sizeof(OBJ);
		goto loop;
	case RET:
		printf("RET\n");
		return;
	case FC1:
		printf("FC1 %p\n",*((void**)p));
		p += sizeof(void*);
		goto loop;
	case FC2:
		printf("FC1 %p\n",*((void**)p));
		p += sizeof(void*);
		goto loop;
	case DONE:
		printf("DONE\n");
		return;
	default:
		printf("error instruction!\n");
		break;
	}
}

/* int vm_init(struct vm *vm) */
/* { */
/* 	vm->top = 0; */
/* 	vm->fp = 0; */
/* 	vm->ip = NULL; */
/* 	vm->bytecode = NULL; */
/* 	return 0; */
/* } */

/* int vm_eval(struct vm *vm,char *bytecode) */
/* { */
/* #define stack (vm->stack) */
/* #define top   (vm->top) */
/* #define ip    (vm->ip) */
/* #define fp    (vm->fp) */

/* 	ip = bytecode; */
/* 	while(ip) */
/* 	{ */
/* 		switch(*ip++) */
/* 		{ */
/* 		case ADD: */
/* 			stack[top-2] = stack[top-1] + stack[top-2]; */
/* 			top--; */
/* 			break; */
/* 		case PUSH: */
/* 			stack[top] = *((int*)ip); */
/* 			top++; */
/* 			ip += sizeof(int); */
/* 			break; */
/* 		case POP: */
/* 			top--; */
/* 			break; */
/* 		case EQ: */
/* 			if(stack[top-1] == stack[top-2]) */
/* 			{ */
/* 				stack[top-2] = 1; */
/* 				top--; */
/* 			} */
/* 			else */
/* 			{ */
/* 				stack[top-2] = 0; */
/* 				top--; */
/* 			} */
/* 			break; */
/* 		case JUMP: */
/* 			ip = *((char**)ip); */
/* 			break; */
/* 		case JUMP_UNLESS: */
/* 			if(stack[top-1] == 0) */
/* 				ip = *((char**)ip); */
/* 			ip += sizeof(char*); */
/* 			break; */
/* 		case DONE: */
/* 			goto out; */
/* 		default: */
/* 			printf("error instruction!\n"); */
/* 			goto out; */
/* 		} */
/* 	} */
/* 	printf("error instruction!\n"); */
/* 	return -1; */
/* out: */
/* 	return stack[top-1]; */
/* #undef stack */
/* #undef top */
/* #undef ip */
/* #undef fp */
/* } */

OBJ vm(char *bytecode)
{
	OBJ stack[1000];
	int top = 0;
	long fp = 0;
	char *ip;
	OBJ tmp;
	int i;

	ip = bytecode;
	while(ip)
	{
		switch(*ip++)
		{
		case CONS:
			stack[top-2] = cons(stack[top-1],stack[top-2]);
			top--;
			break;
		case CAR:
			stack[top-1] = car(stack[top-1]);
			break;
		case CDR:
			stack[top-1] = cdr(stack[top-1]);
			break;
		case PUSH:
			stack[top] = *((OBJ*)ip);
			top++;
			ip += sizeof(OBJ);
			break;
		case POP:
			top--;
			break;
		case SET_CDR:
			cdr(stack[top-1]) = stack[top-2];
			stack[top - 2] = OBJ_VOID;
			top--;
			break;
		case SET_CAR:
			car(stack[top-1]) = stack[top-2];
			stack[top - 2] = OBJ_VOID;
			top--;
			break;
		case REF:
			stack[top -1] = cdr(stack[top -1]);
			break;
		case UNINIT_REF:
			if(cdr(stack[top-1]) == OBJ_VOID)
			{
				fprintf(stderr,"can't uninitialized variable %s.",obj_symbol_data(car(stack[top-1])));
				return OBJ_NULL; /* fixme:should return a runtime error */
			}
			stack[top-1] = cdr(stack[top-1]);
			break;
		case BIND:
			tmp = obj_procedure_formals(stack[top-1]);
			i = top-2;
			while(!nullp(tmp))
			{
				cdr(car(tmp)) = stack[i];
				i--;
				tmp = cdr(tmp);
			}
			stack[i+1] = stack[top-1];
			top = i+2;
			break;
		case EQ:
			if(eq(stack[top-1],stack[top-2]))
				stack[top-2] = OBJ_TRUE;
			else
				stack[top-2] = OBJ_FALSE;
			top--;
			break;
		case ADD:
			stack[top-2] = add(stack[top-1],stack[top-2]);
			top--;
			break;
		case MUL:
			stack[top-2] = mul(stack[top-1],stack[top-2]);
			top--;
			break;
		case SUB:
			stack[top-2] = sub(stack[top-1],stack[top-2]);
			top--;
			break;
			/* case DIV: */
			/* 	stack[top-2] = div(stack[top-1],stack[top-2]); */
			/* 	top--; */
			/* 	break; */
		case TYPE:
			switch(*ip)
			{
			case OBJ_BOOLEAN:
				stack[top-1] = obj_make_boolean(obj_booleanp(stack[top-1]));
				break;
			case OBJ_SYMBOL:
				stack[top-1] = obj_make_boolean(obj_symbolp(stack[top-1]));
				break;
			case OBJ_CHAR:
				stack[top-1] = obj_make_boolean(obj_charp(stack[top-1]));
				break;
			case OBJ_VECTOR:
				stack[top-1] = obj_make_boolean(obj_vectorp(stack[top-1]));
				break;
			case OBJ_PROCEDURE:
				stack[top-1] = obj_make_boolean(obj_primitivep(stack[top-1]) || obj_procedurep(stack[top-1]));
				break;
			case OBJ_PAIR:
				stack[top-1] = obj_make_boolean(obj_pairp(stack[top-1]));
				break;
			case OBJ_NUMBER:
				stack[top-1] = obj_make_boolean(obj_numberp(stack[top-1]));
				break;
			case OBJ_STRING:
 				stack[top-1] = obj_make_boolean(obj_stringp(stack[top-1]));
				break;
//			case OBJ_PORT:
			}
			ip += sizeof(OBJ);
			break;
		case JUMP:
			ip = *((char**)ip);
			break;
		case JUMP_UNLESS:
			if(stack[top-1] == OBJ_FALSE)
				ip = *((char**)ip);
			else
				ip += sizeof(char*);
			break;
		case CALL:
			stack[top] = ip;
			top++;
			stack[top] = fp;
			top++;
			fp = top-3;
			ip = obj_string_data(obj_procedure_code(stack[fp]));
			break;
		case TAIL_CALL:
			ip = obj_string_data(obj_procedure_code(stack[top-1]));
			top = fp+3;
			break;
		case RET:
			ip = (char*)(stack[fp+1]);
			stack[fp] = stack[top-1];
			top = fp+1;
			fp = (long)stack[fp+2];
			break;
		case GT:
			if(obj_numberp(stack[top-1]) && obj_numberp(stack[top-2]))
			{
				stack[top-2] = obj_make_boolean(obj_number_data(stack[top-1]) > obj_number_data(stack[top-2]));
			}
			top--;
			break;
		case FC1:
			stack[top-1] = (*((fn1_t*)ip))(stack[top-1]);
			ip += sizeof(OBJ);
			break;
		case FC2:
			stack[top-2] = (*((fn2_t*)ip))(stack[top-1],stack[top-2]);
			top--;
			ip += sizeof(OBJ);
			break;
		case DONE:
			goto out;
		default:
			printf("error instruction!\n");
			goto error;
		}
	}
error:
	printf("error instruction!\n");
	return OBJ_NULL;
out:
	return stack[top-1];
}

#ifdef VM_TEST
int main()
{

//	struct vm vm;
	char bytecode[2000];
	char *tmp;

//	vm_init(&vm);
	tmp = generate_if(bytecode);
//	ip = generate_push(bytecode,30);	
//	ip = generate_add(bytecode,4,5);
	*tmp = DONE;

	print_bytecode(bytecode);
//	vm_eval(&vm,bytecode);
	printf("result: %d",vm(bytecode));
	return 0;
}
#endif
