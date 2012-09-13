#ifndef VM_H

#include "type.h"

enum opcode
{
	CONS,
	CAR,
	CDR,
	SET_CAR,
	SET_CDR,
	ADD,
	SUB,
	MUL,
	DIV,
	PUSH,
	POP,
	JUMP_UNLESS,
	JUMP,
	EQ,
	REF,
	UNINIT_REF,
	BIND,
	CALL,
	TAIL_CALL,
	RET,
	TYPE,
	GT,
	FC1,
	FC2,
	DONE,
};

OBJ vm(char *bytecode);
void print_bytecode(char *bytecode);
#endif
