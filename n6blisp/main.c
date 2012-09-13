#include "type.h"
#include "gc.h"
#include "env.h"
#include "lex.h"
#include "eval.h"
#include <stdio.h>

int main(void) 
{
	OBJ env;
	OBJ data;
	OBJ result;

	printf("welcome to n6blisp~\n>");
	env = env_init();
	while(1)
	{
		data = lex_read(stdin);
		if(data == OBJ_EOF)
			break;
		result = eval(data,env);
		obj_write(stdout,result);
		printf("\n>");
	}
    
	printf("Goodbye\n");

	return 0;
}
