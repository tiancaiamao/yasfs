#include "mem.h"
#include "type.h"
#include "env.h"
#include <stdlib.h>
#include <stdio.h>

static int needgc = 0;
void gc_needgc()
{
	needgc = 1;
}
static void mark(OBJ obj)
{
	unsigned i;
	if(obj_markedp(obj))
		return;
	if(obj_pointerp(obj))
		obj_mark(obj);
        if(obj_pairp(obj))
	{
		mark(car(obj));
		mark(cdr(obj));
	}
	else if(obj_vectorp(obj))
	{
		for(i=0; i<obj_vector_length(obj); i++)
		{
			mark(obj_vector_ref(obj,i));
		}
	}
	else if(obj_definep(obj))
	{
		mark(obj_define_cell(obj));
		mark(obj_define_ast(obj));
	}
}

static void sweep()
{
	struct chunk *ck;
	struct freenode *tmp;
	OBJ base;
	int begin = 0;

	for(ck=mem_get_chunk(); ck!=NULL; ck=ck->next)
	{
		ck->freelist = NULL;
		base = (OBJ)align((unsigned long)&ck->data);
		while((char*)base < (char*)ck+4096)
		{
			if(obj_markedp(base))
			{
				if(begin == 1)
				{
					begin = 0;
					tmp->size = (char*)base - (char*)tmp;
					tmp->next = ck->freelist;
					ck->freelist = tmp;
				}				
				obj_unmark(base);
				switch(base->tag)
				{
				case OBJ_FLONUM:
					base = (OBJ)align((unsigned long)((char*)base+sizeof(double)));
					break;
				case OBJ_PAIR:
					base = (OBJ)align((unsigned long)((char*)base+obj_sizeof(pair)));
				case OBJ_CORE:
					base = (OBJ)align((unsigned long)((char*)base+obj_sizeof(core)));
					break;
				case OBJ_AST_DEFINE:
					base = (OBJ)align((unsigned long)((char*)base+obj_sizeof(define)));
					break;
				case OBJ_VECTOR:
					base = (OBJ)align((unsigned long)((char*)base + offsetof(struct object,value) + sizeof(unsigned long) + obj_vector_length(base)*sizeof(OBJ)));
					break;
				case OBJ_STRING:
					base = (OBJ)align((unsigned long)((char*)base+offsetof(struct object,value) + sizeof(unsigned) + obj_string_length(base) + 1));
					break;
				case OBJ_SYMBOL:
					base = (OBJ)align((unsigned long)((char*)base+offsetof(struct object,value) + sizeof(unsigned) + obj_symbol_length(base) + 1));
					break;			
				default:
					printf("gc sweep error unknown type!\n");
				}
			}
			else
			{
				if(begin  == 0)
				{
					begin = 1;
					tmp = (struct freenode*)base;
				}
				base++;
			}
		}
	}
}

void gc(OBJ env)
{
	if(needgc)
	{
		needgc = 0;
		mark(env);
		sweep();
	}
}


#ifdef GC_TEST

int main()
{
	OBJ a,b,c;

	a = obj_make_flonum(10.34);

	b = obj_make_integer(33);
	c = cons(a,b);

	return 0;
}
#endif
