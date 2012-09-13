#include "mem.h"
#include "gc.h"
#include <stdio.h>
#include <stdlib.h>

static struct chunk *mem = NULL;

unsigned long align(unsigned long p)
{
	unsigned align = sizeof(void*);
	if((p | (align -1)) != 0)
	{
		p = (p & ~(align-1)) + align;
	}
	return p;
}

void mem_exit_hook()
{
	struct chunk *ck;
	while(mem != NULL)
	{
		ck = mem->next;
	        free(mem);
		mem = ck;
	}
}

struct chunk* mem_get_chunk()
{
	return mem;
}

static int mem_alloc_chunk()
{
	struct chunk *ck;
	ck = malloc(4096);
	if(ck == NULL)
		return -1;
	ck->next = mem;
	mem = ck;
	ck->freelist = (struct freenode*)align((unsigned long)&ck->data);
	ck->freelist->next = NULL;
	ck->freelist->size = (char*)ck + 4096 - (char*)ck->freelist;
	return 0;
}

static void* mem_try_alloc(unsigned int size)
{
	struct chunk *ck;
	struct freenode *p,*prev,*tmp;

	size = align(size);
	for(ck = mem; ck!=NULL; ck=ck->next)
	{

		p = ck->freelist;
		if(p == NULL)
			continue;
		if(p->next == NULL)
		{
			if(p->size >= size)
			{			
				tmp = (struct freenode*)((char*)p + size);
				tmp->next = NULL;
				tmp->size = p->size - size;
				ck->freelist = tmp;			
				return p;
			}
			else
				continue;
		}
		prev = p;
		p = p->next;
		while(p)
		{
			if(p->size >= size)
			{
				tmp = (struct freenode*)((char*)p + size);
				tmp->next = p->next;
				tmp->size = p->size - size;
				prev->next = tmp;
				return p;
			}
			prev = p;
			p = p->next;
		}
	}
	return NULL;
}

void* mem_alloc(unsigned int size)
{
	void *ret;
	ret = mem_try_alloc(size);
	if(mem_alloc_chunk() != 0)
	{
		fprintf(stderr,"out of memory");
		return NULL;
	}
	gc_needgc();
	return mem_try_alloc(size);
}

#ifdef MEM_TEST

int main()
{
	atexit(mem_exit_hook);
	printf("test align: 13-> %d",align(13));
	mem_alloc(13);
	return 0;
}
#endif
