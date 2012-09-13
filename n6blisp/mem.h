#ifndef MEM_H

struct freenode
{
	struct freenode *next;
	unsigned int size;
};

struct chunk
{
	struct chunk *next;
	struct freenode *freelist;
	char data[];
};

void* mem_alloc(unsigned int size);
unsigned long align(unsigned long p);
void mem_exit_hook();
struct chunk* mem_get_chunk();
#endif
