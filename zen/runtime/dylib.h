#ifndef _DYLIB_H
#define _DYLIB_H

struct Handle;

struct Handle* handle_create();
void handle_destroy(struct Handle *h);
void* handle_get(struct Handle* h, char *dylib, char *prim);

#endif
