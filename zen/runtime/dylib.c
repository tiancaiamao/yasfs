#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct HandleItem {
  char *name;
  void *handle;
};

// Handle is used for manage opened dylib handler.
struct Handle {
  struct HandleItem* table;
  int cap;
  int count;
};

static void*
handle_get_dylib(struct Handle* h, char *name) {
  for (int i=0; i<h->count; i++) {
    struct HandleItem *item = &h->table[i];
    if (strcmp(name, item->name) == 0) {
      return item->handle;
    }
  }
  return NULL;
}

static void
handle_append_item(struct Handle *h, char *name, void *handle) {
  if (h->count == h->cap) {
    int new_cap = h->cap * 2;
    void *tmp = malloc(sizeof(struct HandleItem) * new_cap);
    memcpy(tmp, h->table, h->cap);
    h->cap = new_cap;
    free(h->table); // don't forget it!
    h->table = tmp;
  }
  char *buf = malloc(strlen(name)+1);
  strcpy(buf, name); // should make a copy!
  h->table[h->count].name = buf;
  h->table[h->count].handle = handle;
  h->count++;
}

static void*
open_dylib(char *name) {
  // TODO consider PATH LD_PATH
  char full_path[200];
  sprintf(full_path, "runtime/%s.dylib", name);
  return dlopen(full_path, RTLD_GLOBAL | RTLD_NOW);
}

void*
handle_get(struct Handle *h, char *dylib_name, char *prim_name) {
  void *handle = handle_get_dylib(h, dylib_name);
  if (handle == NULL) {
    void *handle = open_dylib(dylib_name);
    if (handle == NULL) {
      return NULL;
    }
    handle_append_item(h, dylib_name, prim_name);
  }

  return dlsym(handle, prim_name);
}

struct Handle *
handle_create() {
  struct Handle *h = (struct Handle*)malloc(sizeof(struct Handle));
  h->cap = 16;
  h->count = 0;
  h->table = malloc(sizeof(struct HandleItem) * h->cap);
  return h;
}

void
handle_destroy(struct Handle *h) {
  for (int i=0; i<h->count; i++) {
    struct HandleItem *item = &h->table[i];
    free(item->name);
    dlclose(item->handle);
    item->handle = NULL;
  }
  free(h->table);
  free(h);
}
