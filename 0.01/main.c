#include <stdio.h>
#include <stdlib.h>

enum object_type
{
	OBJ_PAIR = 0,
	OBJ_NUMBER,
	OBJ_STRING,
	OBJ_ENV,
	OBJ_TYPE_MAX
};

struct object_head
{
	enum object_type type;
	char marked;
};

typedef struct object_head* object_t;

struct pair
{
	struct object_head head;
	object_t car;
	object_t cdr;
};
struct string
{
	struct object_head head;
	unsigned int size;
	char data[1];
};
struct env
{
	struct object_head head;
	struct env *parent;
	struct pair *binding;
};

struct object_head scheme_null_object;
struct object_head scheme_false_object;

object_t scheme_null = &scheme_null_object;
object_t scheme_false = &scheme_false_object;

struct typeinfo
{
	char *name;
	unsigned int size;
	object_t (*generator)();
};

struct typeinfo typeinfo_table[OBJ_TYPE_MAX];

#define type_name(o) (typeinfo_table[o->type].name)
#define type_size(o) (typeinfo_table[o->type].size)

struct env* make_env(struct env *p,struct pair *binding)
{
	struct env* ret = malloc(sizeof(struct env));
	ret->head.type = OBJ_ENV;
	ret->parent = p;
	ret->binding = binding;
	return ret;
}

struct pair* make_pair(object_t car,object_t cdr)
{
	struct pair* ret = malloc(sizeof(struct env));
	ret->head.type = OBJ_PAIR;
	ret->car = car;
	ret->cdr = cdr;
	return ret;
}

void init()
{
	typeinfo_table[OBJ_PAIR].name = "pair";
	typeinfo_table[OBJ_PAIR].size = sizeof(struct pair);
	typeinfo_table[OBJ_PAIR].generator = NULL;

	typeinfo_table[OBJ_ENV].name = "environment";
	typeinfo_table[OBJ_ENV].size = sizeof(struct env);
	typeinfo_table[OBJ_ENV].generator = NULL;

}

int main()
{
	object_t o;

	init();
	o = make_env(scheme_null,scheme_null);
	printf("object->type: %s\n",type_name(o));
	printf("object->type: %d\n",type_size(o));
	return 0;
}
