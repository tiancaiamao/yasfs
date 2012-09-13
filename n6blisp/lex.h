#ifndef LEX_H
#include "type.h"
#include <stdio.h>

OBJ lex_read(FILE *in); 
void obj_write(FILE *out,OBJ obj);

#endif
