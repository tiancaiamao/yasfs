#include "type.h"
#include "gc.h"
#include "lex.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

static char is_delimiter(int c) 
{
	return isspace(c) || c == EOF ||
		c == '('   || c == ')' ||
		c == '"'   || c == ';';
}

static char is_identifier(int c) 
{
	return isalnum(c) || c == '!' || c == '$' || c == '%' ||
		c == '&' || c == '*' || c == '+' || c == '-' ||
		c == '.' || c == '/' || c == ':' || c == '<' ||
		c == '=' || c == '>' || c == '?' || c == '@' ||
		c == '^' || c == '_' || c == '~';
}

static int peek(FILE *in) {
	int c;

	c = getc(in);
	ungetc(c, in);
	return c;
}

static void eat_whitespace(FILE *in) 
{
	int c;
    
	while ((c = getc(in)) != EOF) 
	{
		if (isspace(c)) 
		{
			continue;
		}
		else if (c == ';') /* comments are whitespace also */
		{ 
			while (((c = getc(in)) != EOF) && (c != '\n'));
			continue;
		}
		ungetc(c, in);
		break;
	}
}

static void eat_expected_string(FILE *in, char *str) {
	int c;

	while (*str != '\0') {
		c = getc(in);
		if (c != *str) {
			fprintf(stderr, "unexpected character '%c'\n", c);
			exit(1);
		}
		str++;
	}
}

void peek_expected_delimiter(FILE *in) {
	if (!is_delimiter(peek(in))) {
		fprintf(stderr, "character not followed by delimiter\n");
		exit(1);
	}
}

static OBJ read_character(FILE *in) 
{
	int c;

	c = getc(in);
	switch (c) 
	{
        case EOF:
		fprintf(stderr, "incomplete character literal\n");
		exit(1);
        case 's':
		if (peek(in) == 'p') 
		{
			eat_expected_string(in, "pace");
			peek_expected_delimiter(in);
			return obj_make_character(' ');
		}
		break;
        case 'n':
		if (peek(in) == 'e') 
		{
			eat_expected_string(in, "ewline");
			peek_expected_delimiter(in);
			return obj_make_character('\n');
		}
		break;
	}
	peek_expected_delimiter(in);
	return obj_make_character(c);
}

static OBJ read_pair(FILE *in) {
	int c;
	OBJ car_obj;
	OBJ cdr_obj;
	OBJ ret;
    
	eat_whitespace(in);
    
	c = getc(in);
	if (c == ')') 
	{ /* read the empty list */
		return OBJ_NULL;
	}
	ungetc(c, in);

	car_obj = lex_read(in);
	

	eat_whitespace(in);
    
	c = getc(in);    
	if (c == '.') { /* read improper list */
		c = peek(in);
		if (!is_delimiter(c)) 
		{
			fprintf(stderr, "dot not followed by delimiter\n");
			exit(1);
		}
		cdr_obj = lex_read(in);
		
		eat_whitespace(in);
		c = getc(in);
		if (c != ')') {
			fprintf(stderr,
				"where was the trailing right paren?\n");
			exit(1);
		}
		ret = cons(car_obj,cdr_obj);
		
		
		return ret;
	}
	else 
	{ /* read list */
		ungetc(c, in);
		cdr_obj = read_pair(in);
		ret = cons(car_obj,cdr_obj);
		return ret;
	}
}

static OBJ read_number(FILE *in)
{
	char c;
	char buf[30];
	int i=0;
	int type;		/* 0-flonum; 1-double; 2-error */

	memset(buf,0,30);
	type = 0;
	c = getc(in);
	while(!is_delimiter(c))
	{
		if(c == '.')
		{
			if(type == 0)
				type = 1;
			else
				goto error;
		}
		else if(!isdigit(c))
			goto error;
		buf[i] = c;
		i++;
		c = getc(in);
	}
	ungetc(c,in);
	if(type == 0)
		return obj_make_integer(atoi(buf));
	else if(type == 1)
		return obj_make_flonum(atof(buf));
error:
	fprintf(stderr,"wrong form of number\n");
	exit(-1); 
}

OBJ lex_read(FILE *in) 
{
	int c;
	int i;
	char buffer[1000];
	OBJ tmp;

	eat_whitespace(in);
	c = getc(in);    
	if (c == '#')		/* read a boolean or character */
	{ 
		c = getc(in);
		switch (c) 
		{
		case 't':
			return OBJ_TRUE;
		case 'f':
			return OBJ_FALSE;
		case '\\':
			return read_character(in);
		case '(':	/* fixme!!! */
			fprintf(stderr,"vector not implemented yet\n");
			exit(1);
		case 'e':
		case 'i':
		case 'b':
		case 'o':
		case 'd':
		case 'x':
			fprintf(stderr,"binary,hex and so on not implemented yet\n");
		exit(1);
		case 'u':
			if(peek(in) == 8)      
				fprintf(stderr,"bytevector not implemented yet\n");
			exit(1);
		default:
			fprintf(stderr,
				"unknown lex error after '#'\n");
			exit(1);
		}
	}
	else if (isdigit(c))
	{
		ungetc(c,in);
		return read_number(in);
	}
	else if((c=='+' || c=='-') && isdigit(peek(in)))
	{
		if(c == '-')
		{
			tmp = read_number(in);
			if(obj_integerp(tmp))
				tmp = obj_make_integer(0-obj_unbox_integer(tmp));
			else if(obj_flonump(tmp))
				tmp = obj_make_flonum(0-obj_flonum_value(tmp));
		}
		return tmp;
	}
	else if (is_identifier(c)) /* read a identifier */
	{
		i = 0;
		while (is_identifier(c)) 
		{
			/* subtract 1 to save space for '\0' terminator */
			if (i < 1000 - 1) 
				buffer[i++] = c;
			else 
			{
				fprintf(stderr, "symbol too long. Maximum length is %d\n", 1000);
				exit(1);
			}
			c = getc(in);
		}
		if (is_delimiter(c)) 
		{
			buffer[i] = '\0';
			ungetc(c, in);
			return obj_make_symbol(buffer);
		}
		else 
		{
			fprintf(stderr, "symbol not followed by delimiter. "
				"Found '%c'\n", c);
			exit(1);
		}
	}
	else if (c == '"')	/* read a string */
	{ 
		i = 0;
		while ((c = getc(in)) != '"') 
		{
			if (c == '\\') {
				c = getc(in);
				if (c == 'n') {
					c = '\n';
				}
			}
			if (c == EOF) 
			{
				fprintf(stderr, "non-terminated string literal\n");
				exit(1);
			}
			/* subtract 1 to save space for '\0' terminator */
			if (i < 1000 - 1) 
			{
				buffer[i++] = c;
			}
			else 
			{
				fprintf(stderr, 
					"string too long. Maximum length is %d\n",
					1000);
				exit(1);
			}
		}
		buffer[i] = '\0';
		return obj_make_string_1(buffer);
	}
	else if (c == '(')	/* read the empty list or pair */
		return read_pair(in);
	else if (c == '\'')	/* read quoted expression */
	{ 
		tmp = cons(lex_read(in),OBJ_NULL);
		return cons(obj_make_symbol("quote"),tmp);
	}
	else if (c == EOF)
		return OBJ_EOF;
	else 
	{
		fprintf(stderr, "bad input. Unexpected '%c'\n", c);
		exit(1);
	}
	fprintf(stderr, "read illegal state\n");
	exit(1);
}

static void write_pair(FILE *out, OBJ pair) 
{
	OBJ car_obj;
	OBJ cdr_obj;
    
	car_obj = car(pair);
	cdr_obj = cdr(pair);
	obj_write(out, car_obj);
	if (obj_pairp(cdr_obj)) 
	{
		fprintf(out, " ");
		write_pair(out, cdr_obj);
	}
	else if (nullp(cdr_obj)) 
	{
		return;
	}
	else {
		fprintf(out, " . ");
		obj_write(out, cdr_obj);
	}
}

void obj_write(FILE *out, OBJ obj) 
{
	char *str;
	if(obj_fixnump(obj))
		fprintf(out,"%ld",obj_unbox_integer(obj));
	else if(obj_booleanp(obj))
		fprintf(out,"#%c",(obj == OBJ_TRUE)? 't':'f');
	else if(obj_charp(obj))
		fprintf(out,"#\\%c",obj_unbox_character(obj));
	else if(nullp(obj))
		fprintf(out,"()");
	else if(voidp(obj))
		fprintf(out,"undefined");
	else
	{
		switch(obj_pointer_tag(obj))
		{
		case OBJ_PAIR:
			fprintf(out, "(");
			write_pair(out, obj);
			fprintf(out, ")");
			break;
		case OBJ_STRING:
			str = obj_string_data(obj);
			putchar('"');
			while (*str != '\0') {
				switch (*str) {
				case '\n':
					fprintf(out, "\\n");
					break;
				case '\\':
					fprintf(out, "\\\\");
					break;
				case '"':
					fprintf(out, "\\\"");
					break;
				default:
					putc(*str, out);
				}
				str++;
			}
			putchar('"');
			break;
		case OBJ_SYMBOL:
			fprintf(out, "%s", obj_symbol_data(obj));
			break;
		case OBJ_PRIMITIVE:
		case OBJ_PROCEDURE:
			fprintf(out, "<procedure>");
			break;
		case OBJ_CORE:
		case OBJ_SYNTAX:
			fprintf(out, "<syntax>");
			break;
		default:
			fprintf(stderr, "cannot write unknown type\n");
		}
	}    
}

#ifdef LEX_TEST

int main()
{
	OBJ obj;
	quote_symbol = obj_make_symbol("quote");
	gc_init();
	while(1)
	{
	printf(">\n");
	obj = lex_read(stdin);
	obj_write(stdout,obj);
	}
	return 0;
}
#endif
