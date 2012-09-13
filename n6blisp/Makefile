.PHONY: all clean

CC = gcc
CFLAGS = -Wall -g

SRCS = main.c mem.c type.c gc.c env.c lex.c eval.c vm.c compile.c
OBJS = $(SRCS:.c=.o)

%.o:%.c
	$(CC) -c $(CFLAGS) $< -o $@

.depend:$(SRCS)
	$(CC) -MM $(CFLAGS) $(SRCS) > .depend

all:.depend $(OBJS)
	$(CC) $(OBJS) -o a.out

clean:
	rm -f a.out $(OBJS) .depend
-include .depend
test:
	$(CC) -DENV_TEST  -g mem.c gc.c type.c env.c lex.c -o test