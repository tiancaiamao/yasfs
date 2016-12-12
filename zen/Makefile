.PHONY: all vm clean test

all: vm

vm:
	gcc -g -I runtime -o vm main.c runtime/dylib.c runtime/util.c runtime/value.c runtime/vm.c

test : vm
	go run main.go

clean:
	rm -rf vm vm.dSYM test/*.out test/*.bc
