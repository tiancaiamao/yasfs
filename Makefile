a.out:vm.c vm.h
	gcc -g $< -o $@

clean:
	rm -rf a.out