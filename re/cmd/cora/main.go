package main

import (
	"os"
	"fmt"
	"io"
	
	"yasfs/re"
)

func main() {
	vm := re.New()
	r := re.NewSexpReader(os.Stdin)
	for i:=0; ; i++ {
		fmt.Printf("%d #> ", i)

		sexp, err := r.Read()
		if err != nil && err != io.EOF {
			panic(err)
		}

		sexp = vm.MacroExpand(sexp)

		res := vm.Eval(sexp)

		fmt.Printf(res.String())
		fmt.Println()
	}
}
