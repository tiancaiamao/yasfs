.PHONY: all lib clean test
.SUFFIXES: .mli .mll .mly .ml .cmo .cmi .cmx
.ml.cmo:
	ocamlc -c $(OFLAGS) $(INCLUDES) $<
.ml.cmi:
	ocamlc -c $(OFLAGS) $(INCLUDES) $<
.mli.cmi:
	ocamlc -c $(OFLAGS) $(INCLUDES) $<
.mll.ml:
	ocamllex $(OFLAGS) $(INCLUDES) $<
.mly.ml:
	ocamlyacc $(OFLAGS) $(INCLUDES) $<
.mly.mli:
	ocamlyacc $(OFLAGS) $(INCLUDES) $<

all: lib

lib:zen.cma

zen.cma : ast.cmo lambda.cmo instruct.cmo lexer.cmo parser.cmo bruijn.cmo zinc.cmo driver.cmo
	ocamlc -o zen.cma -a $^

zinc.cmo : lambda.cmo ast.cmo instruct.cmo

test : zinc_test driver_test bruijn_test

zinc_test: zen.cma
	ocaml zen.cma zinc_test.ml

driver_test: zen.cma
	ocaml zen.cma driver_test.ml

bruijn_test: zen.cma
	ocaml zen.cma bruijn_test.ml

bruijn.cmo: bruijn.ml lambda.cmo ast.cmo

parser.cmo:parser.ml parser.cmi ast.cmo

lexer.cmo: parser.cmo

parser.cmi: parser.mli ast.cmo

clean:
	rm -rf parser.ml parser.mli lexer.ml *.cmo *.cmi *.cma
