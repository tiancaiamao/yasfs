.PHONY: all lib clean
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

lib : ast.cmo lambda.cmo instruct.cmo lexer.cmo parser.cmo bruijn.cmo zinc.cmo driver.cmo
	ocamlc -o zen.cma -a $^

bruijn.cmo: bruijn.ml lambda.cmo

parser.cmo:parser.ml parser.cmi ast.cmo

lexer.cmo: parser.cmo

parser.cmi: parser.mli ast.cmo

clean:
	rm -rf parser.ml parser.mli lexer.ml *.cmo *.cmi *.cma
