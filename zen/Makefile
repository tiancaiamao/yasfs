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

OBJS = ast.cmo global.cmo lambda.cmo type.cmo instruct.cmo lexer.cmo parser.cmo bruijn.cmo zinc.cmo infer.cmo driver.cmo

DEPEND += lexer.ml parser.ml parser.mli

all: .depend lib

include .depend

lib: zen.cma

zen.cma : $(DEPEND) $(OBJS)
	ocamlc -o zen.cma -a $(OBJS)

test : zinc_test driver_test bruijn_test infer_test

infer_test: zen.cma
	ocaml zen.cma infer_test.ml

zinc_test: zen.cma
	ocaml zen.cma zinc_test.ml

driver_test: zen.cma
	ocaml zen.cma driver_test.ml

bruijn_test: zen.cma
	ocaml zen.cma bruijn_test.ml

clean:
	rm -rf parser.ml parser.mli lexer.ml *.cmo *.cmi *.cma

.depend: $(DEPEND)
	ocamldep $(INCLUDE) *.mli *.ml > .depend
