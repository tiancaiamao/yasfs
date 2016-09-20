all: lib

%.cmo : %.ml
	ocamlc -c $@ $<

lib : ast.cmo lexer.cmo parser.cmo bruijn.cmo lambda.cmo instruct.cmo zinc.cmo driver.cmo
	ocamlc -o zen.cma -a $^

bruijn.cmo: bruijn.ml lambda.cmo

parser.cmo:parser.ml parser.cmi ast.cmo
	ocamlc -c parser.ml

ast.cmo : ast.ml
	ocamlc -c ast.ml

lexer.cmo:lexer.ml parser.cmo
	ocamlc -c lexer.ml

lexer.ml:lexer.mll
	ocamllex lexer.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

parser.cmi: parser.mli ast.cmo
	ocamlc -c parser.mli

zinc.cmo: zinc.ml
	ocamlc -c zinc.ml

clean:
	rm -rf parser.ml parser.mli lexer.ml *.cmo *.cmi *.cma
