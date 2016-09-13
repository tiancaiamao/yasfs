parser: parser.cmo lexer.cmo

parser.cmo:parser.ml parser.cmi ast.cmo
	ocamlc -c parser.ml

ast.cmo : ast.ml
	ocamlc -c ast.ml

lexer.cmo:lexer.ml
	ocamlc -c lexer.ml

lexer.ml:lexer.mll
	ocamllex lexer.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

parser.cmi: parser.mli ast.cmo
	ocamlc -c parser.mli


clean:
	rm -rf parser.ml parser.mli lexer.ml *.cmo *.cmi
