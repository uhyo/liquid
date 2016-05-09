PACKS = Z3

RESULT = z3test
SOURCES = tree.ml syntax.mli syntax.ml parser.mly lexer.mll \
	  main.ml

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
