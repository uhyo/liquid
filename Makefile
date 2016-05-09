PACKS = Z3

RESULT = z3test
SOURCES = id.ml m.ml \
	  tree.ml bType.mli bType.ml syntax.mli syntax.ml parser.mly lexer.mll \
	  bTyping.ml \
	  q.ml lType.ml lTyping.ml \
	  main.ml

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
