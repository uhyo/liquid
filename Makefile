PACKS = Z3

RESULT = z3test
SOURCES = bType.mli bType.ml id.ml m.ml \
	  tree.ml syntax.mli syntax.ml parser.mly lexer.mll \
	  bTyping.ml \
	  q.ml lType.ml template.ml builtin.ml cons.ml \
	  lTyping.ml \
	  kNormal.ml \
	  main.ml

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
