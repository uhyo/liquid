PACKS = Z3
CFLAGS = -g

RESULT = z3test
SOURCES = bType.mli bType.ml id.ml m.ml \
	  tree.ml syntax.mli syntax.ml parser.mly lexer.mll \
	  bTyping.ml \
	  constant.ml kNormal.ml lType.ml \
	  builtin.ml cons.ml \
	  main.ml

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
