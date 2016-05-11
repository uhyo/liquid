PACKS = Z3
CFLAGS = -g

RESULT = z3test
SOURCES = bType.mli bType.ml id.ml m.ml s.ml MI.ml \
	  tree.ml syntax.mli syntax.ml parser.mly lexer.mll \
	  bTyping.ml \
	  constant.ml kNormal.ml lType.ml \
	  builtin.ml cons.ml \
	  prover.ml \
	  solve.ml \
	  main.ml

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
