OCAMLMAKEFILE := OCamlMakefile

SOURCES := name.ml names.ml ast.ml parse.ml value.ml eval.ml prelude.ml sqGraphics.ml main.ml
PRE_TARGETS := pa_sqfun.cmi pa_sqfun.cmo
USE_CAMLP4 := yes
RESULT := square

OCAMLBLDFLAGS = unix.cma str.cma graphics.cma
OCAMLNLDFLAGS = unix.cmxa str.cmxa graphics.cmxa

all: native-code

include $(OCAMLMAKEFILE)
