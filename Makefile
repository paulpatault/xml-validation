BUILD      = dune build
CLEAN      = dune clean
RM         = rm -rf
OCAMLFIND  = ocamlfind
OCAMLC     = ocamlopt
# OCAMLFLAGS = -O3 -linkpkg -package graphics

BAZAR =

all:
	dune build

test0:
	dune exec -- ./src/validate.exe tests/ex.typ tests/1.xml x

clean:
	dune clean

cleanall: clean
	$(RM) $(BAZAR)

.PHONY: all exec clean cleanall
