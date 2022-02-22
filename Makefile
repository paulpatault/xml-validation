BUILD      = dune build
CLEAN      = dune clean
RM         = rm -rf
OCAMLFIND  = ocamlfind
OCAMLC     = ocamlopt
# OCAMLFLAGS = -O3 -linkpkg -package graphics

BAZAR =

.PHONY: all
all:
	dune build

.PHONY: exec
test0:
	dune exec -- ./src/validate.exe tests/ex.typ tests/1.xml t

.PHONY: clean
clean:
	dune clean

.PHONY: cleanall
cleanall: clean
	$(RM) $(BAZAR)
