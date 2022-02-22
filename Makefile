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
exec:
	dune exec -- src/validate.exe

.PHONY: clean
clean:
	dune clean

.PHONY: cleanall
cleanall: clean
	$(RM) $(BAZAR)
