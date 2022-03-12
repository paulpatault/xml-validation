BUILDER     = dune build
EXEC        = dune exec
CLEAN       = dune clean
FMT_FLAGS   = @fmt --auto-promote
EXE.builded = ./_build/default/bin/validate.exe
EXE.before  = ./bin/validate.exe
EXE.loc     = ./validate

all:
	$(BUILDER)

run:
	$(EXEC) $(EXE.before) tests/ex.dtd tests/1.xml hd

clean:
	$(CLEAN)

fmt:
	$(BUILDER) $(FMT_FLAGS)

install: all
	cp -f $(EXE.builded) $(EXE.loc)

.PHONY: all run clean fmt install
