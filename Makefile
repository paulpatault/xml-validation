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
	$(EXEC) $(EXE.before) tests/1.dtd tests/1.xml a

runv:
	$(EXEC) $(EXE.before) tests/1.dtd tests/1.xml a 1

clean:
	$(CLEAN)

cleanall: clean
	rm -rf ./validate

fmt:
	$(BUILDER) $(FMT_FLAGS)

install: all
	cp -f $(EXE.builded) $(EXE.loc)

.PHONY: all run clean fmt install
