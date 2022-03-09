all:
	dune build

run:
	dune exec validate tests/ex.typ tests/1.xml x

clean:
	dune clean

fmt:
	dune build @fmt --auto-promote
