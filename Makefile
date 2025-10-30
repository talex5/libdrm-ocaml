.PHONY: all test

all:
	dune build @all @check

test:
	dune exec -- ./examples/query.exe
