.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe
	
play:
	OCAMLRUNPARAM=b dune exec gui/interface.exe


doc:
	dune build @doc
clean:
	dune clean