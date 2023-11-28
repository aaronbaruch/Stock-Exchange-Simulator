.PHONY: test 

build: 
	OCAMLRUNPARAM=b dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

clean: bisect-clean
	dune clean

trade: 
	OCAMLRUNPARAM=b dune exec bin/main.exe

