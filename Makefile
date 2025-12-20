all: dune

dune:
	dune build

test tests runtest check:
	dune runtest

install:
	dune install

promote:
	dune promote

top:
	utop -init pml.ocamlinit

doc docs:
	dune build @doc

clean:
	find -name '*~' | xargs rm -f
	dune clean

realclean: clean
