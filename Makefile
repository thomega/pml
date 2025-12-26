all: dune

dune:
	@dune build

run:
	@dune exec -- bin/pml_cli.exe disc

test tests runtest check:
	@dune runtest

install:
	@dune install

promote:
	@dune promote

top:
	@utop -init pml.ocamlinit

docs:
	@dune build @doc

clean:
	find -name '*~' | xargs rm -f
	dune clean

realclean: clean
