all:
	dune build @install

test:
	dune runtest

clean:
	rm -Rf _build

.FORCE:
