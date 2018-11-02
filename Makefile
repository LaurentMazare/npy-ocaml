ALL = bin/list_npz.exe \

all:
	dune build $(ALL)
	dune build @install

test:
	dune runtest

clean:
	rm -Rf _build

.FORCE:
